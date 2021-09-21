(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf
open Unprime

type 'a presence =
  | Absent
  | Inserting of 'a Lwt_condition.t
  | Present of 'a
  | Deleting of unit Lwt_condition.t

type ('value_r, 'value_d, 'change) persist_patch_in =
  [ `Insert of 'value_r * 'value_d
  | `Update of 'change list
  | `Delete ]

type ('value, 'change) persist_patch_out =
  [ `Insert of 'value
  | `Update of 'change list
  | `Delete ]

type conflict_error = {
  conflict_type: [`Insert_insert | `Update_insert | `Update_delete];
  conflict_table: string;
} [@@deriving show]

exception Not_present
exception Conflict of conflict_error

let or_fail = function
 | Ok x -> Lwt.return x
 | Error #Caqti_error.t as r -> Caqti_lwt.or_fail r
 | Error (`Conflict err) -> Lwt.fail (Conflict err)

type 'a order_predicate =
  [ `Eq of 'a | `Ne of 'a | `Lt of 'a | `Le of 'a | `Ge of 'a | `Gt of 'a
  | `Between of 'a * 'a | `Not_between of 'a * 'a ]

type 'a order_item =
  | Asc of 'a
  | Desc of 'a
  | Asc_sql of string
  | Desc_sql of string

let cache_hertz = Int64.to_float ExtUnixSpecific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let query_overhead = ref (1e-3 *. cache_second)
let transfer_grade = ref (1e-8 *. cache_second)

let default_select_grade n = !query_overhead /. float_of_int n
                          +. !transfer_grade
let select_grade = ref default_select_grade

module type PK_CACHABLE = sig
  type key
  type state
  type value
  type change
  module Result_lwt : sig
    type (+'a, +'e) t
    val return_ok : 'a -> ('a, 'e) t
    val conflict : conflict_error -> ('a, [> `Conflict of conflict_error]) t
    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val bind_lwt : ('a -> ('b, 'e) t) -> 'a Lwt.t -> ('b, 'e) t
  end
  val key_size : int
  val state_size : int
  val fetch :
    Caqti_lwt.connection ->
    key -> (state option, [> Caqti_error.t]) Result_lwt.t
  val table_name : string
end

module type PK_CACHED = sig
  type key
  type state
  type value
  type change
  type beacon
  type t = {
    key : key;
    mutable state : state presence;
    beacon : beacon;
    patches : (value, change) persist_patch_out React.event;
    notify : ?step: React.step -> (value, change) persist_patch_out -> unit;
  }
  type (+'a, +'e) result_lwt
  val find : key -> t option
  val fetch :
    Caqti_lwt.connection ->
    key -> (t, [> Caqti_error.t]) result_lwt
  val merge : key * state presence -> t
  val merge_created :
    key * state -> (t, [> `Conflict of conflict_error]) result_lwt
  val uncache_key : key -> unit
  val uncache : t -> unit
  val uncache_all : unit -> unit
end

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) = struct

  type patch_out = (P.value, P.change) persist_patch_out

  type t = {
    key : P.key;
    mutable state : P.state presence;
    beacon : Beacon.t;
    patches : patch_out React.event;
    notify : ?step: React.step -> patch_out -> unit;
  }

  module W = Weak.Make (struct
    type nonrec t = t
    let equal a b = a.key = b.key
    let hash a = Hashtbl.hash a.key
  end)

  let cache = W.create 17
  let fetch_grade = !select_grade (P.key_size + P.state_size + 8)

  let uncache_all () = W.clear cache

  let mk_key =
    let notify ?step:_ _patch = assert false in
    let patches = React.E.never in
    fun key -> {key; state = Absent; beacon = Beacon.dummy; patches; notify}

  let uncache_key key = W.remove cache (mk_key key)
  let uncache obj = W.remove cache obj

  let find key =
    try Some (W.find cache (mk_key key))
    with Not_found -> None

  let merge (key, state) =
    let patches, notify = React.E.create () in
    Beacon.embed fetch_grade
      (fun beacon -> W.merge cache {key; state; beacon; patches; notify})

  let fetch c key =
    try
      P.Result_lwt.return_ok (W.find cache (mk_key key))
    with Not_found ->
      let aux state =
        let state = match state with None -> Absent | Some x -> Present x in
        merge (key, state)
      in
      P.fetch c key |> P.Result_lwt.map aux

  let merge_created (key, state) =
    try
      let o = W.find cache (mk_key key) in
      let rec retry () =
        (match o.state with
         | Deleting c ->
            P.Result_lwt.bind_lwt retry (Lwt_condition.wait c)
         | Absent ->
            o.state <- Present state;
            P.Result_lwt.return_ok ()
         | Inserting _ | Present _ ->
            let error = {
              conflict_type = `Insert_insert;
              conflict_table = P.table_name;
            } in
            P.Result_lwt.conflict error)
      in
      P.Result_lwt.map (fun () -> o) (retry ())
    with Not_found ->
      let o =
        let patches, notify = React.E.create () in
        Beacon.embed fetch_grade begin fun beacon ->
          {key; state = Present state; beacon; patches; notify}
        end
      in
      W.add cache o;
      P.Result_lwt.return_ok o
end

module Params = struct
  type t = V : 'a Caqti_type.t * 'a -> t
  let empty = V (Caqti_type.unit, ())
  let add pt pv (V (pt', pv')) = V (Caqti_type.(tup2 pt' pt), (pv', pv))
end

type (_, _) request =
  Request : ('a, 'b, 'm) Caqti_request.t * 'a -> ('b, 'm) request

module Insert = struct

  type wd = Wd
  type wod = Wod

  module Spec = struct
    type 'q t =
      | Done : string -> unit t
      | Field : {
          cn: string;
          ct: 'a Caqti_type.t;
          next: 'q t;
        } -> (('a * wod) * 'q) t
      | Field_default : {
          cn: string;
          ct: 'a Caqti_type.t;
          next: 'q t;
        } -> (('a * wd) * 'q) t
  end

  module Request = struct
    type (_, _, _) t =
      | Done :
          ('p, unit, Caqti_mult.zero) Caqti_request.t -> (unit, 'p, unit) t
      | Done_default :
          ('p, 'a * 'r, Caqti_mult.one) Caqti_request.t -> (unit, 'p, 'a * 'r) t
      | Field : {
          set: ('q, 'p * 'a, 'r) t;
        } -> (('a * wod) * 'q, 'p, 'r) t
      | Field_default : {
          set: ('q, 'p * 'a, 'r) t Lazy.t;
          ret: ('q, 'p, 'r * 'a) t Lazy.t;
        } -> (('a * wd) * 'q, 'p, 'r) t

    let rec create'
      : type q pt rt. (pt Caqti_type.t * _ * rt Caqti_type.t * _) -> q Spec.t ->
        (q, pt, rt) t =
      fun (pt, pcns, rt, rcns) ->
      (function
       | Spec.Done tn ->
          let pcns = List.rev pcns in
          let rcns = List.rev rcns in
          let qs =
            "INSERT INTO " ^ tn ^
            (if pcns = [] then " DEFAULT VALUES" else
             " (" ^ String.concat ", " pcns ^ ") VALUES (" ^
             (String.concat ", " (List.map (konst "?") pcns)) ^ ")")
          in
          (match rt with
           | Caqti_type.Unit ->
              Done (Caqti_request.exec pt qs)
           | Caqti_type.Tup2 (_, _) ->
              let qs = qs ^ " RETURNING " ^ String.concat ", " rcns in
              Done_default (Caqti_request.find pt rt qs)
           | _ -> assert false)
       | Spec.Field {cn; ct; next} ->
          let set =
            (create' (Caqti_type.(tup2 pt ct), cn :: pcns, rt, rcns) next) in
          Field {set}
       | Spec.Field_default {cn; ct; next} ->
          let set = lazy
            (create' (Caqti_type.(tup2 pt ct), cn :: pcns, rt, rcns) next) in
          let ret = lazy
            (create' (pt, pcns, Caqti_type.(tup2 rt ct), cn :: rcns) next) in
          Field_default {set; ret})

    let create spec = create' (Caqti_type.unit, [], Caqti_type.unit, []) spec
  end

  type (_, _) app =
    | App : {
        request: ('q, 'p, 'r) Request.t;
        param: 'p;
        default: 'r -> 'd;
      } -> ('q, 'd) app

  let init request = App {request; param = (); default = ident}

  let ($) : type a ad q d. ((a * ad) * q, d) app -> a -> (q, d) app =
    fun request arg ->
    (match request with
     | App {request = Request.Field {set; _}; param; default} ->
        App {request = set; param = (param, arg); default}
     | App {request = Request.Field_default {set; _}; param; default} ->
        App {request = Lazy.force set; param = (param, arg); default})

  let ($?) : type a q d. ((a * wd) * q, d) app -> a option -> (q, d * a) app =
    fun request arg ->
    (match request with
     | App {request = Request.Field_default {set; ret; _}; param; default} ->
        (match arg with
         | Some arg ->
            let default rs = (default rs, arg) in
            App {request = Lazy.force set; param = (param, arg); default}
         | None ->
            let default (rs, r) = (default rs, r) in
            App {request = Lazy.force ret; param; default}))
end

module Update_buffer = struct

  type state = Init | Set | Where | Noop

  type t = {
    buf : Buffer.t;
    mutable params : Params.t;
    mutable state : state;
  }

  let create _driver_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "UPDATE ";
    Buffer.add_string buf table_name;
    Buffer.add_string buf " SET ";
    {buf; params = Params.empty; state = Init}

  let assign ub pn (pt, pv) =
    Buffer.add_string ub.buf pn;
    Buffer.add_string ub.buf " = ?";
    ub.params <- Params.add pt pv ub.params

  let set ub pn pv =
    begin match ub.state with
    | Init -> ub.state <- Set
    | Set -> Buffer.add_string ub.buf ", "
    | _ -> assert false
    end;
    assign ub pn pv

  let where ub pn pv =
    begin match ub.state with
    | Init | Noop -> ub.state <- Noop
    | Set -> Buffer.add_string ub.buf " WHERE "; ub.state <- Where
    | Where -> Buffer.add_string ub.buf " AND "
    end;
    assign ub pn pv

  let contents ub =
    let qs = konst (Buffer.contents ub.buf) in
    (match ub.state with
     | Init | Noop -> None
     | Set -> assert false (* Precaution, we don't need unconditional update. *)
     | Where ->
        let Params.V (pt, pv) = ub.params in
        let rt, m = Caqti_type.unit, Caqti_mult.zero in
        Some (Request (Caqti_request.create_p ~oneshot:true pt rt m qs, pv)))
end

module Select_buffer = struct

  type query_fragment =
    | S : string -> query_fragment
    | P : 'a Caqti_type.t * 'a -> query_fragment

  type state = Init | Ret | Where | Order_by | Final

  type t = {
    buf : Buffer.t;
    table_name : string;
    mutable params : Params.t;
    mutable state : state;
  }

  let create _driver_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "SELECT ";
    {buf; table_name; params = Params.empty; state = Init}

  let ret sb pn =
    match sb.state with
    | Init -> sb.state <- Ret; Buffer.add_string sb.buf pn
    | Ret -> Buffer.add_string sb.buf ", "; Buffer.add_string sb.buf pn
    | Where | Order_by | Final -> assert false

  let emit_from sb =
    Buffer.add_string sb.buf " FROM ";
    Buffer.add_string sb.buf sb.table_name

  let where sb qfs =
    begin match sb.state with
    | Init | Order_by | Final -> assert false
    | Ret ->
      emit_from sb;
      Buffer.add_string sb.buf " WHERE ";
      sb.state <- Where
    | Where ->
      Buffer.add_string sb.buf " AND "
    end;
    qfs |> List.iter begin function
     | S s -> Buffer.add_string sb.buf s
     | P (pt, pv) ->
        Buffer.add_string sb.buf "?";
        sb.params <- Params.add pt pv sb.params
    end

  let obsolete_order_by sb cn =
    match sb.state with
    | Init | Final -> assert false
    | Ret | Where ->
      if sb.state = Ret then emit_from sb;
      bprintf sb.buf " ORDER BY %s" cn;
      sb.state <- Order_by
    | Order_by ->
      bprintf sb.buf ", %s" cn

  let order_by sb f order_item =
    (match sb.state with
     | Init | Final -> assert false
     | Ret | Where ->
        if sb.state = Ret then emit_from sb;
        sb.state <- Order_by;
        Buffer.add_string sb.buf " ORDER BY "
     | Order_by ->
        Buffer.add_string sb.buf ", ");
    (match order_item with
     | Asc col -> bprintf sb.buf "\"%s\"" (f col)
     | Desc col -> bprintf sb.buf "\"%s\" DESC" (f col)
     | Asc_sql sql -> Buffer.add_string sb.buf sql
     | Desc_sql sql -> bprintf sb.buf "%s DESC" sql)

  let limit sb n =
    (match sb.state with
     | Init -> assert false
     | Ret -> emit_from sb
     | Where | Order_by | Final -> ());
    bprintf sb.buf " LIMIT %d" n;
    sb.state <- Final

  let offset sb n =
    (match sb.state with
     | Init -> assert false
     | Ret -> emit_from sb
     | Where | Order_by | Final -> ());
    bprintf sb.buf " OFFSET %d" n;
    sb.state <- Final

  let contents sb rt =
    (match sb.state with
     | Init -> assert false
     | Ret ->
        Buffer.add_string sb.buf " FROM ";
        Buffer.add_string sb.buf sb.table_name
     | Where | Order_by | Final -> ());
    let qs = Buffer.contents sb.buf in
    let Params.V (pt, pv) = sb.params in
    let m = Caqti_mult.zero_or_more in
    Request (Caqti_request.create_p ~oneshot:true pt rt m (konst qs), pv)

end

let () =
  Printexc.register_printer
    (function
     | Conflict error -> Some (show_conflict_error error)
     | _ -> None)
