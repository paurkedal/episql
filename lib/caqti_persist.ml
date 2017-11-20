(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Printf

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

exception Not_present
exception Conflict of [ `Insert_insert | `Update_insert | `Update_delete ]

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
  val key_size : int
  val state_size : int
  val fetch : key -> state option Lwt.t
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
  val find : key -> t option
  val fetch : key -> t Lwt.t
  val merge : key * state presence -> t
  val merge_created : key * state -> t Lwt.t
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
    type tmp = t type t = tmp
    let equal a b = a.key = b.key
    let hash a = Hashtbl.hash a.key
  end)

  let cache = W.create 17
  let fetch_grade = !select_grade (P.key_size + P.state_size + 8)

  let mk_key =
    let notify ?step:_ _patch = assert false in
    let patches = React.E.never in
    fun key -> {key; state = Absent; beacon = Beacon.dummy; patches; notify}

  let find key =
    try Some (W.find cache (mk_key key))
    with Not_found -> None

  let merge (key, state) =
    let patches, notify = React.E.create () in
    Beacon.embed fetch_grade
      (fun beacon -> W.merge cache {key; state; beacon; patches; notify})

  let fetch key =
    try
      Lwt.return (W.find cache (mk_key key))
    with Not_found ->
      P.fetch key >|= fun state ->
      let state = match state with None -> Absent | Some x -> Present x in
      merge (key, state)

  let merge_created (key, state) =
    try
      let o = W.find cache (mk_key key) in
      begin match o.state with
      | Deleting c -> Lwt_condition.wait c >|= fun () ->
                      o.state <- Present state
      | Absent -> o.state <- Present state; Lwt.return_unit
      | Inserting _ | Present _ -> Lwt.fail (Conflict `Insert_insert)
      end >|= fun () -> o
    with Not_found ->
      let o =
        let patches, notify = React.E.create () in
        Beacon.embed fetch_grade
          (fun beacon -> {key; state=Present state; beacon; patches; notify}) in
      W.add cache o;
      Lwt.return o

end

module Insert_buffer (C : Caqti_lwt.CONNECTION) = struct
  open Caqti_query

  type t = {
    driver_info : Caqti_driver_info.t;
    buf : Buffer.t;
    mutable param_count : int;
    mutable params : C.Param.t list;
    mutable returning : string list;
  }

  let create driver_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "INSERT INTO ";
    Buffer.add_string buf table_name;
    {driver_info; buf; param_count = 0; params = []; returning = []}

  let set ib pn pv =
    Buffer.add_string ib.buf (if ib.param_count = 0 then " (" else ", ");
    Buffer.add_string ib.buf pn;
    ib.params <- pv :: ib.params;
    ib.param_count <- ib.param_count + 1

  let ret ib r = ib.returning <- r :: ib.returning

  let have_ret ib = ib.returning <> []

  let contents ib =
    if ib.param_count = 0 then
      Buffer.add_string ib.buf " DEFAULT VALUES"
    else begin
      Buffer.add_string ib.buf ") VALUES (";
      (match Caqti_driver_info.parameter_style ib.driver_info with
       | `Linear s ->
          Buffer.add_string ib.buf s;
          for _ = 1 to ib.param_count - 1 do
            Buffer.add_string ib.buf ", ";
            Buffer.add_string ib.buf s
          done
       | `Indexed sf ->
          Buffer.add_string ib.buf (sf 0);
          for i = 1 to ib.param_count - 1 do
            Buffer.add_string ib.buf ", ";
            Buffer.add_string ib.buf (sf i)
          done
       | _ -> raise Missing_query_string);
      Buffer.add_char ib.buf ')'
    end;
    (match List.rev ib.returning with
     | [] -> ()
     | r :: rs ->
        Buffer.add_string ib.buf " RETURNING ";
        Buffer.add_string ib.buf r;
        List.iter (fun r -> Buffer.add_string ib.buf ", ";
                            Buffer.add_string ib.buf r) rs);
    let qs = Buffer.contents ib.buf in
    (Oneshot (fun _ -> qs), Array.of_list (List.rev ib.params))
end

let make_param_for driver_info =
  (match Caqti_driver_info.parameter_style driver_info with
   | `Linear s -> fun _ -> s
   | `Indexed sf -> sf
   | _ -> raise Caqti_query.Missing_query_string)

module Update_buffer (C : Caqti_lwt.CONNECTION) = struct

  type state = Init | Set | Where | Noop

  type t = {
    make_param : int -> string;
    buf : Buffer.t;
    mutable param_count : int;
    mutable params : C.Param.t list;
    mutable state : state;
  }

  let create driver_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "UPDATE ";
    Buffer.add_string buf table_name;
    Buffer.add_string buf " SET ";
    { make_param = make_param_for driver_info; buf;
      param_count = 0; params = []; state = Init }

  let assign ub pn pv =
    Buffer.add_string ub.buf pn;
    Buffer.add_string ub.buf " = ";
    Buffer.add_string ub.buf (ub.make_param ub.param_count);
    ub.params <- pv :: ub.params;
    ub.param_count <- ub.param_count + 1

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
    let qs = Buffer.contents ub.buf in
    match ub.state with
    | Init | Noop -> None
    | Set -> assert false (* Precaution, we don't need unconditional update. *)
    | Where -> Some (Caqti_query.Oneshot (fun _ -> qs),
                     Array.of_list (List.rev ub.params))
end

module Select_buffer (C : Caqti_lwt.CONNECTION) = struct

  type query_fragment = S of string | P of C.Param.t

  type state = Init | Ret | Where | Order_by | Final

  type t = {
    make_param : int -> string;
    buf : Buffer.t;
    table_name : string;
    mutable params : C.Param.t list;
    mutable param_count : int;
    mutable state : state;
  }

  let create driver_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "SELECT ";
    { make_param = make_param_for driver_info; buf; table_name;
      params = []; param_count = 0; state = Init; }

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
    List.iter
      (function
        | S s -> Buffer.add_string sb.buf s
        | P pv ->
          Buffer.add_string sb.buf (sb.make_param sb.param_count);
          sb.params <- pv :: sb.params;
          sb.param_count <- sb.param_count + 1)
      qfs

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

  let contents sb =
    begin match sb.state with
    | Init -> assert false
    | Ret ->
      Buffer.add_string sb.buf " FROM ";
      Buffer.add_string sb.buf sb.table_name
    | Where | Order_by | Final -> ()
    end;
    let qs = Buffer.contents sb.buf in
    Caqti_query.Oneshot (fun _ -> qs), Array.of_list (List.rev sb.params)

end
