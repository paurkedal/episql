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

open Types

let cache_hertz = Int64.to_float ExtUnix.Specific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let query_overhead = ref (1e-3 *. cache_second)
let transfer_grade = ref (1e-8 *. cache_second)

let default_select_grade n = !query_overhead /. float_of_int n
                          +. !transfer_grade
let select_grade = ref default_select_grade

type 'a presence =
  | Absent
  | Inserting of 'a Lwt_condition.t
  | Present of 'a
  | Deleting of unit Lwt_condition.t

module type CACHABLE = sig
  type key
  type state
  type value
  type change
  module Result_lwt : sig
    type (+'a, +'e) t
    val return_ok : 'a -> ('a, 'e) t
    val conflict : Error.conflict -> ('a, [> `Conflict of Error.conflict]) t
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

module type S = sig
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
    key * state -> (t, [> `Conflict of Error.conflict]) result_lwt
  val uncache_key : key -> unit
  val uncache : t -> unit
  val uncache_all : unit -> unit
end

module Make (Beacon : Prime_beacon.S) (P : CACHABLE) = struct

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
            let error = {Error.
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

let () =
  Printexc.register_printer
    (function
     | Error.Conflict error -> Some (Error.show_conflict error)
     | _ -> None)
