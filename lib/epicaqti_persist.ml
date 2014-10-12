(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

type empty = Invalid_empty_value

type 'a presence =
  | Absent
  | Inserting of 'a Lwt_condition.t
  | Present of 'a
  | Deleting of unit Lwt_condition.t

type ('a, 'b) patch = Insert of 'a * 'b list | Update of 'b list | Delete

exception Merge_conflict

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let cache_hertz = Int64.to_float ExtUnixSpecific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let fetch_grade = 1e-3 *. cache_second

module type PK_CACHABLE = sig
  type pk
  type nonpk
  type req
  type change
  val fetch : pk -> nonpk option Lwt.t
end

module type PK_CACHED = sig
  type pk
  type nonpk
  type req
  type change
  type beacon
  type t = {
    pk : pk;
    mutable nonpk : nonpk presence;
    beacon : beacon;
    patches : (req, change) patch React.event;
    notify : ?step: React.step -> (req, change) patch -> unit;
  }
  val find : pk -> t option
  val make : pk -> t Lwt.t
  val merge_present : pk * nonpk -> t Lwt.t
end

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) = struct

  type t = {
    pk : P.pk;
    mutable nonpk : P.nonpk presence;
    beacon : Beacon.t;
    patches : (P.req, P.change) patch React.event;
    notify : ?step: React.step -> (P.req, P.change) patch -> unit;
  }

  module W = Weak.Make (struct
    type tmp = t type t = tmp
    let equal a b = a.pk = b.pk
    let hash a = Hashtbl.hash a.pk
  end)

  let cache = W.create 17

  let mk_key =
    let notify ?step patch = assert false in
    let patches = React.E.never in
    fun pk -> {pk; nonpk = Absent; beacon = Beacon.dummy; patches; notify}

  let find pk =
    try Some (W.find cache (mk_key pk))
    with Not_found -> None

  let make pk =
    try
      Lwt.return (W.find cache (mk_key pk))
    with Not_found ->
      P.fetch pk >|= fun nonpk ->
      let nonpk = match nonpk with None -> Absent | Some x -> Present x in
      let patches, notify = React.E.create () in
      Beacon.embed fetch_grade
	(fun beacon -> W.merge cache {pk; nonpk; beacon; patches; notify})

  let merge_present (pk, nonpk) =
    try
      let o = W.find cache (mk_key pk) in
      begin match o.nonpk with
      | Deleting c -> Lwt_condition.wait c >|= fun () ->
		      o.nonpk <- Present nonpk
      | Absent -> o.nonpk <- Present nonpk; Lwt.return_unit
      | Inserting _ | Present _ -> Lwt.fail Merge_conflict
      end >|= fun () -> o
    with Not_found ->
      let o =
	let patches, notify = React.E.create () in
	Beacon.embed fetch_grade
	  (fun beacon -> {pk; nonpk=Present nonpk; beacon; patches; notify}) in
      W.add cache o;
      Lwt.return o
end

module Query_buffer (C : Caqti_lwt.CONNECTION) = struct
  open Caqti_metadata
  open Caqti_query

  type t = {
    backend_info : backend_info;
    buf : Buffer.t;
    mutable param_no : int;
    mutable params : C.param list;
    mutable comma_supressed : bool;
  }
  let create backend_info =
    { backend_info;
      buf = Buffer.create 256;
      param_no = 1; params = [];
      comma_supressed = false }
  let add_string {buf} s = Buffer.add_string buf s
  let add_param b p =
    b.params <- p :: b.params;
    match b.backend_info.bi_parameter_style with
    | `Linear s -> Buffer.add_string b.buf s
    | `Indexed sf -> Buffer.add_string b.buf (sf b.param_no);
		     b.param_no <- b.param_no + 1
    | _ -> raise Missing_query_string
  let supress_comma b = b.comma_supressed <- true
  let add_comma b =
    if b.comma_supressed = true
    then b.comma_supressed <- false
    else Buffer.add_string b.buf ", "
  let contents b =
    let qs = Buffer.contents b.buf in
    Oneshot (fun _ -> qs), Array.of_list (List.rev b.params)
end

module Insert_buffer (C : Caqti_lwt.CONNECTION) = struct
  open Caqti_metadata
  open Caqti_query

  type t = {
    backend_info : backend_info;
    buf : Buffer.t;
    mutable param_count : int;
    mutable params : C.param list;
    mutable returning : string list;
  }

  let create backend_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "INSERT INTO ";
    Buffer.add_string buf table_name;
    {backend_info; buf; param_count = 0; params = []; returning = []}

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
      begin match ib.backend_info.bi_parameter_style with
      | `Linear s ->
	Buffer.add_string ib.buf s;
	for i = 1 to ib.param_count - 1 do
	  Buffer.add_string ib.buf ", ";
	  Buffer.add_string ib.buf s
	done
      | `Indexed sf ->
	Buffer.add_string ib.buf (sf 0);
	for i = 1 to ib.param_count - 1 do
	  Buffer.add_string ib.buf ", ";
	  Buffer.add_string ib.buf (sf i)
	done
      | _ -> raise Missing_query_string
      end;
      Buffer.add_char ib.buf ')'
    end;
    begin match List.rev ib.returning with
    | [] -> ()
    | r :: rs ->
      Buffer.add_string ib.buf " RETURNING ";
      Buffer.add_string ib.buf r;
      List.iter (fun r -> Buffer.add_string ib.buf ", ";
			  Buffer.add_string ib.buf r) rs
    end;
    let qs = Buffer.contents ib.buf in
    (Oneshot (fun _ -> qs), Array.of_list (List.rev ib.params))
end

module Update_buffer (C : Caqti_lwt.CONNECTION) = struct
  open Caqti_metadata
  open Caqti_query

  type state = Init | Set | Where | Noop

  type t = {
    make_param : int -> string;
    buf : Buffer.t;
    mutable param_count : int;
    mutable params : C.param list;
    mutable state : state;
  }

  let create backend_info table_name =
    let buf = Buffer.create 256 in
    Buffer.add_string buf "UPDATE ";
    Buffer.add_string buf table_name;
    Buffer.add_string buf " SET ";
    let make_param =
      match backend_info.bi_parameter_style with
      | `Linear s -> fun _ -> s
      | `Indexed sf -> sf
      | _ -> raise Missing_query_string in
    {make_param; buf; param_count = 0; params = []; state = Init}

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
    | Where -> Some (Oneshot (fun _ -> qs), Array.of_list (List.rev ub.params))
end
