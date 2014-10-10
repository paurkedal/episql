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

type 'a presence =
  | Absent
  | Inserting of 'a Lwt_condition.t
  | Present of 'a
  | Deleting of unit Lwt_condition.t

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let cache_hertz = Int64.to_float ExtUnixSpecific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let fetch_grade = 1e-3 *. cache_second

module type PK_CACHABLE = sig
  type pk
  type nonpk
  val fetch : pk -> nonpk option Lwt.t
end

module type PK_CACHED = sig
  type pk
  type nonpk
  type beacon
  type t = {pk : pk; mutable nonpk : nonpk presence; beacon : beacon}
  val find : pk -> t option
  val make : pk -> t Lwt.t
  val merge : pk -> nonpk option -> t
end

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) = struct

  type t = {
    pk : P.pk;
    mutable nonpk : P.nonpk presence;
    beacon : Beacon.t;
  }

  module W = Weak.Make (struct
    type tmp = t type t = tmp
    let equal a b = a.pk = b.pk
    let hash a = Hashtbl.hash a.pk
  end)

  let cache = W.create 17

  let find pk =
    try Some (W.find cache {pk; nonpk = Absent; beacon = Beacon.dummy})
    with Not_found -> None

  let make pk =
    try
      Lwt.return (W.find cache {pk; nonpk = Absent; beacon = Beacon.dummy})
    with Not_found ->
      P.fetch pk >|= fun nonpk ->
      let nonpk = match nonpk with None -> Absent | Some x -> Present x in
      Beacon.embed fetch_grade (fun beacon -> W.merge cache {pk; nonpk; beacon})

  let merge pk nonpk =
    let nonpk = match nonpk with None -> Absent | Some x -> Present x in
    W.merge cache (Beacon.embed fetch_grade (fun beacon -> {pk; nonpk; beacon}))
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
    Oneshot (fun _ -> Buffer.contents b.buf), Array.of_list (List.rev b.params)
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
      Buffer.add_string ib.buf " RETURNING (";
      Buffer.add_string ib.buf r;
      List.iter (fun r -> Buffer.add_string ib.buf ", ";
			  Buffer.add_string ib.buf r) rs;
      Buffer.add_char ib.buf ')'
    end;
    (Oneshot (fun _ -> Buffer.contents ib.buf),
     Array.of_list (List.rev ib.params))
end
