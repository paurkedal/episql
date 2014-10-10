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

module type PK_CACHABLE = sig
  type pk
  type nonpk
  val fetch : pk -> nonpk option Lwt.t
end

module type PK_CACHED = sig
  type pk
  type nonpk
  type beacon
  type t = {
    pk : pk;
    mutable nonpk : nonpk presence;
    beacon : beacon;
  }
  val find : pk -> t option
  val make : pk -> t Lwt.t
  val merge : pk -> nonpk option -> t
end

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) :
	PK_CACHED with type pk := P.pk and type nonpk := P.nonpk
		   and type beacon := Beacon.t

module Query_buffer (C : Caqti_lwt.CONNECTION) : sig
  type t
  val create : Caqti_metadata.backend_info -> t
  val add_string : t -> string -> unit
  val add_param : t -> C.param -> unit
  val supress_comma : t -> unit
  val add_comma : t -> unit
  val contents : t -> Caqti_query.query * C.param array
end

module Insert_buffer (C : Caqti_lwt.CONNECTION) : sig
  type t
  val create : Caqti_metadata.backend_info -> string -> t
  val set : t -> string -> C.param -> unit
  val ret : t -> string -> unit
  val have_ret : t -> bool
  val contents : t -> Caqti_query.query * C.param array
end
