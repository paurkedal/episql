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

type ('value_r, 'value_d, 'change) persist_patch_in =
  [ `Insert of 'value_r * 'value_d
  | `Update of 'change list
  | `Delete ]

type ('value, 'change) persist_patch_out =
  [ `Insert of 'value
  | `Update of 'change list
  | `Delete ]

exception Merge_conflict

val default_select_grade : int -> float
val select_grade : (int -> float) ref

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

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) :
	PK_CACHED with type key := P.key and type state := P.state
		   and type value := P.value and type change := P.change
		   and type beacon := Beacon.t

module Insert_buffer (C : Caqti_lwt.CONNECTION) : sig
  type t
  val create : Caqti_metadata.backend_info -> string -> t
  val set : t -> string -> C.param -> unit
  val ret : t -> string -> unit
  val have_ret : t -> bool
  val contents : t -> Caqti_query.query * C.param array
end

module Update_buffer (C : Caqti_lwt.CONNECTION) : sig
  type t
  val create : Caqti_metadata.backend_info -> string -> t
  val set : t -> string -> C.param -> unit
  val where : t -> string -> C.param -> unit
  val contents : t -> (Caqti_query.query * C.param array) option
end

module Select_buffer (C : Caqti_lwt.CONNECTION) : sig
  type t
  type query_fragment = S of string | P of C.param
  val create : Caqti_metadata.backend_info -> string -> t
  val ret : t -> string -> unit
  val where : t -> query_fragment list -> unit
  val contents : t -> Caqti_query.query * C.param array
end
