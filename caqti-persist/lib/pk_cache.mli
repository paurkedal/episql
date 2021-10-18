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

val default_select_grade : int -> float
val select_grade : (int -> float) ref

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
    key: key;
    mutable state: state presence;
    beacon: beacon;
    patches: (value, change) persist_patch_out React.event;
    notify: ?step: React.step -> (value, change) persist_patch_out -> unit;
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

module Make :
  functor (Beacon : Prime_beacon.S) ->
  functor (P : CACHABLE) ->
  S with type key := P.key and type state := P.state
     and type value := P.value and type change := P.change
     and type (+'a, +'e) result_lwt := ('a, 'e) P.Result_lwt.t
     and type beacon := Beacon.t