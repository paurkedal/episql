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

val or_fail :
  ('a, [< Caqti_error.t | `Conflict of conflict_error]) result -> 'a Lwt.t

type 'a order_predicate =
  [ `Eq of 'a | `Ne of 'a | `Lt of 'a | `Le of 'a | `Ge of 'a | `Gt of 'a
  | `Between of 'a * 'a | `Not_between of 'a * 'a ]

type 'a order_item =
  | Asc of 'a
  | Desc of 'a
  | Asc_sql of string
  | Desc_sql of string

val default_select_grade : int -> float
val select_grade : (int -> float) ref

module type PK_CACHABLE = sig
  type key
  type state
  type value
  type change
  module Result_lwt : sig
    type (+'a, +'e) t
    val return_ok : 'a -> ('a, 'e) t
    val conflict :
      conflict_error -> ('a, [> `Conflict of conflict_error]) t
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

module Make_pk_cache :
  functor (Beacon : Prime_beacon.S) ->
  functor (P : PK_CACHABLE) ->
  PK_CACHED
    with type key := P.key and type state := P.state
     and type value := P.value and type change := P.change
     and type (+'a, +'e) result_lwt := ('a, 'e) P.Result_lwt.t
     and type beacon := Beacon.t

type (_, _) request =
  Request : ('a, 'b, 'm) Caqti_request.t * 'a -> ('b, 'm) request

module Insert : sig

  type wd = Wd
  type wod = Wod

  module Spec : sig
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

  module Request : sig
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

    val create : 'q Spec.t -> ('q, unit, unit) t
  end

  type (_, _) app =
    | App : {
        request: ('q, 'p, 'r) Request.t;
        param: 'p;
        default: 'r -> 'd;
      } -> ('q, 'd) app

  val init : ('q, unit, unit) Request.t -> ('q, unit) app
  val ($) : (('a * _) * 'q, 'd) app -> 'a -> ('q, 'd) app
  val ($?) : (('a * wd) * 'q, 'd) app -> 'a option -> ('q, 'd * 'a) app
end

module Update_buffer : sig
  type t
  val create : Caqti_driver_info.t -> string -> t
  val set : t -> string -> 'a Caqti_type.t * 'a -> unit
  val where : t -> string -> 'a Caqti_type.t * 'a -> unit
  val contents : t -> (unit, Caqti_mult.zero) request option
end

module Select_buffer : sig
  type t

  type query_fragment =
    | S : string -> query_fragment
    | P : 'a Caqti_type.t * 'a -> query_fragment

  val create : Caqti_driver_info.t -> string -> t
  val ret : t -> string -> unit
  val where : t -> query_fragment list -> unit
  val obsolete_order_by : t -> string -> unit
  val order_by : t -> ('a -> string) -> 'a order_item -> unit
  val limit : t -> int -> unit
  val offset : t -> int -> unit
  val contents : t -> 'a Caqti_type.t -> ('a, Caqti_mult.zero_or_more) request
end
