(* Copyright (C) 2021  Petter A. Urkedal <paurkedal@gmail.com>
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

type t

type 'a predicate = [
  | `Is_null | `Is_not_null
  | 'a order_predicate
  | `Like of 'a
  | `Ilike of 'a
]

type _ request =
  Request :
    ('a, 'b, Caqti_mult.zero_or_more) Caqti_request.t * 'a -> 'b request

type query_fragment =
  | S : string -> query_fragment
  | P : 'a Caqti_type.t * 'a -> query_fragment

val create : Caqti_driver_info.t -> string -> t
val ret : t -> string -> unit
val where : t -> query_fragment list -> unit
val where_field : t -> string -> 'a Caqti_type.t -> [< 'a predicate] -> unit
val order_by : t -> ('a -> string) -> 'a order_item -> unit
val limit : t -> int -> unit
val offset : t -> int -> unit
val contents : t -> 'a Caqti_type.t -> 'a request
