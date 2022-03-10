(* Copyright (C) 2021--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
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

val create : unit -> t

val where : t -> string -> 'a Caqti_type.t -> [< 'a predicate] -> unit

val finish :
  table_name: string ->
  select_list: string list ->
  select_type: 'a Caqti_type.t ->
  order_column_name: ('b -> string) ->
  order_by: 'b order_item list ->
  ?limit: int ->
  ?offset: int ->
  t -> 'a request
