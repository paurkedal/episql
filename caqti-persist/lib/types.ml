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

type 'a order_predicate = [
  | `Eq of 'a | `Ne of 'a | `Lt of 'a | `Le of 'a | `Ge of 'a | `Gt of 'a
  | `Between of 'a * 'a | `Not_between of 'a * 'a
]

type 'a order_item =
  | Asc of 'a
  | Desc of 'a
  | Asc_sql of string
  | Desc_sql of string

type ('value_r, 'value_d, 'change) persist_patch_in = [
  | `Insert of 'value_r * 'value_d
  | `Update of 'change list
  | `Delete
]

type ('value, 'change) persist_patch_out = [
  | `Insert of 'value
  | `Update of 'change list
  | `Delete
]

module type CONNECTION_SPEC = sig

  val rename_schema : string option -> string option
  (** Given a schema [ns] found in the SQL table definitions, [rename_schema ns]
      shall return the schema used to quelify the same tables in actual database
      requests.  [None] means that the table name is unqualified. *)

  val use_db :
    ((module Caqti_lwt.CONNECTION) -> ('a, 'e) result Lwt.t) ->
    ('a, [> Caqti_error.connect] as 'e) result Lwt.t
  (** [use_db f] shall call [f] with a database connection. *)

  module Beacon : Prime_beacon.S
  (** Caching specifications. *)

end
