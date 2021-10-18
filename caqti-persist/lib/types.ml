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
