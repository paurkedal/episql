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

type qname = string option * string

type datatype =
  [ `Smallint
  | `Integer
  | `Bigint
  | `Smallserial
  | `Serial
  | `Bigserial
  | `Text
  | `Char of int
  | `Varchar of int
  | `Numeric of int * int
  | `Decimal of int * int
  | `Custom of qname ]

type serialtype = [ `Smallserial | `Serial | `Bigserial ]

type literal =
  | Lit_integer of int
  | Lit_text of string

type column_constraint =
  [ `Not_null
  | `Null
  | `Unique
  | `Primary_key
  | `Default of literal
  | `References of qname * qname option ]

type table_constraint =
  [ `Unique of string list ]

type table_item =
  | Column of string * datatype * column_constraint list
  | Constraint of table_constraint

type statement =
  | Create_schema of string
  | Create_table of qname * table_item list
  | Create_enum of qname * string list
