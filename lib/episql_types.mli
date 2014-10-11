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
  [ `Boolean
  | `Real
  | `Double_precision
  | `Smallint
  | `Integer
  | `Bigint
  | `Smallserial
  | `Serial
  | `Bigserial
  | `Text
  | `Bytea
  | `Char of int
  | `Varchar of int
  | `Numeric of int * int
  | `Decimal of int * int
  | `Time
  | `Date
  | `Timestamp
  | `Timestamp_with_timezone
  | `Interval
  | `Custom of qname ]

type serialtype = [ `Smallserial | `Serial | `Bigserial ]

type literal =
  | Lit_integer of int
  | Lit_text of string

type expression =
  | Expr_qname of qname
  | Expr_literal of literal
  | Expr_app of qname * expression list

type column_constraint =
  [ `Not_null
  | `Null
  | `Unique
  | `Primary_key
  | `Default of expression
  | `References of qname * string option ]

type table_constraint =
  [ `Check of expression * [`No_inherit] list
  | `Unique of string list
  | `Primary_key of string list
  | `Foreign_key of string list * qname * string list ]

type table_item =
  | Column of string * datatype * column_constraint list
  | Constraint of table_constraint

type sequence_attr =
  [ `Temporary | `Increment of int
  | `Minvalue of int | `Maxvalue of int | `Start of int (* FIXME: int64 *)
  | `Cache of int | `Cycle | `No_cycle | `Owner of qname ]

type drop_option = [`If_exists | `Cascade | `Restrict]

type statement =
  | Create_schema of string
  | Create_sequence of qname * bool * sequence_attr list
  | Create_table of qname * table_item list
  | Create_enum of qname * string list
  | Drop_schema of string list * drop_option list
  | Drop_table of qname list * drop_option list
  | Drop_sequence of qname list * drop_option list
  | Drop_type of qname list * drop_option list
