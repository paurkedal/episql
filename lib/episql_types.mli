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
  | Lit_integer of int64
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
  | `References of qname * string option
  | `On_delete of [`Cascade | `Restrict]
  | `On_update of [`Cascade | `Restrict] ]

type table_scope = [`Permanent | `Permanent_unlogged | `Temporary]

type table_constraint =
  [ `Check of expression * [`No_inherit] list
  | `Unique of string list
  | `Primary_key of string list
  | `Foreign_key of string list * qname * string list ]

type table_item =
  | Column of string * datatype * column_constraint list
  | Constraint of table_constraint

type table = {
  table_qname : qname;
  table_scope : table_scope;
  table_if_not_exists : bool;
  table_items : table_item list;
}

type sequence_scope = [`Permanent | `Temporary]

type sequence_attr =
  [ `Start of int64 | `Increment of int64
  | `Minvalue of int64 | `Maxvalue of int64
  | `Cache of int64 | `Cycle | `No_cycle | `Owner of qname ]

type sequence = {
  sequence_qname : qname;
  sequence_scope : sequence_scope;
  sequence_attrs : sequence_attr list;
}

type drop_option = [`If_exists | `Cascade | `Restrict]

type statement =
  | Create_schema of string
  | Create_sequence of sequence
  | Create_table of table
  | Create_enum of qname * string list
  | Drop_schema of string list * drop_option list
  | Drop_table of qname list * drop_option list
  | Drop_sequence of qname list * drop_option list
  | Drop_type of qname list * drop_option list
