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

open Episql_types
open Printf

let parse_file = Episql_lexer.parse_file

let string_of_qname = function
  | (None, name) -> name
  | (Some ns, name) -> ns ^ "." ^ name

let sql_quote s =
  let buf = Buffer.create (String.length s * 4 / 3) in
  Buffer.add_char buf '\'';
  String.iter
    (function (* CHECKME *)
      | '\'' -> Buffer.add_string buf "''"
      | ch -> Buffer.add_char buf ch) s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let string_of_datatype : datatype -> string = function
  | `Boolean -> "boolean"
  | `Real -> "real"
  | `Double_precision -> "double precision"
  | `Smallint -> "smallint"
  | `Integer -> "integer"
  | `Bigint -> "bigint"
  | `Smallserial -> "smallserial"
  | `Serial -> "serial"
  | `Bigserial -> "bigserial"
  | `Text -> "text"
  | `Bytea -> "bytea"
  | `Char n -> sprintf "char(%d)" n
  | `Varchar n -> sprintf "varchar(%d)" n
  | `Numeric (p, d) -> sprintf "numeric(%d, %d)" p d
  | `Decimal (p, d) -> sprintf "decimal(%d, %d)" p d
  | `Time -> "time"
  | `Date -> "date"
  | `Timestamp -> "timestamp"
  | `Timestamp_with_timezone -> "timestamp with timezone"
  | `Interval -> "interval"
  | `Custom qn -> string_of_qname qn

let string_of_literal = function
  | Lit_integer i -> string_of_int i
  | Lit_text s -> sql_quote s

let string_of_column_constraint = function
  | `Not_null -> "NOT NULL"
  | `Null -> "NULL"
  | `Unique -> "UNIQUE"
  | `Primary_key -> "PRIMARY KEY"
  | `Default lit -> "DEFAULT(" ^ string_of_literal lit ^ ")"
  | `References (tqn, None) -> "REFERENCES " ^ string_of_qname tqn
  | `References (tqn, Some cn) -> "REFERENCES ("^(string_of_qname tqn)^")"^cn

type generator = statement list -> out_channel -> unit
let generators : (string, generator) Hashtbl.t = Hashtbl.create 11
let generate gn = Hashtbl.find generators gn
let register_generator gn g = Hashtbl.add generators gn g
