(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module Types = Types
module Lexer = Lexer

open Types
open Printf

type dialect_tag = [`Sql | `Mysql | `Pgsql | `Sqlite]

let parse_file = Lexer.parse_file

let with_buffer f = let buf = Buffer.create 128 in f buf; Buffer.contents buf

let bprint_qname buf = function
  | (None, name) -> Buffer.add_string buf name
  | (Some ns, name) -> Buffer.add_string buf ns; Buffer.add_char buf '.';
                       Buffer.add_string buf name

let pp_qname =
  Fmt.(using fst (option (string ++ const string ".")) ++ using snd string)

let string_of_qname = function
  | (None, name) -> name
  | (Some ns, name) -> ns ^ "." ^ name

let qname_of_string qn =
  (match String.split_on_char '.' qn with
   | [name] -> (None, name)
   | [ns; name] -> (Some ns, name)
   | _ -> failwith "Episql.qname_of_string")

let sql_quote s =
  let buf = Buffer.create (String.length s * 4 / 3) in
  Buffer.add_char buf '\'';
  String.iter
    (function (* CHECKME *)
      | '\'' -> Buffer.add_string buf "''"
      | ch -> Buffer.add_char buf ch) s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let string_of_interval_fields = function
  | `Default -> ""
  | `Year -> "year"
  | `Month -> "month"
  | `Day -> "day"
  | `Hour -> "hour"
  | `Minute -> "minute"
  | `Second -> "second"
  | `Year_to_month -> "year to month"
  | `Day_to_hour -> "day to hour"
  | `Day_to_minute -> "day to minute"
  | `Day_to_second -> "day to second"
  | `Hour_to_minute -> "hour to minute"
  | `Hour_to_second -> "hour to second"
  | `Minute_to_second -> "minute to second"

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
  | `Numeric_auto -> sprintf "numeric"
  | `Numeric (p, 0) -> sprintf "numeric(%d)" p
  | `Numeric (p, d) -> sprintf "numeric(%d, %d)" p d
  | `Time (None, false) -> "time"
  | `Time (None, true) -> "time with timezone"
  | `Time (Some n, false) -> sprintf "time(%d)" n
  | `Time (Some n, true) -> sprintf "time(%d) with timezone" n
  | `Date -> "date"
  | `Timestamp (None, false) -> "timestamp"
  | `Timestamp (None, true) -> "timestamp with timezone"
  | `Timestamp (Some n, false) -> sprintf "timestamp(%d)" n
  | `Timestamp (Some n, true) -> sprintf "timestamp(%d) with timezone" n
  | `Interval (`Default, None) -> "interval"
  | `Interval (`Default, Some p) -> sprintf "interval(%d)" p
  | `Interval (ivlf, None) -> "interval " ^ string_of_interval_fields ivlf
  | `Interval (ivlf, Some p) ->
    sprintf "interval %s (%d)" (string_of_interval_fields ivlf) p
  | `Custom qn -> string_of_qname qn

let string_of_literal = function
  | Lit_null -> "NULL"
  | Lit_bool true -> "TRUE"
  | Lit_bool false -> "FALSE"
  | Lit_integer i -> Int64.to_string i
  | Lit_float x -> Printf.sprintf "%.16g" x
  | Lit_text s -> sql_quote s

let rec bprint_expression buf = function
  | Expr_qname qn -> Buffer.add_string buf (string_of_qname qn)
  | Expr_literal lit -> Buffer.add_string buf (string_of_literal lit)
  | Expr_app (f, []) -> bprint_qname buf f; Buffer.add_string buf "()"
  | Expr_app (f, e :: es) ->
    bprint_qname buf f;
    Buffer.add_char buf '(';
    bprint_expression buf e;
    List.iter (fun e -> Buffer.add_string buf ", "; bprint_expression buf e) es;
    Buffer.add_char buf ')'

let string_of_expression e = with_buffer (fun buf -> bprint_expression buf e)

let string_of_check_constraint {condition; no_inherit} =
  "CHECK (" ^ string_of_expression condition ^ ")" ^
  (if no_inherit then " NO INHERIT" else "")

let string_of_action = function
 | `Cascade -> "CASCADE"
 | `Restrict -> "RESTRICT"

let pp_action = Fmt.using string_of_action Fmt.string

let pp_column_reference =
  let open Fmt in
  const string "REFERENCES" ++ sp
    ++ using (fun {table; _} -> table) pp_qname
    ++ using (fun {columns; _} -> columns)
        (option (sp ++ parens (list ~sep:comma string)))
    ++ using (fun {on_delete; _} -> on_delete)
        (option (sp ++ const string "ON DELETE " ++ pp_action))
    ++ using (fun {on_update; _} -> on_update)
        (option (sp ++ const string "ON UPDATE " ++ pp_action))

let string_of_column_reference = Fmt.to_to_string pp_column_reference

let string_of_column_constraint = function
  | `Check check_constraint -> string_of_check_constraint check_constraint
  | `Not_null -> "NOT NULL"
  | `Null -> "NULL"
  | `Unique -> "UNIQUE"
  | `Primary_key -> "PRIMARY KEY"
  | `Default e -> "DEFAULT(" ^ string_of_expression e ^ ")"
  | `References refspec -> string_of_column_reference refspec

let rec autorec g = function
  | Expr_qname _ | Expr_literal _ as e -> g e
  | Expr_app (f, es) -> g (Expr_app (f, List.map (autorec g) es))

type generator = statement list -> out_channel -> unit
let generators :
  (string, generator * (Arg.key * Arg.spec * Arg.doc) list) Hashtbl.t =
  Hashtbl.create 11
let find_generator gn = Hashtbl.find generators gn
let register_generator ?(arg_specs = []) gn g =
  Hashtbl.add generators gn (g, arg_specs)
let generator_names () = Hashtbl.fold (fun s _ acc -> s :: acc) generators []
