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
open Episql
open Printf

let string_of_datatype = function
  | `Boolean -> "boolean"
  | `Smallint | `Smallserial -> "smallint"
  | `Integer | `Serial -> "integer"
  | `Bigint | `Bigserial -> "bigint"
  | `Real | `Double_precision -> "double"
  | `Text | `Char _ | `Varchar _ -> "text"
  | `Bytea -> "bytea"
  | `Numeric _ | `Decimal _ -> "double" (* FIXME *)
  | `Time -> "time"
  | `Date -> "date"
  | `Timestamp -> "timestamp"
  | `Timestamp_with_timezone -> "timestamptz"
  | `Interval -> "interval"
  | `Custom _ -> "text" (* FIXME *)

let string_of_serialtype = function
  | `Smallserial -> "smallserial"
  | `Serial -> "serial"
  | `Bigserial -> "bigserial"

let generate stmts oc =
  let emit_type dt = output_string oc (string_of_datatype dt) in
  let emit_column_constraint = function
    | `Not_null | `Primary_key -> output_string oc " NOT NULL"
    | `Null | `Unique | `References _ -> ()
    | `Default lit -> () (* FIXME *) in
  let emit_serial_seq tqn = function
    | Column (cn, (#serialtype as dt), _) ->
      fprintf oc "let %s_%s_seq =\n  <:sequence< %s \"%s_%s_seq\" >>\n"
		 (snd tqn) cn
		 (string_of_serialtype dt)
		 (string_of_qname tqn) cn
    | _ -> () in
  let emit_colspec tqn i = function
    | Column (cn, dt, ccs) ->
      output_string oc (if i > 0 then ",\n\t" else "\n\t");
      output_string oc cn; output_char oc ' ';
      emit_type dt;
      List.iter emit_column_constraint ccs;
      begin match dt with
      | #serialtype -> fprintf oc " DEFAULT(nextval $%s_%s_seq$)" (snd tqn) cn
      | _ -> ()
      end
    | Constraint _ -> () in
  let emit_top = function
    | Create_schema _ | Create_sequence (_, true, _) -> ()
    | Create_sequence (sqn, false, attrs) ->
      (* CHECKME: Better to use bigserial? *)
      fprintf oc "let %s =\n  <:sequence< serial \"%s\" >>\n"
		 (snd sqn) (string_of_qname sqn);
    | Create_table (tqn, items) ->
      List.iter (emit_serial_seq tqn) items;
      fprintf oc "let %s =\n  <:table< %s (" (snd tqn) (string_of_qname tqn);
      List.iteri (emit_colspec tqn) items;
      output_string oc " ) >>\n"
    | _ -> () in
  List.iter emit_top stmts

let () = register_generator "macaque" generate
