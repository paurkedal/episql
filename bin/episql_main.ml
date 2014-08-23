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
open Unprime_list

let string_of_qname = function
  | (None, name) -> name
  | (Some ns, name) -> ns ^ "." ^ name

let string_of_datatype = function
  | `Smallint | `Smallserial -> "smallint"
  | `Integer | `Serial -> "integer"
  | `Bigint | `Bigserial -> "bigint"
  | `Text | `Char _ | `Varchar _ -> "text"
  | `Numeric _ | `Decimal _ -> "double" (* FIXME *)
  | `Custom _ -> "text" (* FIXME *)

let gen_macaque stmts oc =
  let emit_type dt = output_string oc (string_of_datatype dt) in
  let emit_column_constraint = function
    | `Not_null | `Primary_key -> output_string oc " NOT NULL"
    | `Null | `Unique | `References _ -> ()
    | `Default lit -> () (* FIXME *) in
  let emit_colspec i = function
    | Column (cn, dt, ccs) ->
      if i > 0 then output_string oc ", ";
      output_string oc cn; output_char oc ' ';
      emit_type dt;
      List.iter emit_column_constraint ccs
    | Constraint _ -> () in
  let emit_top = function
    | Create_table (qn, items) ->
      fprintf oc "let %s = <:table< %s (" (snd qn) (string_of_qname qn);
      List.iteri emit_colspec items;
      output_string oc ") >>\n"
    | _ -> () in
  List.iter emit_top stmts

let () =
  let arg_inputs = ref [] in
  let arg_output = ref "-" in
  let arg_generator = ref gen_macaque in
  let set_generator = function
    | "macaque" -> arg_generator := gen_macaque
    | _ -> raise (Arg.Bad "Invalid generator name.") in
  let arg_specs =
    [ "-g", Arg.String set_generator,
	"GENERATOR Select generator. Currently the only option is macaque.";
      "-o", Arg.Set_string arg_output,
	"PATH Output path."; ] in
  Arg.parse arg_specs (fun fp -> arg_inputs := fp :: !arg_inputs) Sys.argv.(0);
  let stmts =
    List.fold (fun fp acc -> Episql.parse_file fp @ acc) !arg_inputs [] in
  match !arg_output with
  | "-" -> !arg_generator stmts stdout
  | fp -> Prime_io.with_file_out (!arg_generator stmts) fp
