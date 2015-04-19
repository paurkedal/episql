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

let generate stmts oc =
  output_string oc "\
    # This script was generated by episql.\n\
    # You can source it from shell scripts after defining:\n\
    #   enter_create_table TABLE_QNAME\n\
    #   add_column TABLE_QNAME COLUMN_NAME COLUMN_TYPE CONSTRAINTS\n\
    #   leave_create_table TABLE_QNAME\n\n";
  let emit_column_constraint cc =
    output_string oc " \"";
    (* FIXME: Need proper shell quoting. *)
    output_string oc (String.escaped (string_of_column_constraint cc));
    output_char oc '"' in
  let emit_colspec tqn = function
    | Column (cn, dt, ccs) ->
      fprintf oc "add_column %s %s '%s'"
		 (string_of_qname tqn) cn (string_of_datatype dt);
      List.iter emit_column_constraint ccs;
      output_char oc '\n'
    | Constraint _ -> () in
  let emit_top = function
    | Create_sequence {sequence_qname = sqn; sequence_scope = `Permanent} ->
      fprintf oc "create_sequence %s\n\n" (string_of_qname sqn)
    | Create_table {table_qname = tqn; table_scope = `Permanent;
		    table_items = items} ->
      fprintf oc "enter_create_table %s\n" (string_of_qname tqn);
      List.iter (emit_colspec tqn) items;
      fprintf oc "leave_create_table %s\n" (string_of_qname tqn);
      output_char oc '\n'
    | _ -> () in
  List.iter emit_top stmts

let () = register_generator "shell" generate
