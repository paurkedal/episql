(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

exception Unsupported

let string_of_datatype = function
 | `Boolean -> "boolean"
 | `Smallint | `Smallserial -> "smallint"
 | `Integer | `Serial -> "integer"
 | `Bigint | `Bigserial -> "bigint"
 | `Real | `Double_precision -> "double"
 | `Text | `Char _ | `Varchar _ -> "text"
 | `Bytea -> "bytea"
 | `Numeric_auto | `Numeric _ -> "double" (* FIXME *)
 | `Time (_, _) -> "time"
 | `Date -> "date"
 | `Timestamp (_, false) -> "timestamp"
 | `Timestamp (_, true) -> "timestamptz"
 | `Interval (_, _) -> "interval"
 | `Custom _ -> "text" (* FIXME *)

let string_of_serialtype = function
 | `Smallserial -> "smallserial"
 | `Serial -> "serial"
 | `Bigserial -> "bigserial"

let int_conversion_of_datatype = function
 | `Smallint | `Smallserial -> "int16", ""
 | `Integer | `Serial -> "int32", "l"
 | `Bigint | `Bigserial -> "int64", "L"
 | `Real | `Double_precision -> "float", "e0"
 | _ -> failwith "Found integer literal in DEFAULT value of non-integer."

let qname_info =
  let ht = Hashtbl.create 13 in
  Array.iter (fun (idr, info) -> Hashtbl.add ht idr info)
    [|"current_timestamp", `Convert_to_function;
      "localtimestamp", `Convert_to_function;
      "__at_time_zone", `Unsupported|];
  (function
   | (None, idr) -> (try Hashtbl.find ht idr with Not_found -> `Unsupported)
   | (Some _, _) -> `Unsupported)

let rec translate_inner = function
 | Expr_qname qn as e ->
    (match qname_info qn with
     | `Convert_to_function -> Expr_app (qn, [])
     | `Unsupported -> raise Unsupported
     | `Noop -> e)
 | Expr_literal (Lit_integer _) -> raise Unsupported
 | Expr_literal (Lit_text _) as e -> e
 | Expr_app (qn, es) ->
    (match qname_info qn with
     | `Unsupported -> raise Unsupported
     | `Noop | `Convert_to_function ->
        Expr_app (qn, List.map translate_inner es))

let translate dt = function
 | Expr_literal _ as e -> e
 | Expr_app ((None, "nextval"), _) as e when dt = `Integer -> e
 | e -> translate_inner e

let generate stmts oc =
  let emit_type dt = output_string oc (string_of_datatype dt) in
  let rec emit_expr = function
   | Expr_qname (_, name) -> fprintf oc "$%s$" name
   | Expr_literal (Lit_integer _) -> assert false
   | Expr_literal (Lit_text s) ->
      fprintf oc "$string:\"%s\"$" (String.escaped s)
   | Expr_app (f, []) ->
      output_string oc (string_of_qname f);
      output_string oc "()"
   | Expr_app (f, e :: es) ->
      output_string oc (string_of_qname f);
      output_char oc '(';
      emit_expr e;
      List.iter (fun e -> output_string oc ", "; emit_expr e) es;
      output_char oc ')' in
  let emit_typed_expr dt = function
   | Expr_literal (Lit_integer i) ->
      let conv, suffix = int_conversion_of_datatype dt in
      fprintf oc "$%s:%Ld%s$" conv i suffix
   | e -> emit_expr e in
  let emit_column_constraint dt = function
   | `Not_null | `Primary_key -> output_string oc " NOT NULL"
   | `Null | `Unique | `References _ | `On_delete _ | `On_update _ -> ()
   | `Default e ->
      try
        let e = translate dt e in
        output_string oc " DEFAULT("; emit_typed_expr dt e; output_char oc ')'
      with Unsupported -> () in
  let emit_serial_seq tqn = function
   | Column {column_name = cn; column_type = #serialtype as dt; _} ->
      fprintf oc "let %s_%s_seq =\n  <:sequence< %s \"%s_%s_seq\" >>\n"
                 (snd tqn) cn
                 (string_of_serialtype dt)
                 (string_of_qname tqn) cn
   | _ -> () in
  let emit_colspec tqn i = function
   | Column {column_name = cn; column_type = dt; column_constraints = ccs; _} ->
      output_string oc (if i > 0 then ",\n\t" else "\n\t");
      output_string oc cn; output_char oc ' ';
      emit_type dt;
      List.iter (emit_column_constraint dt) ccs;
      begin match dt with
      | #serialtype -> fprintf oc " DEFAULT(nextval $%s_%s_seq$)" (snd tqn) cn
      | _ -> ()
      end
   | Constraint _ -> () in
  let emit_table_post tqn = function
   | Column {column_name = cn; column_type = dt; column_constraints = ccs; _} ->
      let emit_default_nul_workaround = function
       | `Default e ->
          (try
            ignore (translate dt e);
            fprintf oc "let () = ignore <:value< nullable $%s$?%s >>\n"
                       (snd tqn) cn
           with Unsupported -> ())
       | _ -> () in
      if List.mem `Not_null ccs then
        List.iter emit_default_nul_workaround ccs
   | Constraint _ -> () in
  let emit_top = function
   | Create_schema _
   | Create_sequence {sequence_scope = `Temporary; _}
   | Create_table {table_scope = `Temporary; _} -> ()
   | Create_sequence {sequence_qname = sqn; _} ->
      (* CHECKME: Better to use bigserial? *)
      (* TODO: sequence_attrs *)
      fprintf oc "let %s =\n  <:sequence< serial \"%s\" >>\n"
                 (snd sqn) (string_of_qname sqn);
   | Create_table {table_qname = tqn; table_items = items; _} ->
      List.iter (emit_serial_seq tqn) items;
      fprintf oc "let %s =\n  <:table< %s (" (snd tqn) (string_of_qname tqn);
      List.iteri (emit_colspec tqn) items;
      output_string oc " ) >>\n";
      List.iter (emit_table_post tqn) items
   | _ -> () in
  List.iter emit_top stmts

let () = register_generator "macaque" generate
