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

exception Unsupported

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

let identifier_info =
  let ht = Hashtbl.create 13 in
  Array.iter (fun (idr, info) -> Hashtbl.add ht idr info)
    [|"current_timestamp", `Convert_to_function;
      "localtimestamp", `Convert_to_function;
      "__at_time_zone", `Unsupported|];
  fun idr -> try Hashtbl.find ht idr with Not_found -> `Noop

let translate = autorec @@ function
  | Expr_qname (None, idr) as e ->
    begin match identifier_info idr with
    | `Convert_to_function -> Expr_app ((None, idr), [])
    | `Unsupported -> raise Unsupported
    | _ -> e
    end
  | Expr_app ((None, idr), _) as e ->
    begin match identifier_info idr with
    | `Unsupported -> raise Unsupported
    | _ -> e
    end
  | e -> e

let generate stmts oc =
  let emit_type dt = output_string oc (string_of_datatype dt) in
  let emit_expr e = output_string oc (string_of_expression e) in
  let emit_column_constraint = function
    | `Not_null | `Primary_key -> output_string oc " NOT NULL"
    | `Null | `Unique | `References _ -> ()
    | `Default e ->
      try
	let e = translate e in
	output_string oc " DEFAULT("; emit_expr e; output_char oc ')'
      with Unsupported -> () in
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
  let emit_table_post tqn = function
    | Column (cn, dt, ccs) ->
      let emit_default_nul_workaround = function
	| `Default e ->
	  (try
	    ignore (translate e);
	    fprintf oc "let () = ignore <:value< nullable $%s$?%s >>\n"
		       (snd tqn) cn
	   with Unsupported -> ())
	| _ -> () in
      if List.mem `Not_null ccs then
	List.iter emit_default_nul_workaround ccs
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
      output_string oc " ) >>\n";
      List.iter (emit_table_post tqn) items
    | _ -> () in
  List.iter emit_top stmts

let () = register_generator "macaque" generate
