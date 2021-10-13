(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Episql
open Episql.Types
open Unprime
open Unprime_option
open Unprime_list

let use_compact_lists = ref false
let tag name = ("", name)

module Attr = struct
  let string name x = (("", name), x)
  let int name x = string name (string_of_int x)
  let int64 name x = string name (Int64.to_string x)
  let bool name x = string name (string_of_bool x)
  let flag name = bool name true
  let qname name x = string name (string_of_qname x)
  let expr name x = string name (string_of_expression x)
  let string_list name xs = string name (String.concat "," xs)
end

let output_leaf o name attrs =
  Xmlm.output o (`El_start (tag name, attrs));
  Xmlm.output o `El_end

let xml_of_attr = function
 | `Start i -> Attr.int64 "start" i
 | `Increment i -> Attr.int64 "increment" i
 | `Minvalue i -> Attr.int64 "minvalue" i
 | `Maxvalue i -> Attr.int64 "maxvalue" i
 | `Cache i -> Attr.int64 "cache" i
 | `Cycle -> Attr.bool "cycle" true
 | `No_cycle -> Attr.bool "cycle" false
 | `Owner owner -> Attr.string "owner" (string_of_qname owner)

let string_of_action = function
 | `Cascade -> "cascade"
 | `Restrict -> "restrict"

let opt_cons f = Option.fold (List.cons % f)

let xmlattrs_of_column_reference {table; columns; on_delete; on_update} =
  List.cons (Attr.string "ref-table" (string_of_qname table)) @@
  opt_cons (Attr.string "ref-columns" % String.concat " ") columns @@
  opt_cons (Attr.string "on-delete" % string_of_action) on_delete @@
  opt_cons (Attr.string "on-update" % string_of_action) on_update @@ []

let xmlattrs_of_column_constraint (_, constr) = match constr with
 | `Check _ -> [] (* output as element *)
 | `Not_null -> [Attr.bool "nullable" false]
 | `Null -> [Attr.bool "nullable" true]
 | `Unique -> [Attr.bool "unique" true]
 | `Primary_key -> [Attr.bool "primary-key" true]
 | `Default expr -> [Attr.expr "default" expr]
 | `References colref -> xmlattrs_of_column_reference colref

let xmlattrs_of_coltype ct =
  let mk name = name, [] in
  let mk_n name n = name, [Attr.int "length" n] in
  let mk_p name p = name, [Attr.int "precision" p] in
  let mk_ps name p s = name, [Attr.int "precision" p; Attr.int "scale" s] in
  let mk_z name wtz = name, [Attr.bool "with-timezone" wtz] in
  let mk_pz name p wtz =
    name, [Attr.int "precision" p; Attr.bool "with-timezone" wtz] in
  let mk_i fields =
    "interval", [Attr.string "fields" (string_of_interval_fields fields)] in
  let mk_ip fields p =
    "interval", [Attr.string "fields" (string_of_interval_fields fields);
                 Attr.int "precision" p] in
  (match ct with
   | `Boolean           -> mk "boolean"
   | `Smallint          -> mk "smallint"
   | `Smallserial       -> mk "smallserial"     (* not SQL *)
   | `Integer           -> mk "integer"
   | `Serial            -> mk "serial"          (* not SQL *)
   | `Bigint            -> mk "bigint"
   | `Bigserial         -> mk "bigserial"       (* not SQL *)
   | `Real              -> mk "real"
   | `Double_precision  -> mk "double precision"
   | `Text              -> mk "text"            (* not SQL *)
   | `Char n            -> mk_n "char" n
   | `Varchar n         -> mk_n "varchar" n
   | `Bytea             -> mk "bytea"           (* not SQL *)
   | `Numeric_auto      -> mk "numeric"
   | `Numeric (p, 0)    -> mk_p "numeric" p
   | `Numeric (p, s)    -> mk_ps "numeric" p s
   | `Time (None, wtz)  -> mk_z "time" wtz
   | `Time (Some p, wtz) -> mk_pz "time" p wtz
   | `Date              -> mk "date"
   | `Timestamp (None, wtz) -> mk_z "timestamp with timezone" wtz
   | `Timestamp (Some p, wtz) -> mk_pz "timestamp with timezone" p wtz
   | `Interval (`Default, None) -> mk "interval"
   | `Interval (`Default, Some p) -> mk_p "interval" p
   | `Interval (fields, None) -> mk_i fields
   | `Interval (fields, Some p) -> mk_ip fields p
   | `Custom qn  -> mk (string_of_qname qn))

let write_column o name = output_leaf o "column" [Attr.string "name" name]

let write_check_constraint o (name, {condition; no_inherit}) =
  let attrs = [Attr.expr "condition" condition] in
  let attrs = if no_inherit then attrs else Attr.flag "no-inherit" :: attrs in
  let attrs =
    (match name with
     | None -> attrs
     | Some name -> Attr.string "name" name :: attrs) in
  output_leaf o "check" attrs

let write_item o = function
 | Column { column_name = cn; column_type = ct;
            column_collate = cc; column_constraints = constrs } ->
    let ctn, ct_attrs = xmlattrs_of_coltype ct in
    let attrs =
      ct_attrs @ List.flatten (List.map xmlattrs_of_column_constraint constrs)
      |> Option.fold (List.cons % Attr.string "collate") cc
      |> List.cons (Attr.string "type" ctn)
      |> List.cons (Attr.string "name" cn) in
    Xmlm.output o (`El_start (tag "column", attrs));
    constrs |> List.iter begin function
     | (name, `Check check_constraint) ->
        write_check_constraint o (name, check_constraint)
     | _ -> ()
    end;
    Xmlm.output o `El_end
 | Constraint (name, `Check check_constraint) ->
     write_check_constraint o (name, check_constraint)
 | Constraint (name, `Unique cols) ->
    let attrs = List.of_option (Option.map (Attr.string "name") name) in
    if !use_compact_lists then
      output_leaf o "unique" (Attr.string_list "columns" cols :: attrs)
    else begin
      Xmlm.output o (`El_start (tag "unique", attrs));
      List.iter (write_column o) cols;
      Xmlm.output o `El_end
    end
 | Constraint (name, `Primary_key cols) ->
    let attrs = List.of_option (Option.map (Attr.string "name") name) in
    if !use_compact_lists then
      output_leaf o "primary-key" (Attr.string_list "columns" cols :: attrs)
    else begin
      Xmlm.output o (`El_start (tag "primary-key", attrs));
      List.iter (write_column o) cols;
      Xmlm.output o `El_end
    end
 | Constraint (name, `Foreign_key (cols, refspec)) ->
    let attrs = List.of_option (Option.map (Attr.string "name") name) in
    if !use_compact_lists then
      let attrs =
        Attr.string_list "columns" cols ::
        xmlattrs_of_column_reference refspec @
        attrs
      in
      output_leaf o "foreign-key" attrs
    else begin
      let attrs =
        attrs @
        opt_cons (Attr.string "on-delete" % string_of_action)
          refspec.on_delete @@
        opt_cons (Attr.string "on-update" % string_of_action)
          refspec.on_update @@ []
      in
      Xmlm.output o (`El_start (tag "foreign-key", attrs));
      List.iter (write_column o) cols;
      Xmlm.output o (`El_start (tag "references",
                                [Attr.qname "table" refspec.table]));
      (match refspec.columns with
       | None -> ()
       | Some columns -> List.iter (write_column o) columns);
      Xmlm.output o `El_end;
      Xmlm.output o `El_end
    end

let xmlattr_of_drop_option = function
 | `If_exists -> Attr.flag "if-exists"
 | `Cascade -> Attr.flag "cascade"
 | `Restrict -> Attr.flag "restrict"

let write_qn_item o tn qn =
  Xmlm.output o (`El_start (tag tn, [Attr.qname "name" qn]));
  Xmlm.output o `El_end

let write_statement o = function
 | Create_schema name ->
    output_leaf o "create-schema" [Attr.string "name" name]
 | Create_sequence seq ->
    let attrs = List.map xml_of_attr seq.sequence_attrs in
    let attrs =
      (match seq.sequence_scope with
       | `Permanent -> attrs
       | `Temporary -> Attr.string "temporary" "true" :: attrs) in
    let attrs = Attr.qname "name" seq.sequence_qname :: attrs in
    output_leaf o "create-sequence" attrs
 | Create_table table ->
    let attrs =
      if table.table_if_not_exists then [Attr.bool "if-exists" false] else [] in
    let attrs =
      (match table.table_scope with
       | `Permanent -> attrs
       | `Permanent_unlogged -> Attr.string "scope" "unlogged" :: attrs
       | `Temporary -> Attr.string "scope" "temporary" :: attrs) in
    let attrs = Attr.qname "name" table.table_qname :: attrs in
    Xmlm.output o (`El_start (tag "create-table", attrs));
    List.iter (write_item o) table.table_items;
    Xmlm.output o `El_end
 | Create_enum (qn, labels) ->
    Xmlm.output o (`El_start (tag "create-enum", [Attr.qname "name" qn]));
    List.iter
      (fun label ->
        Xmlm.output o (`El_start (tag "item", []));
        Xmlm.output o (`Data label);
        Xmlm.output o `El_end)
      labels;
    Xmlm.output o `El_end
 | Drop_schema (names, opts) ->
    let attrs = List.map xmlattr_of_drop_option opts in
    Xmlm.output o (`El_start (tag "drop-schemas", attrs));
    List.iter
      (fun sn ->
        Xmlm.output o (`El_start (tag "schema", [Attr.string "name" sn]));
        Xmlm.output o `El_end)
      names;
    Xmlm.output o `El_end
 | Drop_table (qns, opts) ->
    let attrs = List.map xmlattr_of_drop_option opts in
    Xmlm.output o (`El_start (tag "drop-tables", attrs));
    List.iter (write_qn_item o "table") qns;
    Xmlm.output o `El_end
 | Drop_sequence (qns, opts) ->
    let attrs = List.map xmlattr_of_drop_option opts in
    Xmlm.output o (`El_start (tag "drop-sequences", attrs));
    List.iter (write_qn_item o "sequence") qns;
    Xmlm.output o `El_end
 | Drop_type (qns, opts) ->
    let attrs = List.map xmlattr_of_drop_option opts in
    Xmlm.output o (`El_start (tag "drop-types", attrs));
    List.iter (write_qn_item o "type") qns;
    Xmlm.output o `El_end

let generate_xml stmts oc =
  let o = Xmlm.make_output (`Channel oc) in
  Xmlm.output o (`Dtd None);
  Xmlm.output o (`El_start (tag "episql", []));
  List.iter (write_statement o) stmts;
  Xmlm.output o `El_end

let () = Episql.register_generator "xml" generate_xml
