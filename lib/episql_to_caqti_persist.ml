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
open Unprime_option

type genopts = {
  mutable go_types_module : string option;
  mutable go_event : bool;
  mutable go_patch : bool;
  mutable go_value : bool;
  mutable go_insert : bool;
  mutable go_insert_upserts : bool;
  mutable go_create : bool;
  mutable go_update : bool;
  mutable go_delete : bool;
  mutable go_getters : bool;
  mutable go_select : bool;
  mutable go_select_cache : bool;
  mutable go_collapse_pk : bool;
  mutable go_pk_prefix : string;
  mutable go_state_prefix : string;
  mutable go_value_prefix : string;
  mutable go_value_r_prefix : string;
  mutable go_value_d_prefix : string;
  mutable go_deriving : string list;
  mutable go_open : string list;
  mutable go_type_counit : string;
  mutable go_type_date : string;
  mutable go_type_timestamp : string;
  mutable go_raise_on_absent : bool;
  mutable go_log_debug : string option;
}
let go = {
  go_types_module = None;
  go_event = true;
  go_patch = true;
  go_value = true;
  go_insert = true;
  go_insert_upserts = true;
  go_create = true;
  go_update = true;
  go_delete = true;
  go_getters = true;
  go_select = true;
  go_select_cache = true;
  go_collapse_pk = true;
  go_pk_prefix = "k_";
  go_state_prefix = "s_";
  go_value_prefix = "v_";
  go_value_r_prefix = "r_";
  go_value_d_prefix = "d_";
  go_deriving = [];
  go_open = [];
  go_type_counit = "Prime.counit";
  go_type_date = "CalendarLib.Date.t";
  go_type_timestamp = "CalendarLib.Calendar.t";
  go_raise_on_absent = false;
  go_log_debug = Some "caqti-persist";
}

let convname_of_datatype = function
  | `Boolean -> "bool"
  | `Smallint | `Smallserial -> "int"
  | `Integer | `Serial -> "int32"
  | `Bigint | `Bigserial -> "int64"
  | `Real | `Double_precision -> "float"
  | `Text | `Char _ | `Varchar _ -> "text"
  | `Bytea -> "octets"
  | `Numeric _ | `Decimal _ -> "float" (* FIXME *)
  | `Time -> "float"
  | `Date -> "date"
  | `Timestamp -> "utc"
  | `Timestamp_with_timezone -> "utc"
  | `Interval -> "string" (* FIXME *)
  | `Custom _ -> "string"

let string_of_datatype = function
  | `Boolean -> "bool"
  | `Smallint | `Smallserial -> "int"
  | `Integer | `Serial -> "int32"
  | `Bigint | `Bigserial -> "int64"
  | `Real | `Double_precision -> "float"
  | `Text | `Char _ | `Varchar _ -> "string"
  | `Bytea -> "string"
  | `Numeric _ | `Decimal _ -> "float" (* FIXME *)
  | `Time -> "float"
  | `Date -> go.go_type_date
  | `Timestamp -> go.go_type_timestamp
  | `Timestamp_with_timezone -> go.go_type_timestamp
  | `Interval -> "string" (* FIXME *)
  | `Custom _ -> "string"

type coltype = {
  ct_type : datatype;
  ct_pk : bool;
  ct_nullable : bool;
  ct_defaultable : bool;
}

let coltype_is_required ct = not ct.ct_nullable && not ct.ct_defaultable

let convname_of_coltype ct =
  let s = convname_of_datatype ct.ct_type in
  if ct.ct_nullable then "option " ^ s else s

let string_of_coltype ct =
  let s = string_of_datatype ct.ct_type in
  if ct.ct_nullable then s ^ " option" else s

type table_info = {
  ti_tqn : string option * string;
  ti_cts : (string * coltype) list;
  ti_req_cts : (string * coltype) list;
  ti_pk_cts : (string * coltype) list;
  ti_nonpk_cts : (string * coltype) list;
  ti_nonpk_def_cts : (string * coltype) list;
  ti_nonpk_req_cts : (string * coltype) list;
  ti_nonpk_nonreq_cts : (string * coltype) list;
  ti_pk_has_default : bool;
}

let collect = function
  | Column (cn, ct_type, ccs) ->
    let is_serial = function #serialtype -> true | _ -> false in
    let is_default = function `Default _ -> true | _ -> false in
    let ct_defaultable = is_serial ct_type || List.exists is_default ccs in
    if List.mem `Primary_key ccs then
      begin function
      | None, cts ->
	let ct = {ct_pk = true; ct_type; ct_nullable = false; ct_defaultable} in
	Some [cn], (cn, ct) :: cts
      | Some _, _ -> failwith "Cannot have multiple primary keys."
      end
    else
      let ct_nullable = not (List.mem `Not_null ccs) in
      let ct = {ct_pk = false; ct_type; ct_nullable; ct_defaultable} in
      fun (pk_opt, cts) -> (pk_opt, (cn, ct) :: cts)
  | Constraint (`Primary_key pk) ->
    begin function
    | None, cts -> Some pk, cts
    | Some _, _ -> failwith "Cannot have multiple primary keys."
    end
  | Constraint _ -> fun acc -> acc

let fprint oc s = output_string oc s
let fprintl oc s = output_string oc s; output_char oc '\n'
let fprintlf oc fmt = ksprintf (fprintl oc) fmt
let rec findent oc n =
  if n = 0  then () else
  if n >= 8 then (output_char oc '\t'; findent oc (n - 8))
	    else (output_char oc ' ';  findent oc (n - 1))

let emit_custom_open oc =
  List.iter (fun m -> fprintlf oc "open %s" m) (List.rev go.go_open);
  output_char oc '\n'

let emit_deriving_nl oc =
  if go.go_deriving = [] then
    fprintl oc "\n"
  else
    fprintlf oc " deriving (%s)\n" (String.concat ", " go.go_deriving)

let emit_type_pk oc ti =
  if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then begin
    fprintf oc "    type key = %s"
	    (string_of_coltype (snd (List.hd ti.ti_pk_cts)));
    emit_deriving_nl oc
  end else begin
    fprintl oc "    type key = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      %s%s : %s;"
		 go.go_pk_prefix cn (string_of_coltype ct))
      ti.ti_pk_cts;
    fprint oc "    }";
    emit_deriving_nl oc
  end

let emit_type_nonpk ~in_intf oc ti =
  if ti.ti_nonpk_cts = [] then
    fprintl oc "    type state = unit"
  else begin
    if in_intf then
      fprintl oc "    type state = private {"
    else
      fprintl oc "    type state = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      mutable %s%s : %s;"
		 go.go_state_prefix cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    fprintl oc "    }"
  end

let emit_type_patch_etc oc ti =
  if ti.ti_nonpk_cts = [] then begin
    fprint  oc "    type value = unit"; emit_deriving_nl oc
  end else begin
    fprintl oc "    type value = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      %s%s : %s;"
		 go.go_value_prefix cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    fprint  oc "    }"; emit_deriving_nl oc
  end;
  if ti.ti_nonpk_req_cts = [] then begin
    fprint  oc "    type value_r = unit"; emit_deriving_nl oc
  end else begin
    fprintl oc "    type value_r = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      %s%s : %s;"
		 go.go_value_r_prefix cn (string_of_datatype ct.ct_type))
      ti.ti_nonpk_req_cts;
    fprint  oc "    }"; emit_deriving_nl oc
  end;
  if ti.ti_nonpk_nonreq_cts = [] then begin
    fprint  oc "    type value_d = unit"; emit_deriving_nl oc
  end else begin
    fprintl oc "    type value_d = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      %s%s : %s option;"
		 go.go_value_d_prefix cn (string_of_datatype ct.ct_type))
      ti.ti_nonpk_nonreq_cts;
    fprint  oc "    }"; emit_deriving_nl oc
  end;
  if ti.ti_nonpk_cts = [] then begin
    fprintf oc "    type change = %s" go.go_type_counit; emit_deriving_nl oc
  end else begin
    fprint  oc "    type change =";
    List.iteri
      (fun i (cn, ct) ->
	fprintf oc "\n      %c `Set_%s of %s"
		(if i = 0 then '[' else '|') cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    fprint oc "]"; emit_deriving_nl oc;
  end;
  fprint oc "    type patch_in = (value_r, value_d, change) persist_patch_in";
  emit_deriving_nl oc;
  fprint oc "    type patch_out = (value, change) persist_patch_out";
  emit_deriving_nl oc

let emit_types ~in_intf oc ti =
  if in_intf then
    fprintlf oc "module %s : sig\n" (String.capitalize (snd ti.ti_tqn))
  else
    fprintlf oc "module %s = struct\n" (String.capitalize (snd ti.ti_tqn));
  emit_type_pk oc ti;
  emit_type_patch_etc oc ti;
  if in_intf then begin
    fprintl oc "    val defaults : value_d"
  end else if ti.ti_nonpk_nonreq_cts = [] then
    fprintl oc "    let defaults = ()"
  else begin
    fprintl oc "    let defaults = {";
    List.iter
      (fun (cn, _) -> fprintlf oc "\t%s%s = None;" go.go_value_d_prefix cn)
      ti.ti_nonpk_nonreq_cts;
    fprintl oc "    }"
  end;
  if in_intf then
    fprintl oc "    val changes_of_value : value -> change list"
  else begin
    fprintl oc "    let changes_of_value f = [";
    List.iter
      (fun (cn, _) -> fprintlf oc "\t`Set_%s f.%s%s;" cn go.go_value_prefix cn)
      ti.ti_nonpk_cts;
    fprintl oc "    ]"
  end;
  fprintl oc "end\n"

let emit_intf oc ti =
  fprintf oc "  module %s : sig\n" (String.capitalize (snd ti.ti_tqn));
  begin match go.go_types_module with
  | None ->
    emit_type_pk oc ti;
    emit_type_patch_etc oc ti
  | Some types_module ->
    let mn = types_module ^ "." ^ String.capitalize (snd ti.ti_tqn) in
    fprintlf oc "    include module type of %s" mn;
    fprintlf oc "\twith type key = %s.key" mn;
    fprintlf oc "\t and type value = %s.value" mn;
    fprintlf oc "\t and type value_r = %s.value_r" mn;
    fprintlf oc "\t and type value_d = %s.value_d" mn;
    fprintlf oc "\t and type change = %s.change" mn;
    fprintlf oc "\t and type patch_in = %s.patch_in" mn;
    fprintlf oc "\t and type patch_out = %s.patch_out" mn
  end;
  emit_type_nonpk ~in_intf:true oc ti;
  fprintl oc "    type t";
  fprintl oc "    val key : t -> key";
  fprintl oc "    val is_present : t -> bool";
  if go.go_raise_on_absent then
    fprintl oc "    val state : t -> state"
  else
    fprintl oc "    val state : t -> state option";
  if go.go_value then begin
    if go.go_raise_on_absent then
      fprintl oc "    val value : t -> value"
    else
      fprintl oc "    val value : t -> value option"
  end;
  if go.go_getters then begin
    List.iter
      (fun (cn, ct) ->
	let is_opt = ct.ct_pk || go.go_raise_on_absent && not ct.ct_nullable in
	fprintlf oc "    val get_%s : t -> %s%s"
		 cn (string_of_datatype ct.ct_type)
		 (if is_opt then "" else " option"))
      ti.ti_cts
  end;
  fprintl oc "    val fetch : key -> t Lwt.t";
  if go.go_select then begin
    fprint oc "    val select :";
    List.iter
      (fun (cn, ct) ->
	let tn = string_of_datatype ct.ct_type in
	fprintf oc "\n      ?%s: [< %s order_predicate" cn tn;
	if ct.ct_type = `Text then
	  fprintf oc " | `Like of %s | `Ilike of %s" tn tn;
	if ct.ct_nullable then fprint oc " | `Null";
	fprint oc "] ->")
      ti.ti_cts;
      fprintl oc "\n      unit -> t list Lwt.t"
  end;
  if go.go_create then begin
    fprint oc "    val create :";
    List.iter
      (fun (cn, ct) ->
	fprintf oc "\n      %s%s: %s ->"
		(if ct.ct_nullable || ct.ct_defaultable then "?" else "")
		cn (string_of_datatype ct.ct_type))
      ti.ti_cts;
    fprintl oc "\n      unit -> t Lwt.t"
  end;
  if go.go_insert then begin
    fprint oc "    val insert :";
    List.iter
      (fun (cn, ct) ->
	fprintf oc "\n      %s%s: %s ->"
		(if ct.ct_nullable || ct.ct_defaultable then "?" else "")
		cn (string_of_datatype ct.ct_type))
      ti.ti_nonpk_cts;
    fprintl oc "\n      t -> unit Lwt.t"
  end;
  if go.go_update && ti.ti_nonpk_cts <> [] then begin
    fprint  oc "    val update :";
    List.iter
      (fun (cn, {ct_type = dt; ct_nullable = dn}) ->
	fprintf oc "\n      ?%s: %s%s ->" cn (string_of_datatype dt)
		(if dn then " option" else ""))
      ti.ti_nonpk_cts;
    fprintl oc "\n      t -> unit Lwt.t"
  end;
  if go.go_delete then
    fprintl oc "    val delete : t -> unit Lwt.t";
  if go.go_patch then
    fprintl oc "    val patch : t -> patch_in -> unit Lwt.t";
  if go.go_event then
    fprintl oc "    val patches : t -> patch_out React.E.t";
  fprintl oc "  end\n"

let emit_query oc name emit =
  let pgparam_no = ref 0 in
  let pgparam () = incr pgparam_no; sprintf "$%d" !pgparam_no in
  let sqliteparam () = "?" in
  fprintlf oc "      let %s = Caqti_query.prepare_fun @@ function" name;
  fprint   oc "\t| `Pgsql -> \""; emit pgparam; fprintl oc "\"";
  fprint   oc "\t| `Sqlite -> \""; emit sqliteparam; fprintl oc "\"";
  fprintl  oc "\t| _ -> raise Caqti_query.Missing_query_string"

let use_C = "P.use_db @@ fun (module C : Caqti_lwt.CONNECTION) ->\n"
let emit_use_C oc n = findent oc n; output_string oc use_C

let emit_param oc ti pk cts =
  let n_pk = List.length ti.ti_pk_cts in
  fprint oc "C.Param.([|";
  List.iteri
    (fun i (cn, ct) ->
      if i > 0 then fprint oc "; ";
      fprint oc (convname_of_coltype ct); fprint oc " ";
      if go.go_collapse_pk && n_pk = 1 && ct.ct_pk then
	fprint oc pk
      else if ct.ct_pk then
	fprintf oc "%s.%s%s" pk go.go_pk_prefix cn
      else
	fprintf oc "%s" cn)
    cts;
  fprint oc "|])"

let emit_detuple oc cts =
  if cts = [] then
    fprint oc "C.Tuple.(fun t -> ())"
  else begin
    fprint oc "C.Tuple.(fun t -> {";
    List.iteri
      (fun i (cn, ct) ->
	if i > 0 then fprint oc "; ";
	fprintf oc "%s%s = %s %d t"
		go.go_state_prefix cn (convname_of_coltype ct) i)
      cts;
    fprint oc "})"
  end

let emit_impl oc ti =
  let have_default = ti.ti_pk_has_default || ti.ti_nonpk_def_cts <> [] in

  let emit_pk_cond next_param =
    List.iteri
      (fun i (cn, _) ->
	if i > 0 then fprint oc " AND ";
	fprintf oc "%s = %s" cn (next_param ()))
      ti.ti_pk_cts in

  let emit_fetch next_param =
    fprintf oc "SELECT %s FROM %s WHERE "
      (String.concat ", " (List.map (fun (cn, _) -> cn) ti.ti_nonpk_cts))
      (Episql.string_of_qname ti.ti_tqn);
    emit_pk_cond next_param in

  let emit_delete next_param =
    fprintf oc "DELETE FROM %s WHERE " (Episql.string_of_qname ti.ti_tqn);
    emit_pk_cond next_param in

  Option.iter (fprintlf oc "  let section = Lwt_log.Section.make \"%s\"")
	      go.go_log_debug;

  fprintf oc "  module %s = struct\n" (String.capitalize (snd ti.ti_tqn));
  fprintl oc "    module Q = struct";
  emit_query oc "fetch" emit_fetch;
  emit_query oc "delete" emit_delete;
  fprintl oc "    end";
  begin match go.go_types_module with
  | None ->
    emit_type_pk oc ti;
    emit_type_patch_etc oc ti
  | Some tm ->
    fprintlf oc "    include %s.%s" tm (String.capitalize (snd ti.ti_tqn))
  end;
  emit_type_nonpk ~in_intf:false oc ti;
  fprintl oc "    include Cache (struct";
  fprintl oc "      type _t0 = key\ttype key = _t0";
  fprintl oc "      type _t1 = state\ttype state = _t1";
  fprintl oc "      type _t2 = value\ttype value = _t2";
  fprintl oc "      type _t3 = change\ttype change = _t3";
  fprintlf oc "      let key_size = %d" (List.length ti.ti_pk_cts);
  fprintlf oc "      let state_size = %d" (List.length ti.ti_nonpk_cts);
  fprintl oc "      let fetch key =";
  emit_use_C oc 8;
  fprint  oc "\tC.find_opt Q.fetch ";
  emit_detuple oc ti.ti_nonpk_cts; fprint oc " ";
  emit_param oc ti "key" ti.ti_pk_cts; fprintl oc "";
  fprintl oc "    end)";

  fprintl oc "    let key {key} = key";
  if go.go_raise_on_absent then begin
    fprintl oc "    let absent op o =";
    if go.go_log_debug <> None then
      fprintlf oc "      Lwt_log.ign_debug_f ~section \
			    \"Called %%s on absent row of %s.\" op;"
		  (snd ti.ti_tqn);
    fprintl oc "      raise Not_present"
  end;
  fprintl oc "    let is_present = \
		    function {state = Present _} -> true | _ -> false";
  if go.go_raise_on_absent then
    fprintl oc "    let state = \
		      function {state = Present x} -> x \
			     | o -> absent \"state\" o"
  else
    fprintl oc "    let state = \
		      function {state = Present x} -> Some x | _ -> None";
  if go.go_getters then begin
    let n_pk = List.length ti.ti_pk_cts in
    List.iter
      (fun (cn, ct) ->
	fprint oc "    let get_"; fprint oc cn; fprint oc " o = ";
	if go.go_collapse_pk && n_pk = 1 && ct.ct_pk then
	  fprintlf oc "o.key"
	else if ct.ct_pk then
	  fprintlf oc "o.key.%s%s" go.go_pk_prefix cn
	else if go.go_raise_on_absent then
	  fprintlf oc "match o.state with Present x -> x.%s%s \
					| _ -> absent \"get_%s\" o"
		   go.go_state_prefix cn cn
	else if ct.ct_nullable then
	  fprintlf oc "match o.state with Present x -> x.%s%s | _ -> None"
		   go.go_state_prefix cn
	else
	  fprintlf oc "match o.state with Present x -> Some x.%s%s | _ -> None"
		   go.go_state_prefix cn)
      ti.ti_cts
  end;

  if go.go_select_cache then begin
    fprintl oc "    let select_cache = \
		      Prime_cache.create P.Beacon.cache_metric 19";
    fprintl oc "    let clear_select_cache () = Prime_cache.clear select_cache"
  end;

  if go.go_insert || go.go_patch then begin
    fprint  oc "    let insert";
    List.iter
      (fun (cn, ct) ->
	fprintf oc " %c%s"
		(if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_nonpk_cts;
    fprintl  oc " o =";
    fprintl  oc "      let rec retry () =";
    fprintl  oc "\tmatch o.state with";
    fprintl  oc "\t| Absent ->";
    fprintl  oc "\t  let c = Lwt_condition.create () in";
    fprintl  oc "\t  o.state <- Inserting c;";
    emit_use_C oc 10;
    fprintl  oc "\t    let module Ib = Insert_buffer (C) in";
    fprintlf oc "\t    let ib = Ib.create C.backend_info \"%s\" in"
	     (Episql.string_of_qname ti.ti_tqn);
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      let cn, ct = List.hd ti.ti_pk_cts in
      fprintlf oc "\t    Ib.set ib \"%s\" C.Param.(%s o.key);"
	       cn (convname_of_datatype ct.ct_type)
    else
      List.iter
	(fun (cn, ct) ->
	  fprintlf oc "\t    Ib.set ib \"%s\" C.Param.(%s o.key.%s%s);"
		   cn (convname_of_datatype ct.ct_type) go.go_pk_prefix cn)
	ti.ti_pk_cts;
    List.iter
      (fun (cn, ct) ->
	if ct.ct_nullable || ct.ct_defaultable then
	  fprintlf oc "\t    (match %s with \
			      None -> () \
			      | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
		   cn cn (convname_of_datatype ct.ct_type)
	else
	  fprintlf oc "\t    Ib.set ib \"%s\" C.Param.(%s %s);"
		   cn (convname_of_datatype ct.ct_type) cn)
      ti.ti_nonpk_cts;
    List.iter
      (fun (cn, ct) ->
	if ct.ct_defaultable then
	  fprintlf oc "\t    Ib.ret ib \"%s\";" cn)
	ti.ti_nonpk_cts;
    fprintl  oc "\t    let q, p = Ib.contents ib in";

    let emit_field pfx i (cn, ct) =
      if i > 0 then fprint oc "; ";
      fprintf oc "%s%s = %s" pfx cn cn in
    if ti.ti_nonpk_def_cts <> [] then begin
      fprintl  oc "\t    let decode t =";
      fprintl  oc "\t      let _i = ref (-1) in";
      fprintl  oc "\t      let _df c = function Some x -> x \
					      | None -> incr _i; c !_i t in";
      List.iter
	(fun (cn, ct) ->
	  fprint oc "\t      let "; fprint oc cn;
	  fprint oc (if ct.ct_nullable then " = Some (" else " = ");
	  fprintf oc "_df C.Tuple.%s %s" (convname_of_datatype ct.ct_type) cn;
	  fprintl oc (if ct.ct_nullable then ") in" else " in"))
	ti.ti_nonpk_def_cts;
      if ti.ti_nonpk_cts = [] then
	fprintl oc " () in"
      else begin
	fprint oc "\t      {";
	List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
	fprintl oc "} in"
      end;
      fprintl oc "\t    C.find q decode p >|= fun state ->";
      if go.go_select_cache then
	fprintl oc "\t    clear_select_cache ();";
    end else begin
      fprintl oc "\t    C.exec q p >|= fun () ->";
      if go.go_select_cache then
	fprintl oc "\t    clear_select_cache ();";
      if ti.ti_nonpk_cts = [] then
	fprintl oc "\t    let state = () in"
      else begin
	fprint oc "\t    let state = {";
	List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
	fprintl oc "} in"
      end
    end;
    fprintl  oc "\t    o.state <- Present state;";
    fprint   oc "\t    Lwt_condition.broadcast c state";
    if go.go_event then begin
      fprintl oc ";";
      if ti.ti_nonpk_cts = [] then
	fprintl oc "\t    o.notify (`Insert ())"
      else begin
	fprintl oc "\t    o.notify (`Insert {";
	List.iter
	  (fun (cn, ct) ->
	    fprintlf oc "\t      %s%s = state.%s%s;"
		     go.go_value_prefix cn go.go_state_prefix cn)
	  ti.ti_nonpk_cts;
	fprintl oc "\t    })"
      end
    end else
      fprintl oc "";
    fprintl  oc "\t| Inserting c -> Lwt_condition.wait c >|= fun _ -> ()";
    fprintl  oc "\t| Present x -> Lwt.return_unit";
    fprintl  oc "\t| Deleting c -> Lwt_condition.wait c >>= retry in";
    fprintl  oc "      retry ()"
  end;

  if go.go_create then begin
    fprint  oc "    let create";
    List.iter
      (fun (cn, ct) ->
	fprintf oc " %c%s"
	  (if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_cts;
    fprintl oc " () =";
    emit_use_C oc 6;
    fprintl  oc "\tlet module Ib = Insert_buffer (C) in";
    fprintlf oc "\tlet ib = Ib.create C.backend_info \"%s\" in"
	     (Episql.string_of_qname ti.ti_tqn);
    List.iter
      (fun (cn, ct) ->
	if ct.ct_defaultable then
	  fprintlf oc "\t(match %s with None -> Ib.ret ib \"%s\" \
			   | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
		   cn cn cn (convname_of_datatype ct.ct_type)
	else if ct.ct_nullable then
	  fprintlf oc "\t(match %s with None -> () \
			   | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
		   cn cn (convname_of_datatype ct.ct_type)
	else
	  fprintlf oc "\tIb.set ib \"%s\" C.Param.(%s %s);"
		   cn (convname_of_datatype ct.ct_type) cn)
      ti.ti_cts;
    if have_default then
      fprintl oc "\tif not (Ib.have_ret ib) then Ib.ret ib \"0\";";
    fprintl  oc "\tlet q, p = Ib.contents ib in";
    fprintl  oc "\tlet decode t =";
    if have_default then begin
      fprintl  oc "\t  let _i = ref (-1) in";
      fprintl  oc "\t  let _df c = function Some x -> x \
					  | None -> incr _i; c !_i t in";
    end;
    List.iter
      (fun (cn, ct) ->
	if ct.ct_defaultable then begin
	  fprint oc "\t  let "; fprint oc cn;
	  fprint oc (if ct.ct_nullable then " = Some (" else " = ");
	  fprintf oc "_df C.Tuple.%s %s" (convname_of_datatype ct.ct_type) cn;
	  fprintl oc (if ct.ct_nullable then ") in" else " in")
	end)
      ti.ti_cts;
    let emit_field pfx i (cn, ct) =
      if i > 0 then fprint oc "; ";
      fprintf oc "%s%s = %s" pfx cn cn in
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      fprintf oc "\t  %s," (fst (List.hd ti.ti_pk_cts))
    else begin
      fprint oc "\t  {";
      List.iteri (emit_field go.go_pk_prefix) ti.ti_pk_cts;
      fprint oc "},"
    end;
    if ti.ti_nonpk_cts = [] then
      fprintl oc " () in"
    else begin
      fprint oc "\n\t  {";
      List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
      fprintl oc "} in"
    end;
    if have_default then begin
      fprint  oc "\tC.find q decode p >>= fun (key, state) ->\n\t";
      if go.go_select_cache then fprint oc "clear_select_cache (); ";
      fprintl oc "merge_created (key, state)"
    end else begin
      fprint  oc "\tC.exec q p >>= fun () -> ";
      if go.go_select_cache then fprint oc "clear_select_cache (); ";
      fprintl oc "merge_created (decode ())"
    end;
  end;

  if go.go_select then begin
    fprint  oc "    let select";
    List.iter (fun (cn, ct) -> fprint oc " ?"; fprint oc cn) ti.ti_cts;
    fprintl oc " () =";
    if go.go_select_cache then begin
      fprint oc "      let args = ";
      List.iteri
	(fun i (cn, _) ->
	  if i <> 0 then fprint oc ", ";
	  fprint oc cn)
	ti.ti_cts;
      fprintl oc " in";
      fprintl oc "      try Lwt.return (Prime_cache.find select_cache args) \
			with Not_found ->";
    end;
    emit_use_C oc 6;
    fprintl  oc "\tlet module Sb = Select_buffer (C) in";
    fprintlf oc "\tlet sb = Sb.create C.backend_info \"%s\" in"
	     (Episql.string_of_qname ti.ti_tqn);
    let emit_ret (cn, _) = fprintlf oc "\tSb.ret sb \"%s\";" cn in
    List.iter emit_ret ti.ti_pk_cts;
    List.iter emit_ret ti.ti_nonpk_cts;
    List.iter
      (fun (cn, ct) ->
	let conv = convname_of_datatype ct.ct_type in
	fprintlf oc "\tbegin match %s with" cn;
	fprintl  oc "\t| None -> ()";
	if ct.ct_nullable then
	  fprintlf oc "\t| Some `Null -> Sb.(where sb [S\"%s IS NULL\"])" cn;
	let mk_binary (on, op) =
	  fprintlf oc "\t| Some (`%s x) -> \
			   Sb.(where sb [S\"%s %s \"; P C.Param.(%s x)])"
		      on cn op conv in
	let mk_ternary (on, op) =
	  fprintlf oc "\t| Some (`%s (x, y)) -> \
			 Sb.(where sb [S\"%s %s \"; P C.Param.(%s x); \
				       S\" AND \"; P C.Param.(%s y)])"
		      on cn op conv conv in
	List.iter mk_binary
	  ["Eq", "="; "Ne", "<>"; "Lt", "<"; "Le", "<="; "Ge", ">="; "Gt", ">"];
	List.iter mk_ternary
	  ["Between", "BETWEEN"; "Not_between", "NOT BETWEEN"];
	if ct.ct_type = `Text then
	  List.iter mk_binary ["Like", "LIKE"; "Ilike", "ILIKE"];
	fprintl  oc "\tend;")
      ti.ti_cts;
    fprintl  oc "\tlet q, p = Sb.contents sb in";
    fprintl  oc "\tlet decode t acc =";
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      fprintlf oc "\t  let key = C.Tuple.(%s 0 t) in"
	       (convname_of_coltype (snd (List.hd ti.ti_cts)))
    else begin
      fprint   oc "\t  let key = C.Tuple.({";
      List.iteri
	(fun i (cn, ct) ->
	  if i > 0 then fprint oc "; ";
	  fprintf oc "%s%s = %s %d t"
		  go.go_pk_prefix cn (convname_of_coltype ct) i)
	ti.ti_pk_cts;
      fprintl oc "}) in"
    end;
    if ti.ti_nonpk_cts = [] then
      fprintl oc "\t  let state = () in"
    else begin
      let n_pk = List.length ti.ti_pk_cts in
      fprint  oc "\t  let state = C.Tuple.({";
      List.iteri
	(fun i (cn, ct) ->
	  if i > 0 then fprint oc "; ";
	  fprintf oc "%s%s = %s %d t"
		  go.go_state_prefix cn (convname_of_coltype ct) (i + n_pk))
	ti.ti_nonpk_cts;
      fprintl oc "}) in"
    end;
    fprintl oc "\t  merge (key, Present state) :: acc in";
    if go.go_select_cache then begin
      fprintl  oc "\tC.fold q decode p [] >|= fun r ->";
      fprintlf oc "\tlet g = !select_grade (List.length r * %d + %d) in"
		  (List.length ti.ti_nonpk_cts) (List.length ti.ti_cts + 2);
      fprintl  oc "\tPrime_cache.replace select_cache g args r; r"
    end else
      fprintl oc "\tC.fold q decode p []"
  end;

  if (go.go_update || go.go_patch) && ti.ti_nonpk_cts <> [] then begin
    fprint  oc "    let update ";
    List.iter (fun (cn, ct) -> fprintf oc "?%s " cn) ti.ti_nonpk_cts;
    fprintl oc "o =";
    fprintl oc "      match o.state with";
    fprintl oc "      | Inserting _ -> Lwt.fail (Conflict `Update_insert)";
    fprintl oc "      | Deleting _ -> Lwt.fail (Conflict `Update_delete)";
    fprintl oc "      | Absent ->";
    if go.go_insert_upserts then begin
      List.iter
	(fun (cn, ct) ->
	  if ct.ct_nullable then
	    fprintlf oc "\tlet %s = match %s with None -> None | Some x -> x in"
		     cn cn)
	ti.ti_nonpk_nonreq_cts;
      if ti.ti_nonpk_req_cts = [] then begin
	fprint  oc "\tinsert";
	List.iter (fun (cn, _) -> fprintf oc " ?%s" cn) ti.ti_nonpk_cts;
	fprintl oc " o"
      end else begin
	fprint  oc "\tbegin match ";
	List.iteri
	  (fun i (cn, _) -> if i > 0 then fprint oc ", "; fprint oc cn)
	  ti.ti_nonpk_req_cts;
	fprintl oc " with";
	fprint  oc "\t| ";
	List.iteri
	  (fun i (cn, _) -> if i > 0 then fprint oc ", "; fprintf oc "Some %s" cn)
	  ti.ti_nonpk_req_cts;
	fprintl oc " ->";
	fprint  oc "\t  insert";
	List.iter
	  (fun (cn, ct) ->
	    fprintf oc " %c%s" (if coltype_is_required ct then '~' else '?') cn)
	  ti.ti_nonpk_cts;
	fprintl oc " o";
	fprintl oc "\t| _ ->";
	fprintl oc "\t  Lwt.fail (Failure \"Attempt to update an absent row \
					with insufficient data to insert.\")";
	fprintl oc "\tend";
      end
    end else
      fprintl oc "\tLwt.fail (Failure \"Update of absent row.\")";
    fprint  oc "      | Present state -> ";
    emit_use_C oc 0;
    fprintl  oc "\tlet module Ub = Update_buffer (C) in";
    fprintlf oc "\tlet ub = Ub.create C.backend_info \"%s\" in"
	     (Episql.string_of_qname ti.ti_tqn);
    if go.go_event then
      fprintl oc "\tlet changes = ref [] in";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "\tbegin match %s with" cn;
	fprintlf oc "\t| Some x when x <> state.%s%s ->" go.go_state_prefix cn;
	fprintlf oc "\t  Ub.set ub \"%s\" C.Param.(%s x);"
		 cn (convname_of_coltype ct);
	if go.go_event then
	  fprintlf oc "\t  changes := `Set_%s x :: !changes;" cn;
	fprintl  oc "\t| _ -> ()";
	fprintl  oc "\tend;")
      ti.ti_nonpk_cts;
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      let (cn, ct) = List.hd ti.ti_pk_cts in
      fprintlf oc "\tUb.where ub \"%s\" C.Param.(%s o.key);"
	       cn (convname_of_coltype ct)
    else
      List.iter
	(fun (cn, ct) ->
	  fprintlf oc "\tUb.where ub \"%s\" C.Param.(%s o.key.%s%s);"
		   cn (convname_of_coltype ct) go.go_pk_prefix cn)
	ti.ti_pk_cts;
    fprintl oc "\tbegin match Ub.contents ub with";
    fprintl oc "\t| None -> Lwt.return_unit";
    fprintl oc "\t| Some (q, params) ->";
    fprintl oc "\t  C.exec q params >|= fun () ->";
    if go.go_select_cache then fprintl oc "\t  clear_select_cache ();";
    List.iteri
      (fun i (cn, _) ->
	if i > 0 then fprint oc ";\n";
	fprintf oc "\t  (match %s with None -> () | Some v -> state.%s%s <- v)"
		cn go.go_state_prefix cn)
      ti.ti_nonpk_cts;
    if go.go_event then
      fprint oc ";\n\t  o.notify (`Update (List.rev !changes))";
    fprintl oc "\n\tend"
  end;

  if go.go_delete || go.go_patch then begin
    fprintl oc "    let delete ({key} as o) =";
    emit_use_C oc 6;
    fprintl oc "      let rec retry () =";
    fprintl oc "\tmatch o.state with";
    fprintl oc "\t| Absent -> Lwt.return_unit";
    fprintl oc "\t| Inserting c -> Lwt_condition.wait c >>= fun _ -> retry ()";
    fprintl oc "\t| Present _ ->";
    fprintl oc "\t  let c = Lwt_condition.create () in";
    fprintl oc "\t  o.state <- Deleting c;";
    fprint  oc "\t  C.exec Q.delete ";
    emit_param oc ti "key" ti.ti_pk_cts; fprintl oc " >|= fun () ->";
    if go.go_select_cache then fprintl oc "\t  clear_select_cache ();";
    fprintl oc "\t  o.state <- Absent; o.notify `Delete";
    fprintl oc "\t| Deleting c -> Lwt_condition.wait c in";
    fprintl oc "      retry ()"
  end;

  if go.go_patch then begin
    fprintl oc "    let patch o p =";
    fprintl oc "      match p with";
    if ti.ti_nonpk_cts = [] then begin
      fprintl oc "      | `Insert ((), ()) -> insert o";
      fprintl oc "      | `Update [] -> Lwt.return_unit";
      fprintl oc "      | `Update (x :: _) -> Prime.absurd x"
    end else begin
      fprintl oc "      | `Insert (r, d) ->";
      fprint  oc "\tinsert";
      List.iter
	(fun (cn, ct) -> fprintf oc " ~%s:r.%s%s" cn go.go_value_r_prefix cn)
	ti.ti_nonpk_req_cts;
      List.iter
	(fun (cn, ct) -> fprintf oc " ?%s:d.%s%s" cn go.go_value_d_prefix cn)
	ti.ti_nonpk_nonreq_cts;
      fprintl oc " o;";
      fprintl oc "      | `Update changes ->";
      List.iter
	(fun (cn, _) -> fprintlf oc "\tlet %s = ref None in" cn)
	ti.ti_nonpk_cts;
      fprintl oc "\tList.iter";
      fprint  oc "\t  (function";
      List.iter
	(fun (cn, _) ->
	  fprintf oc "\n\t    | `Set_%s v -> %s := Some v" cn cn)
	ti.ti_nonpk_cts;
      fprintl oc ")";
      fprintl oc "\t  changes;";
      fprint  oc "\tupdate";
      List.iter (fun (cn, _) -> fprintf oc " ?%s:!%s" cn cn) ti.ti_nonpk_cts;
      fprintl oc " o";
    end;
    fprintl oc "      | `Delete -> delete o"
  end;

  if go.go_event then fprintl oc "    let patches {patches} = patches";

  if go.go_value then begin
    fprintl oc "    let value o =";
    fprintl oc "      match o.state with";
    fprintl oc "      | Present state ->";
    if ti.ti_nonpk_cts = [] then begin
      if go.go_raise_on_absent then
	fprintl oc "\t()"
      else
	fprintl oc "\tSome ()"
    end else begin
      if go.go_raise_on_absent then
	fprintl oc "\t{"
      else
	fprintl oc "\tSome {";
      List.iter
	(fun (cn, _) ->
	  fprintlf oc "\t  %s%s = state.%s%s;"
		   go.go_value_prefix cn go.go_state_prefix cn)
	ti.ti_nonpk_cts;
      fprintl oc "\t}"
    end;
    if go.go_raise_on_absent then
      fprintl oc "      | _ -> raise Not_present"
    else
      fprintl oc "      | _ -> None"
  end;

  fprintl oc "  end"

let generate emit stmts oc =
  let emit_top = function
    | Create_schema _ | Create_sequence _ | Create_enum _
    | Drop_schema _ | Drop_table _ | Drop_sequence _ | Drop_type _ -> ()
    | Create_table (ti_tqn, items) ->
      let pk_opt, cts = List.fold_right collect items (None, []) in
      begin match pk_opt with
      | None -> ()
      | Some pk ->
	let set_pk = function
	  | cn, {ct_pk = true} as cn_ct -> cn_ct
	  | cn, ct ->
	    let ct_pk = List.mem cn pk in
	    let ct_nullable = ct.ct_nullable && not ct_pk in
	    cn, {ct with ct_pk; ct_nullable} in
	let ti_cts = List.map set_pk cts in
	let ti_req_cts =
	  List.filter
	    (fun (_, ct) -> not ct.ct_nullable && not ct.ct_defaultable)
	    ti_cts in
	let ti_pk_cts = List.filter (fun (_, {ct_pk}) -> ct_pk) ti_cts in
	let ti_nonpk_cts = List.filter (fun (_, {ct_pk}) -> not ct_pk) ti_cts in
	let ti_nonpk_def_cts =
	  List.filter (fun (_, ct) -> ct.ct_defaultable) ti_nonpk_cts in
	let ti_nonpk_req_cts =
	  List.filter (fun (_, ct) -> coltype_is_required ct) ti_nonpk_cts in
	let ti_nonpk_nonreq_cts =
	  List.filter (fun (_, ct) -> not (coltype_is_required ct))
		      ti_nonpk_cts in
	let ti_pk_has_default =
	  List.exists (fun (_, ct) -> ct.ct_defaultable) ti_pk_cts in
	let ti = {
	  ti_tqn;
	  ti_cts;
	  ti_req_cts;
	  ti_pk_cts;
	  ti_nonpk_cts;
	  ti_nonpk_def_cts;
	  ti_nonpk_req_cts;
	  ti_nonpk_nonreq_cts;
	  ti_pk_has_default;
	} in
	emit oc ti
      end in
  List.iter emit_top stmts

let common_header = "\
  (* Generated by episql. *)\n\n\
  open Caqti_persist\n\
  module type P = sig\n\
 \  module Beacon : Prime_beacon.S\n\
 \  val use_db : ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t\n\
  end\n\n"

let generate_intf stmts oc =
  fprint  oc common_header;
  emit_custom_open oc;
  fprintl oc "module type S = sig\n";
  generate emit_intf stmts oc;
  fprintl oc "end\n\n";
  fprintl oc "module Make (P : P) : S\n"

let generate_impl stmts oc =
  fprint  oc common_header;
  fprintl oc "open Printf";
  emit_custom_open oc;
  fprintl oc "module type S = sig\n";
  generate emit_intf stmts oc;
  fprintl oc "end\n\n";
  fprintl oc "let (>>=) = Lwt.(>>=)";
  fprintl oc "let (>|=) = Lwt.(>|=)";
  fprintl oc "module Make (P : P) = struct";
  fprintl oc "  module Cache = Make_pk_cache (P.Beacon)";
  generate emit_impl stmts oc;
  fprintl oc "end"

let generate_types ~in_intf stmts oc =
  fprintl oc "(* Generated by episql. *)\n";
  emit_custom_open oc;
  fprintl oc "type ('value_r, 'value_d, 'change) persist_patch_in =";
  fprintl oc "  [ `Insert of 'value_r * 'value_d";
  fprintl oc "  | `Update of 'change list";
  fprint  oc "  | `Delete ]";
  emit_deriving_nl oc;
  fprintl oc "type ('value, 'change) persist_patch_out =";
  fprintl oc "  [ `Insert of 'value";
  fprintl oc "  | `Update of 'change list";
  fprint  oc "  | `Delete ]";
  emit_deriving_nl oc;
  fprintl oc "";
  generate (emit_types ~in_intf) stmts oc

let () =
  let set_types_module mn =
    go.go_types_module <-
      Some (String.capitalize (if Filename.check_suffix mn ".mli"
			       then Filename.chop_suffix mn ".mli"
			       else mn)) in
  let common_arg_specs = [
    "-deriving", Arg.String (fun c -> go.go_deriving <- c :: go.go_deriving),
      "CLASS Add deriving (CLASS) to type definitions. \
	     Only supported with separate types module. \
	     The -use-type* and -open flags are useful for supplementing \
	     suitable definitions for missing classes.";
    "-open", Arg.String (fun m -> go.go_open <- m :: go.go_open),
      "M Open M at top of the generated files but after other open statements.";
    "-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- true),
      "Raise Not_present instead of returning options for state, value, and \
       getters. The exception is also raised for getters of nullable fields \
       for consistency, even though they return options.";
    "-no-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- false),
      "Inversion of -raise-on-absent and the default for now.";
    "-with-type-counit", Arg.String (fun s -> go.go_type_counit <- s),
      "T Use T as the Prime.counit type.";
    "-with-type-date", Arg.String (fun s -> go.go_type_date <- s),
      "T Use T as the CalendarLib.Date.t type.";
    "-with-type-timestamp", Arg.String (fun s -> go.go_type_timestamp <- s),
      "T Use T as the CalendarLib.Calendar.t type.";
    "-with-types-from",
      Arg.String (fun m -> go.go_type_counit <- m ^ ".counit";
			   go.go_type_date <- m ^ ".date";
			   go.go_type_timestamp <- m ^ ".timestamp"),
      "M Shortcut for passing M.counit, M.date, and M.timestamp to the other \
	 -with-type-* options.";
  ] in
  let arg_specs = [
    "-t", Arg.String set_types_module,
      "M Assume M contains corresponding types generated with \
	 -g caqti-persist-types-*.";
  ] @ common_arg_specs in
  let types_arg_specs = common_arg_specs in
  Episql.register_generator ~arg_specs "caqti-persist-mli" generate_intf;
  Episql.register_generator ~arg_specs "caqti-persist-ml" generate_impl;
  Episql.register_generator ~arg_specs:types_arg_specs "caqti-persist-types-mli"
			    (generate_types ~in_intf:true);
  Episql.register_generator ~arg_specs:types_arg_specs "caqti-persist-types-ml"
			    (generate_types ~in_intf:false)
