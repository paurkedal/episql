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
  | `Date -> "CalendarLib.Date.t"
  | `Timestamp -> "CalendarLib.Calendar.t"
  | `Timestamp_with_timezone -> "CalendarLib.Calendar.t"
  | `Interval -> "string" (* FIXME *)
  | `Custom _ -> "string"

type coltype = {
  ct_type : datatype;
  ct_pk : bool;
  ct_nullable : bool;
  ct_defaultable : bool;
}

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
  ti_nonpkreq_count : int;
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

type genopts = {
  go_event : bool;
  go_patch : bool;
  go_insert : bool;
  go_create : bool;
  go_update : bool;
  go_delete : bool;
}
let go = {
  go_event = true;
  go_patch = true;
  go_insert = true;
  go_create = true;
  go_update = true;
  go_delete = true;
}

let emit_difftypes oc ti =
  let len_nonpk = List.length ti.ti_nonpk_cts in
  if ti.ti_nonpk_cts = [] then begin
    fprintl oc "    type change = empty";
    fprintl oc "    type req = unit"
  end else begin
    fprintl oc "    type change =";
    List.iteri
      (fun i (cn, ct) ->
	fprintlf oc "      %c `Set_%s of %s%s"
		 (if i = 0 then '[' else '|') cn (string_of_coltype ct)
		 (if i = len_nonpk - 1 then " ]" else ""))
      ti.ti_nonpk_cts;
    if ti.ti_req_cts = [] then
      fprintl oc "    type req = unit"
    else begin
      fprintl oc "    type req = {";
      List.iter
	(fun (cn, ct) ->
	  fprintlf oc "      req_%s : %s;" cn (string_of_datatype ct.ct_type))
	ti.ti_req_cts;
      fprintl oc "    }"
    end
  end

let emit_intf oc ti =
  fprintf oc "  module %s : sig\n" (String.capitalize (snd ti.ti_tqn));
  fprintl oc "    type pk = {";
  List.iter
    (fun (cn, ct) ->
      fprintlf oc "      %s : %s;" cn (string_of_coltype ct))
    ti.ti_pk_cts;
  fprintl oc "    }";
  if ti.ti_nonpk_cts = [] then
    fprintl oc "    type nonpk = unit"
  else begin
    fprintl oc "    type nonpk = private {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      mutable %s : %s;" cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    fprintl oc "    }"
  end;
  emit_difftypes oc ti;
  fprintl oc "    type t";
  fprintl oc "    val make : pk -> t Lwt.t";
  fprintl oc "    val get_pk : t -> pk";
  fprintl oc "    val get_nonpk : t -> nonpk option";
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
    fprint  oc "    val insert :";
    List.iter
      (fun (cn, ct) ->
	if not ct.ct_nullable && not ct.ct_defaultable then
	  fprintf oc "\n      %s: %s ->" cn (string_of_datatype ct.ct_type))
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
    fprintl oc "    val patch : t -> (req, change) patch -> unit Lwt.t";
  if go.go_event then
    fprintl oc "    val patches : t -> (req, change) patch React.E.t";
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

let emit_param oc pk_pfx pfx cts =
  fprint oc "C.Param.([|";
  List.iteri
    (fun i (cn, ct) ->
      if i > 0 then fprint oc "; ";
      fprintf oc "%s %s%s" (convname_of_coltype ct)
	      (if ct.ct_pk then pk_pfx else pfx) cn)
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
	fprintf oc "%s = %s %d t" cn (convname_of_coltype ct) i)
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

  let emit_insert next_param =
    let tqn = Episql.string_of_qname ti.ti_tqn in
    if ti.ti_req_cts = [] then
      fprintf oc "INSERT INTO %s DEFAULT VALUES" tqn
    else
      fprintf oc "INSERT INTO %s (%s) VALUES (%s)" tqn
	(String.concat ", " (List.map (fun (cn, _) -> cn) ti.ti_req_cts))
	(String.concat ", " (List.map (fun (cn, _) -> next_param ())
				      ti.ti_req_cts));
    if ti.ti_nonpk_def_cts <> [] then
      let def_cns = List.map fst ti.ti_nonpk_def_cts in
      fprint oc (" RETURNING " ^ String.concat ", " def_cns) in

  let emit_delete next_param =
    fprintf oc "DELETE FROM %s WHERE " (Episql.string_of_qname ti.ti_tqn);
    emit_pk_cond next_param in

(*
  let emit_set cn next_param =
    fprintf oc "UPDATE %s SET %s = %s WHERE "
      (Episql.string_of_qname ti.ti_tqn) cn (next_param ());
    emit_pk_cond next_param in
*)

  fprintf oc "  module %s = struct\n" (String.capitalize (snd ti.ti_tqn));
  fprintl oc "    module Q = struct";
  emit_query oc "fetch" emit_fetch;
  if go.go_insert || go.go_patch then
    emit_query oc "insert" emit_insert;
  emit_query oc "delete" emit_delete;
(*
  List.iter (fun (cn, _) -> emit_query oc ("set_" ^ cn) (emit_set cn))
	    ti.ti_nonpk_cts;
*)
  fprintl oc "    end";
  fprintl oc "    type pk = {";
  List.iter
    (fun (cn, ct) ->
      fprintlf oc "      %s : %s;" cn (string_of_datatype ct.ct_type))
    ti.ti_pk_cts;
  fprintl oc "    }";
  if ti.ti_nonpk_cts = [] then
    fprintl oc "    type nonpk = unit"
  else begin
    fprintl oc "    type nonpk = {";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "      mutable %s : %s;" cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    fprintl oc "    }"
  end;
  emit_difftypes oc ti;
  fprintl oc "    include Cache (struct";
  fprintl oc "      type _t0 = pk\ttype pk = _t0";
  fprintl oc "      type _t1 = nonpk\ttype nonpk = _t1";
  fprintl oc "      type _t2 = req\ttype req = _t2";
  fprintl oc "      type _t3 = change\ttype change = _t3";
  fprintl oc "      let fetch pk =";
  emit_use_C oc 8;
  fprint  oc "\tC.find Q.fetch ";
  emit_detuple oc ti.ti_nonpk_cts; fprint oc " ";
  emit_param oc "pk." "" ti.ti_pk_cts; fprintl oc "";
  fprintl oc "    end)";

  fprintl oc "    let get_pk {pk} = pk";
  fprintl oc "    let get_nonpk = \
		    function {nonpk = Present x} -> Some x | _ -> None";

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
    fprint oc "\t  {";
    let emit_field i (cn, ct) = if i > 0 then fprint oc "; "; fprint oc cn in
    List.iteri emit_field ti.ti_pk_cts;
    fprint oc "},";
    if ti.ti_nonpk_cts = [] then
      fprintl oc " () in"
    else begin
      fprint oc "\n\t  {";
      List.iteri emit_field ti.ti_nonpk_cts;
      fprintl oc "} in"
    end;
    if have_default then begin
      fprintl oc "\tC.find q decode p >>=";
      fprintl oc "\tfunction Some (pk, nonpk) -> merge_present (pk, nonpk)";
      fprintl oc "\t       | None -> assert false"
    end else
      fprintl oc "\tC.exec q p >>= fun () -> merge_present (decode ())";
  end;

  if go.go_insert || go.go_patch then begin
    fprint  oc "    let insert";
    let req_cts =
      List.filter (fun (_, ct) -> not ct.ct_nullable && not ct.ct_defaultable)
		  ti.ti_nonpk_cts in
    List.iter (fun (cn, ct) -> fprintf oc " ~%s" cn) req_cts;
    fprintl oc " o =\n      let rec retry () =";
    fprintl oc "\tmatch o.nonpk with";
    fprintl oc "\t| Absent ->";
    fprintl oc "\t  let c = Lwt_condition.create () in";
    fprintl oc "\t  o.nonpk <- Inserting c;";
    emit_use_C oc 10;
    if ti.ti_nonpk_def_cts <> [] then begin
      fprint oc "\t  C.find Q.insert C.Tuple.(fun t -> (";
      List.iteri
	(fun i (cn, ct) ->
	  if i > 0 then fprint oc ", ";
	  fprintf oc "%s %d t" (convname_of_datatype ct.ct_type) i)
	ti.ti_nonpk_def_cts;
      fprint oc ")) ";
      emit_param oc "o.pk." "" ti.ti_req_cts;
      fprintl oc " >>= fun df ->";
      fprint  oc "\t  let ";
      List.iteri (fun i (cn,_) -> if i > 0 then fprint oc ", "; fprint oc cn)
		 ti.ti_nonpk_def_cts;
      fprintl oc " = match df with Some x -> x | None -> assert false in"
    end else begin
      fprint  oc "\t  C.exec Q.insert ";
      emit_param oc "o.pk." "" ti.ti_req_cts; fprintl oc " >>= fun () ->"
    end;
    if ti.ti_nonpk_cts = [] then begin
      fprintl oc "\t  o.nonpk <- Present ();";
      fprintl oc "\t  Lwt_condition.broadcast c ();"
    end else begin
      fprint  oc "\t  let nonpk = {";
      List.iteri
	(fun i (cn, ct) ->
	  if i > 0 then fprint oc "; ";
	  fprint oc cn;
	  if ct.ct_nullable then
	    fprint oc (if ct.ct_defaultable then " = Some "^cn else " = None"))
	ti.ti_nonpk_cts;
      fprintl oc "} in";
      fprintl oc "\t  o.nonpk <- Present nonpk;";
      fprintl oc "\t  Lwt_condition.broadcast c nonpk;"
    end;
    if req_cts = [] then begin
      if go.go_event then
	fprintl oc "\t  o.notify (Insert ((), []));"
    end else begin
      if go.go_event then begin
	fprint oc "\t  o.notify (Insert ({";
	List.iteri
	  (fun i (cn, ct) ->
	    fprint oc (if i = 0 then "req_" else "; req_");
	    fprint oc cn; fprint oc " = ";
	    if ct.ct_pk then fprint oc "o.pk.";
	    fprint oc cn)
	  ti.ti_req_cts;
	fprintl oc "}, []));"
      end
    end;
    fprintl oc "\t  Lwt.return_unit";
    fprintl oc "\t| Inserting c -> Lwt_condition.wait c >|= fun _ -> ()";
    fprintl oc "\t| Present x -> Lwt.return_unit";
    fprint  oc "\t| Deleting c -> Lwt_condition.wait c >>= retry";
    fprintl oc " in\n      retry ()"
  end;

  if (go.go_update || go.go_patch) && ti.ti_nonpk_cts <> [] then begin
    fprint  oc "    let update ";
    List.iter (fun (cn, ct) -> fprintf oc "?%s " cn) ti.ti_nonpk_cts;
    fprintl oc "o =";
    fprintl oc "      match get_nonpk o with";
    fprintl oc "      | None -> Lwt.fail (Failure \"Update of absent row.\")";
    fprint  oc "      | Some nonpk -> ";
    emit_use_C oc 0;
    fprintl  oc "\tlet module Ub = Update_buffer (C) in";
    fprintlf oc "\tlet ub = Ub.create C.backend_info \"%s\" in"
	     (Episql.string_of_qname ti.ti_tqn);
    if go.go_event then
      fprintl oc "\tlet changes = ref [] in";
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "\tbegin match %s with" cn;
	fprintlf oc "\t| Some x when x <> nonpk.%s ->" cn;
	fprintlf oc "\t  Ub.set ub \"%s\" C.Param.(%s x);"
		 cn (convname_of_coltype ct);
	if go.go_event then
	  fprintlf oc "\t  changes := `Set_%s x :: !changes;" cn;
	fprintl  oc "\t| _ -> ()";
	fprintl  oc "\tend;")
      ti.ti_nonpk_cts;
    List.iter
      (fun (cn, ct) ->
	fprintlf oc "\tUb.where ub \"%s\" C.Param.(%s o.pk.%s);"
		 cn (convname_of_coltype ct) cn)
      ti.ti_pk_cts;
    fprintl oc "\tbegin match Ub.contents ub with";
    fprintl oc "\t| None -> Lwt.return_unit";
    fprintl oc "\t| Some (q, params) ->";
    fprintl oc "\t  C.exec q params >|= fun () ->";
    List.iteri
      (fun i (cn, _) ->
	if i > 0 then fprint oc ";\n";
	fprintf oc "\t  (match %s with None -> () | Some v -> nonpk.%s <- v)"
		cn cn)
      ti.ti_nonpk_cts;
    if go.go_event then
      fprint oc ";\n\t  o.notify (Update (List.rev !changes))";
    fprintl oc "\n\tend"
  end;

  if go.go_delete || go.go_patch then begin
    fprintl oc "    let delete ({pk} as o) =";
    emit_use_C oc 6;
    fprintl oc "      let rec retry () =";
    fprintl oc "\tmatch o.nonpk with";
    fprintl oc "\t| Absent -> Lwt.return_unit";
    fprintl oc "\t| Inserting c -> Lwt_condition.wait c >>= fun _ -> retry ()";
    fprintl oc "\t| Present _ ->";
    fprintl oc "\t  let c = Lwt_condition.create () in";
    fprintl oc "\t  o.nonpk <- Deleting c;";
    fprint  oc "\t  C.exec Q.delete ";
    emit_param oc "pk." "" ti.ti_pk_cts; fprintl oc " >|=";
    fprintl oc "\t  fun () -> o.nonpk <- Absent; o.notify Delete";
    fprintl oc "\t| Deleting c -> Lwt_condition.wait c in";
    fprintl oc "      retry ()"
  end;

  if go.go_patch then begin
    fprintl oc "    let patch o p =";
    fprintl oc "      match p with";
    if ti.ti_nonpk_cts = [] then begin
      fprintl oc "      | Insert ((), []) -> insert o";
      fprintl oc "      | Update [] -> Lwt.return_unit";
      fprintl oc "      | Insert ((), _) | Update _ -> assert false"
    end else begin
      fprintl oc "      | Insert (req, []) ->";
      fprint  oc "\tinsert";
      List.iter
	(fun (cn, ct) ->
	  if not ct.ct_nullable && not ct.ct_defaultable then
	    fprintf oc " ~%s:req.req_%s" cn cn)
	ti.ti_nonpk_cts;
      fprintl oc " o;";
      fprintl oc "      | Insert (req, _) -> assert false (* FIXME *)";
      fprintl oc "      | Update changes ->";
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
    fprintl oc "      | Delete -> delete o"
  end;

  if go.go_event then fprintl oc "    let patches {patches} = patches";

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
	  | cn, ct -> cn, {ct with ct_pk = List.mem cn pk} in
	let ti_cts = List.map set_pk cts in
	let ti_req_cts =
	  List.filter
	    (fun (_, ct) -> not ct.ct_nullable && not ct.ct_defaultable)
	    ti_cts in
	let ti_pk_cts = List.filter (fun (_, {ct_pk}) -> ct_pk) ti_cts in
	let ti_nonpk_cts = List.filter (fun (_, {ct_pk}) -> not ct_pk) ti_cts in
	let ti_nonpk_def_cts =
	  List.filter (fun (_, {ct_defaultable}) -> ct_defaultable)
		      ti_nonpk_cts in
	let is_nullable (_, {ct_nullable}) = ct_nullable in
	let pk_count = List.length pk in
	let nonpk_count = List.length ti_cts - pk_count in
	let nullable_count = List.count is_nullable ti_cts in
	let ti_nonpkreq_count = nonpk_count - nullable_count in
	let ti_pk_has_default =
	  List.exists (fun (_, ct) -> ct.ct_defaultable) ti_pk_cts in
	let ti = {
	  ti_tqn;
	  ti_cts;
	  ti_req_cts;
	  ti_pk_cts;
	  ti_nonpk_cts;
	  ti_nonpk_def_cts;
	  ti_nonpkreq_count;
	  ti_pk_has_default;
	} in
	emit oc ti
      end in
  List.iter emit_top stmts

let common_header = "\
  (* Generated by episql. *)\n\n\
  open Epicaqti_persist\n\
  module type P = sig\n\
 \  module Beacon : Prime_beacon.S\n\
 \  val use_db : ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t\n\
  end\n\n"

let generate_intf stmts oc =
  fprint  oc common_header;
  fprintl oc "module Make (P : P) : sig";
  generate emit_intf stmts oc;
  fprintl oc "end"

let generate_impl stmts oc =
  fprint  oc common_header;
  fprintl oc "open Printf";
  fprintl oc "let (>>=) = Lwt.(>>=)";
  fprintl oc "let (>|=) = Lwt.(>|=)";
  fprintl oc "module Make (P : P) = struct";
  fprintl oc "  module Cache = Make_pk_cache (P.Beacon)";
  generate emit_impl stmts oc;
  fprintl oc "end"

let () =
  Episql.register_generator "caqti-persist-mli" generate_intf;
  Episql.register_generator "caqti-persist-ml" generate_impl
