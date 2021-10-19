(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Episql.Types
open Format
open Unprime_list
open Unprime_option
open Unprime_string

module Filter = struct
  type instr =
    | Incl of Re.re
    | Excl of Re.re

  type t = instr list

  let instr_of_string s =
    let s = String.trim s in
    if String.length s = 0 then invalid_arg "Filter.instr_of_string";
    (match s.[0] with
     | '+' -> Incl (Re.compile (Re.Glob.glob (String.(trim (slice_from 1 s)))))
     | '-' -> Excl (Re.compile (Re.Glob.glob (String.(trim (slice_from 1 s)))))
     | _ ->   Incl (Re.compile (Re.Glob.glob s)))

  let of_string s = String.chop_affix "," s |> List.map instr_of_string

  let rec test flt s =
    (match flt with
     | [] -> None
     | Incl re :: _ when Re.execp re s -> Some true
     | Excl re :: _ when Re.execp re s -> Some false
     | _ :: flt -> test flt s)

  let test_qname ?(default = true) flt = function
   | (None, s) ->
      test flt s |> Option.get_or default
   | (Some ns, s) ->
      test flt (ns ^ "." ^ s) |> Option.get_else @@ fun () ->
      test flt s |> Option.get_or default
end

type genopts = {
  mutable go_types_module : string option;
  mutable go_filter_tables : Filter.t;
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
  mutable go_pk_module : string option;
  mutable go_pk_prefix : string;
  mutable go_state_prefix : string;
  mutable go_value_prefix : string;
  mutable go_value_r_prefix : string;
  mutable go_value_d_prefix : string;
  mutable go_use_ppx : bool;
  mutable go_deriving : string list;
  mutable go_open : string list;
  mutable go_type_counit : string;
  mutable go_type_date : string;
  mutable go_type_timestamp : string;
  mutable go_raise_on_absent : bool;
  mutable go_return_result : bool;
  mutable go_log_debug : string option;
  mutable go_connection_arg : string option;
  mutable go_public_state : bool;
}
let go = {
  go_types_module = None;
  go_filter_tables = [];
  go_event = false;
  go_patch = false;
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
  go_pk_module = None;
  go_pk_prefix = "k_";
  go_state_prefix = "s_";
  go_value_prefix = "v_";
  go_value_r_prefix = "r_";
  go_value_d_prefix = "d_";
  go_use_ppx = true;
  go_deriving = [];
  go_open = [];
  go_type_counit = "Prime.counit";
  go_type_date = "CalendarLib.Date.t";
  go_type_timestamp = "CalendarLib.Calendar.t";
  go_raise_on_absent = false;
  go_return_result = false;
  go_log_debug = Some "caqti-persist";
  go_connection_arg = None;
  go_public_state = false;
}

let convname_of_datatype = function
  | `Boolean -> "bool"
  | `Smallint | `Smallserial -> "int"
  | `Integer | `Serial -> "int32"
  | `Bigint | `Bigserial -> "int64"
  | `Real | `Double_precision -> "float"
  | `Text | `Char _ | `Varchar _ -> "string"
  | `Bytea -> "bytes"
  | `Numeric_auto | `Numeric _ -> "float" (* FIXME *)
  | `Time (_, _) -> "float"
  | `Date -> "cdate"
  | `Timestamp (_, _) -> "ctime"
  | `Interval (_, _) -> "string" (* FIXME *)
  | `Custom _ -> "string"

let string_of_datatype = function
  | `Boolean -> "bool"
  | `Smallint | `Smallserial -> "int"
  | `Integer | `Serial -> "int32"
  | `Bigint | `Bigserial -> "int64"
  | `Real | `Double_precision -> "float"
  | `Text | `Char _ | `Varchar _ -> "string"
  | `Bytea -> "string"
  | `Numeric_auto | `Numeric _ -> "float" (* FIXME *)
  | `Time (_, _) -> "float"
  | `Date -> go.go_type_date
  | `Timestamp (_, _) -> go.go_type_timestamp
  | `Interval (_, _) -> "string" (* FIXME *)
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
  if ct.ct_nullable then "(option " ^ s ^ ")" else s

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

let variant_of_colname cn = cn

let collect = function
  | Column { column_name = cn; column_type = ct_type;
             column_constraints = ccs; _ } ->
    let is_serial = function #serialtype -> true | _ -> false in
    let is_default = function (_, `Default _) -> true | _ -> false in
    let ct_defaultable = is_serial ct_type || List.exists is_default ccs in
    if List.exists (function (_, `Primary_key) -> true | _ -> false) ccs then
      (function
       | None, cts ->
          let ct =
            {ct_pk = true; ct_type; ct_nullable = false; ct_defaultable} in
          Some [cn], (cn, ct) :: cts
       | Some _, _ -> failwith "Cannot have multiple primary keys.")
    else
      let ct_nullable =
        not (List.exists (function (_, `Not_null) -> true | _ -> false) ccs) in
      let ct = {ct_pk = false; ct_type; ct_nullable; ct_defaultable} in
      fun (pk_opt, cts) -> (pk_opt, (cn, ct) :: cts)
  | Constraint (_, `Primary_key pk) ->
    begin function
    | None, cts -> Some pk, cts
    | Some _, _ -> failwith "Cannot have multiple primary keys."
    end
  | Constraint _ -> fun acc -> acc

let pp = Format.fprintf
let fprint oc s = pp_print_string oc s
let fprintf oc fmt = Printf.ksprintf (fprint oc) fmt

let emit_custom_open oc =
  List.iter (fun m -> pp oc "@ open %s" m) (List.rev go.go_open)

let emit_deriving oc =
  if go.go_deriving <> [] then begin
    let modules_str = String.concat ", " go.go_deriving in
    if go.go_use_ppx
      then fprintf oc " [@@deriving %s]" modules_str
      else fprintf oc " deriving (%s)" modules_str
  end

let emit_type_pk ~in_intf oc ti =
  match go.go_pk_module with
  | Some pkm ->
    pp oc "@ @[<v 2>module %s %s" pkm (if in_intf then ": sig" else "= struct");
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ type t = %s"
         (string_of_coltype (snd (List.hd ti.ti_pk_cts)))
    else begin
      pp oc "@ @[<v 2>type t = {";
      List.iter (fun (cn, ct) -> pp oc "@ %s : %s;" cn (string_of_coltype ct))
                ti.ti_pk_cts;
      pp oc "@]@ }";
    end;
    emit_deriving oc;
    pp oc "@]@ end"
  | None ->
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ type key = %s"
         (string_of_coltype (snd (List.hd ti.ti_pk_cts)))
    else begin
      pp oc "@ @[<v 2>type key = {";
      List.iter
        (fun (cn, ct) ->
          pp oc "@ %s%s : %s;" go.go_pk_prefix cn (string_of_coltype ct))
        ti.ti_pk_cts;
      pp oc "@]@ }"
    end;
    emit_deriving oc

let emit_type_nonpk ~in_intf oc ti =
  if ti.ti_nonpk_cts = [] then
    pp oc "@ type state = unit"
  else begin
    pp oc "@ @[<v 2>type state = ";
    pp_print_string oc
      (if in_intf && not go.go_public_state then "private {" else "{");
    List.iter
      (fun (cn, ct) ->
        pp oc "@ mutable %s%s : %s;"
              go.go_state_prefix cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    pp oc "@]@ }"
  end

let emit_type_patch_etc oc ti =
  if ti.ti_nonpk_cts = [] then begin
    pp oc "@ type value = unit";
    emit_deriving oc
  end else begin
    pp oc "@ @[<v 2>type value = {";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s : %s;" go.go_value_prefix cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    pp oc "@]@ }";
    emit_deriving oc
  end;
  if go.go_patch then begin
    if ti.ti_nonpk_req_cts = [] then begin
      pp oc "@ type value_r = unit";
      emit_deriving oc
    end else begin
      pp oc "@ @[<v 2>type value_r = {";
      List.iter
        (fun (cn, ct) ->
          pp oc "@ %s%s : %s;"
             go.go_value_r_prefix cn (string_of_datatype ct.ct_type))
        ti.ti_nonpk_req_cts;
      pp  oc "@]@ }";
      emit_deriving oc
    end;
    if ti.ti_nonpk_nonreq_cts = [] then begin
      pp oc "@ type value_d = unit";
      emit_deriving oc
    end else begin
      pp oc "@ @[<v 2>type value_d = {";
      ti.ti_nonpk_nonreq_cts |> List.iter begin fun (cn, ct) ->
        pp oc "@ %s%s : %s option%s;"
           go.go_value_d_prefix cn (string_of_datatype ct.ct_type)
           (if ct.ct_nullable && ct.ct_defaultable then " option" else "")
      end;
      pp oc "@]@ }";
      emit_deriving oc
    end
  end;
  if not go.go_event && not go.go_patch || ti.ti_nonpk_cts = [] then begin
    pp oc "@ type change = %s" go.go_type_counit;
    emit_deriving oc
  end else begin
    pp oc "@ @[<v 2>type change =@ [";
    List.iteri
      (fun i (cn, ct) ->
        if i <> 0 then pp oc "@ |";
        pp oc "`Set_%s of %s" cn (string_of_coltype ct))
      ti.ti_nonpk_cts;
    pp oc "@]@ ]";
    emit_deriving oc;
  end;
  if go.go_patch then begin
    pp oc "@ type patch_in = (value_r, value_d, change) persist_patch_in";
    emit_deriving oc
  end;
  if go.go_event then begin
    pp oc "@ type patch_out = (value, change) persist_patch_out";
    emit_deriving oc
  end

let emit_types ~in_intf oc ti =
  if in_intf then
    pp oc "@ @[<v 2>module %s : sig" (String.capitalize_ascii (snd ti.ti_tqn))
  else
    pp oc "@ @[<v 2>module %s = struct" (String.capitalize_ascii (snd ti.ti_tqn));
  emit_type_pk ~in_intf oc ti;
  emit_type_patch_etc oc ti;
  if go.go_patch then begin
    if in_intf then begin
      pp oc "@ val defaults : value_d"
    end else if ti.ti_nonpk_nonreq_cts = [] then
      pp oc "@ let defaults = ()"
    else begin
      pp oc "@ @[<v 2>let defaults = {";
      List.iter
        (fun (cn, _) -> pp oc "@ %s%s = None;" go.go_value_d_prefix cn)
        ti.ti_nonpk_nonreq_cts;
      pp oc "@]@ }"
    end
  end;
  if go.go_event then begin
    if in_intf then
      pp oc "@ val changes_of_value : value -> change list"
    else begin
      pp oc "@ @[<v 2>let changes_of_value f = [";
      List.iter
        (fun (cn, _) -> pp oc "@ `Set_%s f.%s%s;" cn go.go_value_prefix cn)
        ti.ti_nonpk_cts;
      pp oc "@]@ ]"
    end
  end;
  pp oc "@]@ end"

let emit_intf oc ti =
  let pk_type =
    (match go.go_pk_module with
     | None -> "key"
     | Some pkm -> sprintf "%s.t" pkm)
  in
  let pp_return_type oc t =
    if go.go_return_result then
      pp oc "(%s, [> Caqti_error.t]) result Lwt.t" t
    else
      pp oc "%s Lwt.t" t
  in
  pp oc "@ @[<v 2>module %s : sig" (String.capitalize_ascii (snd ti.ti_tqn));
  (match go.go_types_module with
   | None ->
      emit_type_pk ~in_intf:true oc ti;
      emit_type_patch_etc oc ti
   | Some types_module ->
      let mn = types_module ^ "." ^ String.capitalize_ascii (snd ti.ti_tqn) in
      pp oc "@ @[<v 2>include module type of %s" mn;
      pp oc "@ @[<v 1>with type %s = %s.%s" pk_type mn pk_type;
      pp oc "@ and type value = %s.value" mn;
      if go.go_patch then begin
        pp oc "@ and type value_r = %s.value_r" mn;
        pp oc "@ and type value_d = %s.value_d" mn
      end;
      if go.go_event then
        pp oc "@ and type change = %s.change" mn;
      if go.go_patch then
        pp oc "@ and type patch_in = %s.patch_in" mn;
      if go.go_event then
        pp oc "@ and type patch_out = %s.patch_out" mn;
      pp oc "@]@]");
  emit_type_nonpk ~in_intf:true oc ti;
  pp oc "@ type t";
  pp oc "@ val key : t -> %s" pk_type;
  pp oc "@ val is_present : t -> bool";
  if go.go_raise_on_absent then
    pp oc "@ val state : t -> state"
  else
    pp oc "@ val state : t -> state option";
  if go.go_value then begin
    if go.go_raise_on_absent then
      pp oc "@ val value : t -> value"
    else
      pp oc "@ val value : t -> value option"
  end;
  if go.go_getters then begin
    List.iter
      (fun (cn, ct) ->
        let is_opt = ct.ct_pk || go.go_raise_on_absent && not ct.ct_nullable in
        pp oc "@ val get_%s : t -> %s" cn (string_of_datatype ct.ct_type);
        if not is_opt then pp oc " option")
      ti.ti_cts
  end;

  let open_query_val fn =
    pp oc "@ @[<hv 2>val %s :" fn;
    Option.iter (pp oc "@ ?%s: (module Caqti_lwt.CONNECTION) ->")
                go.go_connection_arg in
  let close_query_val () = pp oc "@]" in

  (* val fetch *)
  open_query_val "fetch";
  pp oc "@ %s -> %a" pk_type pp_return_type "t";
  close_query_val ();

  (* val refetch *)
  if ti.ti_nonpk_cts <> [] then begin
    open_query_val "refetch";
    pp oc "@ t -> %a" pp_return_type "unit";
    close_query_val ()
  end;

  (* val select *)
  if go.go_select then begin
    open_query_val "select";
    List.iter
      (fun (cn, ct) ->
        let tn = string_of_datatype ct.ct_type in
        if ct.ct_type = `Text || ct.ct_nullable then
          begin
            pp oc "@ ?%s: [@[<hov 0>%s order_predicate" cn tn;
            if ct.ct_type = `Text then
              pp oc "@ | `Like of %s@ | `Ilike of %s" tn tn;
            if ct.ct_nullable then pp oc "@ | `Is_null";
            pp oc "@]] ->"
          end
        else
          pp oc "@ ?%s: %s order_predicate ->" cn tn)
      ti.ti_cts;
    pp oc "@ ?order_by: [@[<hov 0>< ";
    List.iteri
      (fun i (cn, _) ->
        if i > 0 then pp oc "@ | ";
        pp oc "`%s" (variant_of_colname cn))
      ti.ti_cts;
    pp oc "@]] order_item list ->";
    pp oc "@ ?limit: int ->@ ?offset: int ->";
    pp oc "@ unit ->@ %a" pp_return_type "t list";
    close_query_val ()
  end;

  (* val clear_cache *)
  if go.go_select_cache then
    pp oc "@ val clear_select_cache : unit -> unit";

  (* val create *)
  if go.go_create then begin
    open_query_val "create";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s: %s%s ->"
                (if ct.ct_nullable || ct.ct_defaultable then "?" else "")
                cn (string_of_datatype ct.ct_type)
                (if ct.ct_nullable && ct.ct_defaultable then " option" else ""))
      ti.ti_cts;
    if go.go_return_result then
      pp oc "@ unit -> (t, [> Caqti_persist.Error.t]) result Lwt.t"
    else
      pp oc "@ unit -> t Lwt.t";
    close_query_val ()
  end;

  (* val insert *)
  if go.go_insert then begin
    open_query_val "insert";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s: %s%s ->"
           (if ct.ct_nullable || ct.ct_defaultable then "?" else "")
           cn (string_of_datatype ct.ct_type)
           (if ct.ct_nullable && ct.ct_defaultable then " option" else ""))
      ti.ti_nonpk_cts;
    pp oc "@ t -> %a" pp_return_type "unit";
    close_query_val ()
  end;

  (* val update *)
  if go.go_update && ti.ti_nonpk_cts <> [] then begin
    open_query_val "update";
    List.iter
      (fun (cn, {ct_type = dt; ct_nullable = dn; _}) ->
        pp oc "@ ?%s: %s%s ->" cn (string_of_datatype dt)
           (if dn then " option" else ""))
      ti.ti_nonpk_cts;
    pp oc "@ t -> %a" pp_return_type "unit";
    close_query_val ()
  end;

  (* val delete *)
  if go.go_delete then begin
    open_query_val "delete";
    pp oc "@ t -> %a" pp_return_type "unit";
    close_query_val ()
  end;

  (* val uncache* *)
  pp oc "@ val uncache_key : %s -> unit" pk_type;
  pp oc "@ val uncache : t -> unit";
  pp oc "@ val uncache_all : unit -> unit";

  (* val patch *)
  if go.go_patch then begin
    open_query_val "patch";
    pp oc "@ t -> patch_in -> %a" pp_return_type "unit";
    close_query_val ()
  end;

  (* val patches *)
  if go.go_event then
    pp oc "@ val patches : t -> patch_out React.E.t";

  pp oc "@]@ end"

let emit_use_C_noarg oc =
  pp oc "@ P.use_db @@@@ fun (module C : Caqti_lwt.CONNECTION) ->"

let emit_use_C oc =
  match go.go_connection_arg with
  | None ->
    emit_use_C_noarg oc
  | Some s ->
    pp oc "@ (match %s with None -> P.use_db | Some db -> fun f -> f db)" s;
    pp oc "@   @@@@ fun (module C : Caqti_lwt.CONNECTION) ->"

let emit_columns_type ?(filter = fun _ -> true) oc cts =
  fprint oc "Type.";
  List.iter
    (fun (_, ct) ->
      if filter ct then fprintf oc "(tup2 %s " (convname_of_coltype ct))
    cts;
  fprint oc "unit";
  List.iter (fun (_, ct) -> if filter ct then fprint oc ")") cts

let emit_columns_value ?(filter = fun _ -> true) oc cts =
  List.iter (fun (cn, ct) -> if filter ct then fprintf oc "(%s, " cn) cts;
  fprint oc "()";
  List.iter (fun (_, ct) -> if filter ct then fprint oc ")") cts

let emit_returning_value ?(filter = fun _ -> true) oc cts =
  List.iter (fun (_, ct) -> if filter ct then fprint oc "(") cts;
  fprint oc "()";
  List.iter (fun (cn, ct) -> if filter ct then fprintf oc ", %s)" cn) cts

let emit_param oc ti pk cts =
  let n_pk = List.length ti.ti_pk_cts in
  cts |> List.iter begin fun (cn, ct) ->
    if go.go_collapse_pk && n_pk = 1 && ct.ct_pk then
      pp oc "(%s, " pk
    else if ct.ct_pk then
      pp oc "(%s.%s%s, " pk go.go_pk_prefix cn
    else
      pp oc "(%s, " cn
  end;
  pp oc "()";
  cts |> List.iter (fun _ -> pp oc ")")

let emit_impl oc ti =

  let open_query_let fn =
    pp oc "@ @[<v 2>let "; fprint oc fn;
    Option.iter (fprintf oc " ?%s") go.go_connection_arg in
  let close_query_let () = pp oc "@]" in

  let emit_pk_cond () =
    List.iteri
      (fun i (cn, _) -> if i > 0 then fprint oc " AND "; fprintf oc "%s = ?" cn)
      ti.ti_pk_cts
  in

  let bind_result_op, map_result_op, fail_or_map_result_op, return_ok =
    if go.go_return_result then
      begin
        pp oc "@ let (>>=?) m f = \
                   m >>= function Ok x -> f x | Error _ as r -> Lwt.return r";
        pp oc "@ let (>|=?) m f = \
                   m >|= function Ok x -> Ok (f x) | Error _ as r -> r";
        (">>=?", ">|=?", ">|=?", "Lwt.return_ok")
      end
    else
      (">>=", ">|=", ">>= Caqti_lwt.or_fail >|=", "Lwt.return")
  in

  (* module Q = *)
  pp oc "@ @[<v 2>module %s = struct" (String.capitalize_ascii (snd ti.ti_tqn));
  pp oc "@ @[<v 2>module Q = struct";

  (*   let fetch = ... *)
  pp oc "@ @[<v 2>let fetch = Caqti_request.find_opt";
  pp oc "@ "; emit_columns_type oc ti.ti_pk_cts;
  pp oc "@ "; emit_columns_type oc ti.ti_nonpk_cts;
  pp oc "@ \"";
  fprintf oc "SELECT %s FROM %s WHERE "
    (String.concat ", " (List.map (fun (cn, _) -> cn) ti.ti_nonpk_cts))
    (Episql.string_of_qname ti.ti_tqn);
  emit_pk_cond ();
  pp oc "\"@]";

  (*   let delete = ... *)
  pp oc "@ @[<v 2>let delete = Caqti_request.exec";
  pp oc "@ "; emit_columns_type oc ti.ti_pk_cts;
  pp oc "@ \"";
  fprintf oc "DELETE FROM %s WHERE " (Episql.string_of_qname ti.ti_tqn);
  emit_pk_cond ();
  pp oc "\"@]";

  (*   let insert = ... *)
  pp oc "@ @[<v 2>let insert = Ib.Request.create Ib.Spec.(";
  ti.ti_cts |> List.iter begin fun (cn, ct) ->
    pp oc "@ @[%s {cn = \"%s\"; ct = Type.%s; next =@]"
      (if ct.ct_defaultable then "Field_default" else "Field")
      cn (convname_of_coltype ct)
  end;
  pp oc "@ Done \"%s\"" (Episql.string_of_qname ti.ti_tqn);
  ti.ti_cts |> List.iter (fun _ -> fprintf oc "}");
  pp oc ")@]";

  pp oc "@]@ end";
  (* end *)

  (match go.go_types_module with
   | None ->
      emit_type_pk ~in_intf:false oc ti;
      emit_type_patch_etc oc ti
   | Some tm ->
      pp oc "@ include %s.%s" tm (String.capitalize_ascii (snd ti.ti_tqn)));
  emit_type_nonpk ~in_intf:false oc ti;

  (* include Cache (...) *)
  pp oc "@ @[<v 2>include Cache (struct";
  (match go.go_pk_module with
   | None -> pp oc "@ type _t0 = key\ttype key = _t0"
   | Some pkm -> pp oc "@ type key = %s.t" pkm);
  pp oc "@ type nonrec state = state";
  pp oc "@ type nonrec value = value";
  if go.go_event || go.go_patch then
    pp oc "@ type nonrec change = change"
  else
    pp oc "@ type nonrec change = %s" go.go_type_counit;
  if go.go_return_result then
    begin
      pp oc "@ @[<v 2>module Result_lwt = struct";
      pp oc "@ type (+'a, +'e) t = ('a, 'e) result Lwt.t";
      pp oc "@ let return_ok = Lwt.return_ok";
      pp oc "@ let map f m = \
                m >|= function Ok x -> Ok (f x) | Error _ as r -> r";
      pp oc "@ let bind_lwt f m = m >>= f";
      pp oc "@ let conflict err = Lwt.return_error (`Conflict err)";
      pp oc "@]@ end"
    end
  else
    begin
      pp oc "@ @[<v 2>module Result_lwt = struct";
      pp oc "@ type ('a, 'e) t = 'a Lwt.t";
      pp oc "@ let return_ok x = Lwt.return x";
      pp oc "@ let map f = Lwt.map f";
      pp oc "@ let bind_lwt f m = m >>= f";
      pp oc "@ let conflict err = Lwt.fail (Caqti_persist.Error.Conflict err)";
      pp oc "@]@ end"
    end;
  pp oc "@ let key_size = %d" (List.length ti.ti_pk_cts);
  pp oc "@ let state_size = %d" (List.length ti.ti_nonpk_cts);
  pp oc "@ let table_name = \"%s\"" (Episql.string_of_qname ti.ti_tqn);
  pp oc "@ @[<v 2>let fetch (module C : Caqti_lwt.CONNECTION) key =";
  pp oc "@ @[<v 2>C.find_opt Q.fetch ";
  emit_param oc ti "key" ti.ti_pk_cts;
  (match ti.ti_nonpk_cts with
   | [] ->
      if not go.go_return_result then pp oc " >>= Caqti_lwt.or_fail"
   | cts ->
      pp oc " %s function" fail_or_map_result_op;
      pp oc "@ | None -> None";
      pp oc "@ | Some ";
      emit_columns_value oc ti.ti_nonpk_cts;
      fprint oc " -> Some {";
      List.iteri
        (fun i (cn, _) ->
          if i > 0 then fprint oc "; ";
          fprintf oc "%s%s = %s" go.go_state_prefix cn cn)
        cts;
      fprint oc "}");
  pp oc "@]@]@]@ end)";
  open_query_let "fetch";
  fprintf oc " key =";
  emit_use_C oc;
  pp oc "@ fetch (module C) key";
  close_query_let ();

  (* let key = ... *)
  pp oc "@ let key {key; _} = key";

  (* let absent = ... *)
  if go.go_raise_on_absent then begin
    pp oc "@ @[<v 2>let absent op o =";
    if go.go_log_debug <> None then
      pp oc "@ Lwt_log.ign_debug_f ~section \
                \"Called %%s on absent row of %s.\" op;"
         (snd ti.ti_tqn);
    pp oc "@ raise Caqti_persist.Error.Not_present@]"
  end;

  (* let is_present = ... *)
  pp oc "@ let is_present = function {state = Present _; _} -> true \
                                   | _ -> false";

  (* let state = ... *)
  if go.go_raise_on_absent then
    pp oc "@ let state = \
               function {state = Present x; _} -> x | o -> absent \"state\" o"
  else
    pp oc "@ let state = function {state = Present x; _} -> Some x | _ -> None";

  (* let get_... = ... *)
  if go.go_getters then begin
    let n_pk = List.length ti.ti_pk_cts in
    List.iter
      (fun (cn, ct) ->
        pp oc "@ let get_%s o = " cn;
        if go.go_collapse_pk && n_pk = 1 && ct.ct_pk then
          fprint oc "o.key"
        else if ct.ct_pk then
          fprintf oc "o.key.%s%s" go.go_pk_prefix cn
        else if go.go_raise_on_absent then
          fprintf oc "match o.state with Present x -> x.%s%s \
                                        | _ -> absent \"get_%s\" o"
                  go.go_state_prefix cn cn
        else if ct.ct_nullable then
          fprintf oc "match o.state with Present x -> x.%s%s | _ -> None"
                  go.go_state_prefix cn
        else
          fprintf oc "match o.state with Present x -> Some x.%s%s | _ -> None"
                  go.go_state_prefix cn)
      ti.ti_cts
  end;

  if ti.ti_nonpk_cts <> [] then begin
    open_query_let "refetch";
    fprintf oc " ({key; _} as o) =";
    emit_use_C oc;
    pp oc "@ @[<v 2>C.find_opt Q.fetch ";
    emit_param oc ti "key" ti.ti_pk_cts;
    pp oc " %s function" fail_or_map_result_op;
    pp oc "@ | None -> o.state <- Absent";
    pp oc "@ | Some ";
    emit_columns_value oc ti.ti_nonpk_cts;
    fprint oc " -> o.state <- Present {";
    List.iteri
      (fun i (cn, _) ->
        if i > 0 then fprint oc "; ";
        fprintf oc "%s%s = %s" go.go_state_prefix cn cn)
      ti.ti_nonpk_cts;
    fprint oc "}";
    pp oc "@]";
    close_query_let ()
  end;

  (* let select_cache = ... *)
  (* let clear_select_cache = ... *)
  if go.go_select_cache then begin
    pp oc "@ let select_cache = \
               Prime_cache.create ~cache_metric:P.Beacon.cache_metric 19";
    pp oc "@ let clear_select_cache () = Prime_cache.clear select_cache"
  end;

  (* let insert = ... *)
  if go.go_insert || go.go_patch then begin
    open_query_let "insert";
    List.iter
      (fun (cn, ct) ->
        fprintf oc " %c%s"
                (if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_nonpk_cts;
    fprint oc " o =";
    pp oc "@ @[<v 1>let rec retry () = match o.state with";
    pp oc "@ @[<v 3>| Absent ->";
    pp oc "@ let _c = Lwt_condition.create () in";
    pp oc "@ o.state <- Inserting _c;";
    emit_use_C oc;

    pp oc "@ @[<v 1>(match Ib.(init Q.insert";
    ti.ti_cts |> List.iter begin fun (cn, ct) ->
      if ct.ct_pk then begin
        pp oc (if ct.ct_defaultable then " $? Some " else " $ ");
        if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
          pp oc "o.key"
        else
          pp oc "o.key.%s%s" go.go_pk_prefix cn
      end else
        pp oc " %s %s" (if ct.ct_defaultable then "$?" else "$") cn
    end;
    fprint oc ") with";
    pp oc "@ @[<v 3>| Ib.App {request = Ib.Request.Done req; \
                              param; default} ->";
    pp oc "@ C.exec req param %s default@]" fail_or_map_result_op;
    pp oc "@ @[<v 3>| Ib.App {request = Ib.Request.Done_default req; \
                              param; default} ->";
    pp oc "@ C.find req param %s default@]" fail_or_map_result_op;
    pp oc ")@]@   %s fun " map_result_op;
    emit_returning_value oc ~filter:(fun ct -> ct.ct_defaultable) ti.ti_cts;
    fprint oc " ->";
    if go.go_select_cache then pp oc "@ clear_select_cache ();";
    if ti.ti_nonpk_cts = [] then
      pp oc "@ let state = () in"
    else begin
      pp oc "@ @[<hv 2>let state = {";
      ti.ti_nonpk_cts |> List.iter begin fun (cn, _ct) ->
        pp oc "@ %s%s = %s;" go.go_state_prefix cn cn
      end;
      pp oc "@ @]} in"
    end;
    pp oc "@ o.state <- Present state;";
    pp oc "@ Lwt_condition.broadcast _c state";
    if go.go_event then begin
      fprint oc ";";
      if ti.ti_nonpk_cts = [] then
        pp oc "@ o.notify (`Insert ())"
      else begin
        pp oc "@ @[<v 2>o.notify (`Insert {";
        List.iter
          (fun (cn, _ct) ->
            pp oc "@ %s%s = state.%s%s;"
               go.go_value_prefix cn go.go_state_prefix cn)
          ti.ti_nonpk_cts;
        pp oc "@]@ })"
      end
    end;
    pp oc "@]";
    pp oc "@ | Inserting _c -> Lwt_condition.wait _c >|= fun _ -> %s"
      (if go.go_return_result then "Ok ()" else "()");
    pp oc "@ | Present x -> %s ()" return_ok;
    pp oc "@ | Deleting _c -> Lwt_condition.wait _c >>= retry in";
    pp oc "@]@ retry ()";
    close_query_let ()
  end;

  (* let create = ... *)
  if go.go_create then begin
    open_query_let "create";
    List.iter
      (fun (cn, ct) ->
        fprintf oc " %c%s"
          (if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_cts;
    fprint oc " () =";
    emit_use_C oc;

    pp oc "@ @[<v 1>(match Ib.(init Q.insert";
    ti.ti_cts |> List.iter begin fun (cn, ct) ->
      let op = if ct.ct_defaultable then "$?" else "$" in
      fprintf oc " %s %s" op cn
    end;
    fprint oc ") with";
    pp oc "@ @[<v 3>| Ib.App {request = Ib.Request.Done req; \
                              param; default} ->";
    pp oc "@ C.exec req param %s default@]" fail_or_map_result_op;
    pp oc "@ @[<v 3>| Ib.App {request = Ib.Request.Done_default req; \
                              param; default} ->";
    pp oc "@ C.find req param %s default@]" fail_or_map_result_op;
    pp oc ")@]@   %s fun " bind_result_op;
    emit_returning_value oc ~filter:(fun ct -> ct.ct_defaultable) ti.ti_cts;
    fprint oc " ->";
    if go.go_select_cache then pp oc "@ clear_select_cache ();";
    if ti.ti_nonpk_cts = [] then
      pp oc "@ let state = () in"
    else begin
      pp oc "@ @[let state = {";
      ti.ti_nonpk_cts |> List.iter begin fun (cn, _ct) ->
        pp oc "@ %s%s = %s;" go.go_state_prefix cn cn
      end;
      pp oc "@ @]} in"
    end;
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ merge_created (%s, state)" (fst (List.hd ti.ti_pk_cts))
    else begin
      pp oc "@ let pk = @[{";
      ti.ti_pk_cts |> List.iter begin fun (cn, _) ->
        pp oc "@ %s%s = %s;" go.go_pk_prefix cn cn;
      end;
      pp oc "}@] in";
      pp oc "@ merge_created (pk, state)";
    end;

    close_query_let ()
  end;

  (* let order_column_name = ... *)
  if go.go_select then begin
    let pp_variant =
      Fmt.(const string "`" ++ using variant_of_colname string)
    in
    let pp_cases pp_pat pp_res =
      let open Fmt in
      let pp_case = hbox (pp_pat ++ const string " -> " ++ pp_res) in
      list ~sep:((sp ++ const string "| ")) pp_case
    in
    pp oc "@ @[<hov 2>let order_column_name = function@ %a@]"
      Fmt.(pp_cases (using fst pp_variant) (using fst (quote string)))
      ti.ti_cts;
    pp oc "@ @[<hv 2>let select_list = [%a@]]"
      Fmt.(list ~sep:semi (using fst (quote string))) ti.ti_cts;
    pp oc "@ @[<hv 2>let select_type =@ ";
    emit_columns_type oc ti.ti_cts;
    pp oc "@]"
  end;

  (* let select = ... *)
  if go.go_select then begin
    open_query_let "select";
    List.iter (fun (cn, _ct) -> fprint oc " ?"; fprint oc cn) ti.ti_cts;
    fprint oc " ?(order_by = []) ?limit ?offset () =";
    if go.go_select_cache then begin
      pp oc "@ let args = ";
      List.iteri
        (fun i (cn, _) ->
          if i <> 0 then fprint oc ", ";
          fprint oc cn)
        ti.ti_cts;
      fprint oc " in";
      pp oc "@ try %s (Prime_cache.find select_cache args)" return_ok;
      pp oc "@ @[<v 2>with Not_found ->";
    end;
    emit_use_C oc;
    pp oc "@ let sb = Sb.create () in";
    List.iter
      (fun (cn, ct) ->
        let conv = convname_of_datatype ct.ct_type in
        pp oc "@ @[<v 1>(match %s with" cn;
        pp oc "@ | None -> ()";
        pp oc "@ | Some p -> Sb.where sb \"%s\" Type.%s p" cn conv;
        pp oc "@]);")
      ti.ti_cts;
    pp oc "@ @[<v 2>let Request (req, param) =@ \
              @[<hov 2>Sb.finish@ \
                ~table_name:%S@ ~select_list@ ~select_type@ \
                ~order_column_name@ ~order_by@ ?limit@ ?offset@ \
                sb"
      (Episql.string_of_qname ti.ti_tqn);
    pp oc "@]@]@ in";
    pp oc "@ @[<v 2>let decode ";
    emit_columns_value oc ti.ti_cts;
    pp oc " acc =";
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ let key = %s in" (fst (List.hd ti.ti_pk_cts))
    else begin
      pp oc "@ let key = {@[";
      ti.ti_pk_cts |> List.iter begin fun (cn, _) ->
        pp oc "@ %s%s = %s;" go.go_pk_prefix cn cn
      end;
      pp oc "@ @]} in"
    end;
    if ti.ti_nonpk_cts = [] then
      pp oc "@ let state = () in"
    else begin
      pp oc "@ let state = {@[";
      ti.ti_nonpk_cts |> List.iter begin fun (cn, _) ->
        pp oc "%s%s = %s;" go.go_state_prefix cn cn
      end;
      pp oc "@ @]} in"
    end;
    pp oc "@ merge (key, Present state) :: acc in";
    pp oc "@]";
    if go.go_select_cache then begin
      pp oc "@ C.fold req decode param [] %s fun r_rev ->" fail_or_map_result_op;
      pp oc "@ let r = List.rev r_rev in";
      pp oc "@ let g = !Pk_cache.select_grade (List.length r * %d + %d) in"
         (List.length ti.ti_nonpk_cts) (List.length ti.ti_cts + 2);
      pp oc "@ Prime_cache.replace select_cache g args r; r"
    end else
      pp oc "@ C.fold req decode param []";
    pp oc "@]";
    close_query_let ()
  end;

  (* let update = ... *)
  if (go.go_update || go.go_patch) && ti.ti_nonpk_cts <> [] then begin
    open_query_let "update";
    List.iter (fun (cn, _ct) -> fprintf oc " ?%s" cn) ti.ti_nonpk_cts;
    pp oc " o =";
    pp oc "@ @[<v 1>(match o.state with";
    pp oc "@ | @[<v 1>Inserting _ ->";
    pp oc "@ Lwt.fail (Caqti_persist.Error.Conflict \
                {conflict_type = `Update_insert; conflict_table = \"%s\"})@]"
       (Episql.string_of_qname ti.ti_tqn);
    pp oc "@ | @[<v 1>Deleting _ ->";
    pp oc "@ Lwt.fail (Caqti_persist.Error.Conflict \
                {conflict_type = `Update_delete; conflict_table = \"%s\"})@]"
       (Episql.string_of_qname ti.ti_tqn);
    pp oc "@ | @[<v 1>Absent ->";
    if go.go_insert_upserts then begin
      List.iter
        (fun (cn, ct) ->
          if ct.ct_nullable && not ct.ct_defaultable then
            pp oc "@ let %s = match %s with None -> None | Some x -> x in"
               cn cn)
        ti.ti_nonpk_nonreq_cts;
      if ti.ti_nonpk_req_cts = [] then begin
        pp oc "@ insert";
        List.iter (fun (cn, _) -> fprintf oc " ?%s" cn) ti.ti_nonpk_cts;
        fprint oc " o"
      end else begin
        pp oc "@ @[<v 1>(match ";
        List.iteri
          (fun i (cn, _) -> if i > 0 then fprint oc ", "; fprint oc cn)
          ti.ti_nonpk_req_cts;
        pp oc " with";
        pp oc "@ @[<hv 3>| ";
        List.iteri
          (fun i (cn, _) -> if i > 0 then fprint oc ", "; fprintf oc "Some %s" cn)
          ti.ti_nonpk_req_cts;
        fprint oc " ->";
        pp oc "@ insert";
        List.iter
          (fun (cn, ct) ->
            fprintf oc " %c%s" (if coltype_is_required ct then '~' else '?') cn)
          ti.ti_nonpk_cts;
        pp oc " o@]";
        pp oc "@ @[<hv 3>| _ ->";
        pp oc "@ Lwt.fail (Failure \"Attempt to update an absent row \
                                     with insufficient data to insert.\")";
        pp oc "@]@])";
      end
    end else
      pp oc "@ Lwt.fail (Failure \"Update of absent row.\")";
    pp oc "@]@ | @[<v 1>Present state ->";
    emit_use_C oc;
    pp oc "@ let ub = Ub.create () in";
    if go.go_event then
      pp oc "@ let changes = ref [] in";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ @[<v 1>(match %s with" cn;
        pp oc "@ @[<v 3>| Some x when x <> state.%s%s ->" go.go_state_prefix cn;
        pp oc "@ Ub.set ub \"%s\" (Type.%s, x);"
           cn (convname_of_coltype ct);
        if go.go_event then
          pp oc "@ changes := `Set_%s x :: !changes;" cn;
        pp oc "@]@ | _ -> ()@]);")
      ti.ti_nonpk_cts;
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      let (cn, ct) = List.hd ti.ti_pk_cts in
      pp oc "@ Ub.where ub \"%s\" (Type.%s, o.key);"
         cn (convname_of_coltype ct)
    else
      List.iter
        (fun (cn, ct) ->
          pp oc "@ Ub.where ub \"%s\" (Type.%s, o.key.%s%s);"
             cn (convname_of_coltype ct) go.go_pk_prefix cn)
        ti.ti_pk_cts;
    pp oc "@ @[<v 1>(match Ub.finish ~table_name:%S ub with"
      (Episql.string_of_qname ti.ti_tqn);
    pp oc "@ | None -> %s ()" return_ok;
    pp oc "@ @[<v 3>| Some (Request (req, params)) ->";
    pp oc "@ C.exec req params %s fun () ->" fail_or_map_result_op;
    if go.go_select_cache then pp oc "@ clear_select_cache ();";
    List.iteri
      (fun i (cn, _) ->
        if i > 0 then pp oc ";";
        pp oc "@ (match %s with None -> () | Some v -> state.%s%s <- v)"
           cn go.go_state_prefix cn)
      ti.ti_nonpk_cts;
    if go.go_event then
      pp oc ";@ o.notify (`Update (List.rev !changes))";
    pp oc "@]@])@])@]";
    close_query_let ()
  end;

  (* let delete = ... *)
  if go.go_delete || go.go_patch then begin
    open_query_let "delete";
    pp oc " ({key; _} as o) =";
    emit_use_C oc;
    pp oc "@ @[<v 2>let rec retry () =";
    pp oc "@ @[<v 1>(match o.state with";
    pp oc "@ | Absent -> %s ()" return_ok;
    pp oc "@ | Inserting _c -> Lwt_condition.wait _c >>= fun _ -> retry ()";
    pp oc "@ @[<v 3>| Present _ ->";
    pp oc "@ let _c = Lwt_condition.create () in";
    pp oc "@ o.state <- Deleting _c;";
    pp oc "@ C.exec Q.delete ";
    emit_param oc ti "key" ti.ti_pk_cts;
    pp oc " %s fun () ->" fail_or_map_result_op;
    if go.go_select_cache then pp oc "@   clear_select_cache ();";
    pp oc "@ o.state <- Absent;";
    pp oc "@ Lwt_condition.broadcast _c ();";
    pp oc "@ o.notify `Delete";
    pp oc "@]@ | Deleting _c -> Lwt_condition.wait _c%s)@] in"
      (if go.go_return_result then " >|= Result.ok" else "");
    pp oc "@]@ retry ()";
    close_query_let ()
  end;

  (* let patch = ... *)
  if go.go_patch then begin
    open_query_let "patch";
    pp oc " o p =";
    pp oc "@ @[<v 1>(match p with";
    if ti.ti_nonpk_cts = [] then begin
      pp oc "@ | `Insert ((), ()) -> insert o";
      pp oc "@ | `Update [] -> %s ()" return_ok;
      pp oc "@ | `Update (x :: _) -> Prime.absurd x"
    end else begin
      pp oc "@ @[<v 3>| `Insert (r, d) ->";
      pp oc "@ insert";
      List.iter
        (fun (cn, _ct) -> fprintf oc " ~%s:r.%s%s" cn go.go_value_r_prefix cn)
        ti.ti_nonpk_req_cts;
      List.iter
        (fun (cn, _ct) -> fprintf oc " ?%s:d.%s%s" cn go.go_value_d_prefix cn)
        ti.ti_nonpk_nonreq_cts;
      pp oc " o;@]";
      pp oc "@ @[<v 3>| `Update changes ->";
      List.iter
        (fun (cn, _) -> pp oc "@ let %s = ref None in" cn)
        ti.ti_nonpk_cts;
      pp oc "@ List.iter";
      pp oc "@   @[<hv 1>(function";
      List.iter
        (fun (cn, _) -> pp oc "@ | `Set_%s v -> %s := Some v" cn cn)
        ti.ti_nonpk_cts;
      pp oc ")@]";
      pp oc "@   changes;";
      pp oc "@ update";
      List.iter (fun (cn, _) -> fprintf oc " ?%s:!%s" cn cn) ti.ti_nonpk_cts;
      pp oc " o@]";
    end;
    pp oc "@ | `Delete -> delete o)@]";
    close_query_let ()
  end;

  if go.go_select_cache then
    pp oc "@ let uncache_all () = clear_select_cache (); uncache_all ()";

  (* let patches = ... *)
  if go.go_event then pp oc "@ let patches {patches; _} = patches";

  (* let value = ... *)
  if go.go_value then begin
    pp oc "@ @[<v 2>let value o =";
    pp oc "@ @[<v 1>(match o.state with";
    pp oc "@ @[<hv 3>| Present state ->";
    if ti.ti_nonpk_cts = [] then begin
      if go.go_raise_on_absent then
        pp oc "@ ()"
      else
        pp oc "@ Some ()"
    end else begin
      if go.go_raise_on_absent then
        pp oc "@ @[<hv 2>{"
      else
        pp oc "@ @[<hv 2>Some {";
      List.iter
        (fun (cn, _) ->
          pp oc "@ %s%s = state.%s%s;"
             go.go_value_prefix cn go.go_state_prefix cn)
        ti.ti_nonpk_cts;
      pp oc "@]@ }"
    end;
    pp oc "@]";
    if go.go_raise_on_absent then
      pp oc "@ | _ -> raise Caqti_persist.Error.Not_present"
    else
      pp oc "@ | _ -> None";
    pp oc ")@]@]"
  end;

  pp oc "@]@ end"

let generate emit stmts oc =
  let emit_top = function
   | Create_schema _ | Create_sequence _ | Create_enum _
   | Drop_schema _ | Drop_table _ | Drop_sequence _ | Drop_type _
   | Create_table {table_scope = `Temporary; _} -> ()
   | Create_table {table_qname = ti_tqn; table_items = items; _} ->
      if not (Filter.test_qname go.go_filter_tables ti_tqn) then () else
      let pk_opt, cts = List.fold_right collect items (None, []) in
      (match pk_opt with
       | None -> ()
       | Some pk ->
          let set_pk = function
           | _cn, {ct_pk = true; _} as cn_ct -> cn_ct
           | cn, ct ->
              let ct_pk = List.mem cn pk in
              let ct_nullable = ct.ct_nullable && not ct_pk in
              cn, {ct with ct_pk; ct_nullable} in
          let ti_cts = List.map set_pk cts in
          let ti_req_cts =
            List.filter
              (fun (_, ct) -> not ct.ct_nullable && not ct.ct_defaultable)
              ti_cts in
          let ti_pk_cts =
            List.filter (fun (_, {ct_pk; _}) -> ct_pk) ti_cts in
          let ti_nonpk_cts =
            List.filter (fun (_, {ct_pk; _}) -> not ct_pk) ti_cts in
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
          emit oc ti) in
  List.iter emit_top stmts

let common_header = "\
  (* Generated by episql. *)\n\n\
  [@@@ocaml.warning \"-27\"]\n\n\
  open Caqti_persist\n\
  module type P = sig\n\
 \  module Beacon : Prime_beacon.S\n\
 \  val use_db : ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t\n\
  end\n\n"

let generate_intf stmts oc =
  pp oc "@[<v 0>";
  pp_print_string oc common_header;
  emit_custom_open oc;
  pp oc "@[<v 2>module type S = sig";
  generate emit_intf stmts oc;
  pp oc "@]@ end@\n";
  pp oc "@ module Make (P : P) : S@\n@]@."

let generate_impl stmts oc =
  pp oc "@[<v 0>";
  pp_print_string oc common_header;
  pp oc "@ open Lwt.Infix";
  pp oc "@ open Caqti_persist.Caqti_persist_internal";
  emit_custom_open oc;
  pp oc "@ @[<v 2>module type S = sig";
  generate emit_intf stmts oc;
  pp oc "@]@ end@\n";
  if go.go_raise_on_absent then
    Option.iter (pp oc "@ let section = Lwt_log.Section.make \"%s\"")
                go.go_log_debug;
  pp oc "@ @[<v 2>module Make (P : P) = struct";
  pp oc "@ module Cache = Pk_cache.Make (P.Beacon)";
  pp oc "@ @[<v 2>module Type = struct";
  pp oc "@ include Caqti_type";
  pp oc "@ include Caqti_type_calendar";
  pp oc "@ @]end";
  generate emit_impl stmts oc;
  pp oc "@]@ end@]@\n@."

let generate_types ~in_intf stmts oc =
  pp oc "@[<v 0>";
  pp oc "@ (* Generated by episql. *)\n\n";
  pp_print_string oc "[@@@ocaml.warning \"-27\"]\n";
  emit_custom_open oc;
  if go.go_patch then begin
    pp oc "@ @[<v 2>type ('value_r, 'value_d, 'change) persist_patch_in =";
    pp oc "@ [ `Insert of 'value_r * 'value_d";
    pp oc "@ | `Update of 'change list";
    pp oc "@ | `Delete ]@]";
    emit_deriving oc
  end;
  if go.go_event then begin
    pp oc "@ @[<v 2>type ('value, 'change) persist_patch_out =";
    pp oc "@ [ `Insert of 'value";
    pp oc "@ | `Update of 'change list";
    pp oc "@ | `Delete ]@]";
    emit_deriving oc
  end;
  generate (emit_types ~in_intf) stmts oc;
  pp oc "@."

let generate_intf' stmts oc =
  generate_intf stmts (formatter_of_out_channel oc)
let generate_impl' stmts oc =
  generate_impl stmts (formatter_of_out_channel oc)
let generate_types' ~in_intf stmts oc =
  generate_types ~in_intf stmts (formatter_of_out_channel oc)

(*
let obsolete_now_default_flag name =
  let h () =
    eprintf "warning: %s is now the default and will be removed.\n%!" name
  in
  (name, Arg.Unit h, " Obsolete option which is now the default.")
*)

let () =
  let set_types_module mn =
    go.go_types_module <-
      Some (String.capitalize_ascii (if Filename.check_suffix mn ".mli"
                               then Filename.chop_suffix mn ".mli"
                               else mn)) in
  let stack_filter s =
    go.go_filter_tables <- go.go_filter_tables @ Filter.of_string s in
  let stack_filters s = List.iter stack_filter (String.chop_affix "," s) in
  let common_arg_specs = [
    "-ppx-deriving",
      Arg.String (fun c -> go.go_use_ppx <- true;
                           go.go_deriving <- c :: go.go_deriving),
      "CLASS Add [@@deriving CLASS] to type definitions. \
             Only supported with separate types module. \
             The -use-type* and -open flags are useful for supplementing \
             suitable definitions for missing classes.";
    "-deriving",
      Arg.String (fun c -> go.go_use_ppx <- false;
                           go.go_deriving <- c :: go.go_deriving),
      " Deprecated.";
    "-open", Arg.String (fun m -> go.go_open <- m :: go.go_open),
      "M Open M at top of the generated files but after other open statements.";
    "-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- true),
      " Raise Not_present instead of returning options for state, value, and \
       getters. The exception is also raised for getters of nullable fields \
       for consistency, even though they return options.";
    "-no-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- false),
      " Inversion of -raise-on-absent and the default for now.";
    "-return-result", Arg.Unit (fun () -> go.go_return_result <- true),
      " Return result instead of raising an exception on DB failure \
       (recommended, will become default).";
    "-connection-arg", Arg.String (fun s -> go.go_connection_arg <- Some s),
      "NAME If passed, each query function accepts an optional argument ?NAME \
       which will be used in place of calling P.use_db. If you use pooled \
       connections, you will need this to use the query functions within \
       a transaciton.";
    "-filter-tables", Arg.String stack_filters,
      "TN,...,TN Comma-separated list of patterns of tables to include.";
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
    "-pk-module",
      Arg.String (fun arg -> go.go_pk_module <- Some arg;
                             go.go_pk_prefix <- arg ^ "."),
      "M Put key record into a sub-module M instead of prefixing.";
    "-public-state", Arg.Unit (fun () -> go.go_public_state <- true),
      " Don't make state records private. This allows modifying to account for \
       out-of-band changes to the database.";
    "-enable-patch", Arg.Unit (fun () -> go.go_patch <- true),
      " Generate patch function.";
    "-enable-event", Arg.Unit (fun () -> go.go_event <- true),
      " Generate code to emit react events.";
  ] in
  let arg_specs = [
    "-t", Arg.String set_types_module,
      "M Assume M contains corresponding types generated with \
         -g caqti-persist-types-*.";
  ] @ common_arg_specs in
  let types_arg_specs = common_arg_specs in
  Episql.register_generator ~arg_specs "caqti-persist-mli" generate_intf';
  Episql.register_generator ~arg_specs "caqti-persist-ml" generate_impl';
  Episql.register_generator ~arg_specs:types_arg_specs "caqti-persist-types-mli"
                            (generate_types' ~in_intf:true);
  Episql.register_generator ~arg_specs:types_arg_specs "caqti-persist-types-ml"
                            (generate_types' ~in_intf:false)
