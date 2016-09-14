(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
    match s.[0] with
    | '+' -> Incl (Re.compile (Re_glob.glob (String.(trim (slice_from 1 s)))))
    | '-' -> Excl (Re.compile (Re_glob.glob (String.(trim (slice_from 1 s)))))
    | _ ->   Incl (Re.compile (Re_glob.glob s))

  let of_string s = String.chop_affix "," s |> List.map instr_of_string

  let rec test flt s =
    match flt with
    | [] -> None
    | Incl re :: flt when Re.execp re s -> Some true
    | Excl re :: flt when Re.execp re s -> Some false
    | _ :: flt -> test flt s

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
  mutable go_log_debug : string option;
  mutable go_connection_arg : string option;
  mutable go_obsolete_order_by : bool;
}
let go = {
  go_types_module = None;
  go_filter_tables = [];
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
  go_use_ppx = true;
  go_deriving = [];
  go_open = [];
  go_type_counit = "Prime.counit";
  go_type_date = "CalendarLib.Date.t";
  go_type_timestamp = "CalendarLib.Calendar.t";
  go_raise_on_absent = false;
  go_log_debug = Some "caqti-persist";
  go_connection_arg = None;
  go_obsolete_order_by = true;
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
  | `Date -> "date"
  | `Timestamp (_, _) -> "utc"
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

let variant_of_colname cn = cn

let collect = function
  | Column { column_name = cn; column_type = ct_type;
             column_constraints = ccs } ->
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

let pp = Format.fprintf
let fprint oc s = pp_print_string oc s
let fprintf oc fmt = Printf.ksprintf (fprint oc) fmt
let rec findent oc n =
  if n = 0  then () else
  if n >= 8 then (pp_print_char oc '\t'; findent oc (n - 8))
            else (pp_print_char oc ' ';  findent oc (n - 1))

let emit_custom_open oc =
  List.iter (fun m -> pp oc "@ open %s" m) (List.rev go.go_open)

let emit_deriving oc =
  if go.go_deriving <> [] then begin
    let modules_str = String.concat ", " go.go_deriving in
    if go.go_use_ppx
      then fprintf oc " [@@deriving %s]" modules_str
      else fprintf oc " deriving (%s)" modules_str
  end

let emit_type_pk oc ti =
  if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then begin
    pp oc "@ type key = %s"
       (string_of_coltype (snd (List.hd ti.ti_pk_cts)));
    emit_deriving oc
  end else begin
    pp oc "@ @[<v 2>type key = {";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s : %s;" go.go_pk_prefix cn (string_of_coltype ct))
      ti.ti_pk_cts;
    pp oc "@]@ }";
    emit_deriving oc
  end

let emit_type_nonpk ~in_intf oc ti =
  if ti.ti_nonpk_cts = [] then
    pp oc "@ type state = unit"
  else begin
    pp oc "@ @[<v 2>type state = ";
    pp_print_string oc (if in_intf then "private {" else "{");
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
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s : %s option;"
           go.go_value_d_prefix cn (string_of_datatype ct.ct_type))
      ti.ti_nonpk_nonreq_cts;
    pp oc "@]@ }";
    emit_deriving oc
  end;
  if ti.ti_nonpk_cts = [] then begin
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
  pp oc "@ type patch_in = (value_r, value_d, change) persist_patch_in";
  emit_deriving oc;
  pp oc "@ type patch_out = (value, change) persist_patch_out";
  emit_deriving oc

let emit_types ~in_intf oc ti =
  if in_intf then
    pp oc "@ @[<v 2>module %s : sig" (String.capitalize (snd ti.ti_tqn))
  else
    pp oc "@ @[<v 2>module %s = struct" (String.capitalize (snd ti.ti_tqn));
  emit_type_pk oc ti;
  emit_type_patch_etc oc ti;
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
  end;
  if in_intf then
    pp oc "@ val changes_of_value : value -> change list"
  else begin
    pp oc "@ @[<v 2>let changes_of_value f = [";
    List.iter
      (fun (cn, _) -> pp oc "@ `Set_%s f.%s%s;" cn go.go_value_prefix cn)
      ti.ti_nonpk_cts;
    pp oc "@]@ ]"
  end;
  pp oc "@]@ end"

let emit_intf oc ti =
  pp oc "@ @[<v 2>module %s : sig" (String.capitalize (snd ti.ti_tqn));
  begin match go.go_types_module with
  | None ->
    emit_type_pk oc ti;
    emit_type_patch_etc oc ti
  | Some types_module ->
    let mn = types_module ^ "." ^ String.capitalize (snd ti.ti_tqn) in
    pp oc "@ @[<v 2>include module type of %s" mn;
    pp oc "@ @[<v 1>with type key = %s.key" mn;
    pp oc "@ and type value = %s.value" mn;
    pp oc "@ and type value_r = %s.value_r" mn;
    pp oc "@ and type value_d = %s.value_d" mn;
    pp oc "@ and type change = %s.change" mn;
    pp oc "@ and type patch_in = %s.patch_in" mn;
    pp oc "@ and type patch_out = %s.patch_out" mn;
    pp oc "@]@]"
  end;
  emit_type_nonpk ~in_intf:true oc ti;
  pp oc "@ type t";
  pp oc "@ val key : t -> key";
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
  pp oc "@ val fetch : key -> t Lwt.t";
  if go.go_select then begin
    open_query_val "select";
    List.iter
      (fun (cn, ct) ->
        let tn = string_of_datatype ct.ct_type in
        pp oc "@ ?%s: [@[<hov 0>< %s order_predicate" cn tn;
        if ct.ct_type = `Text then
          pp oc "@ | `Like of %s@ | `Ilike of %s" tn tn;
        if ct.ct_nullable then pp oc "@ | `Null";
        pp oc "@]] ->")
      ti.ti_cts;
    pp oc "@ ?order_by: [@[<hov 0>< ";
    List.iteri
      (fun i (cn, _) ->
        if i > 0 then pp oc "@ | ";
        pp oc "`%s" (variant_of_colname cn))
      ti.ti_cts;
    if go.go_obsolete_order_by then
      pp oc "@]] list ->"
    else
      pp oc "@]] order_item list ->";
    pp oc "@ ?limit: int ->@ ?offset: int ->";
    pp oc "@ unit ->@ t list Lwt.t";
    close_query_val ()
  end;
  if go.go_select_cache then
    pp oc "@ val clear_select_cache : unit -> unit";
  if go.go_create then begin
    open_query_val "create";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s: %s ->"
                (if ct.ct_nullable || ct.ct_defaultable then "?" else "")
                cn (string_of_datatype ct.ct_type))
      ti.ti_cts;
    pp oc "@ unit -> t Lwt.t";
    close_query_val ()
  end;
  if go.go_insert then begin
    open_query_val "insert";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ %s%s: %s ->"
           (if ct.ct_nullable || ct.ct_defaultable then "?" else "")
           cn (string_of_datatype ct.ct_type))
      ti.ti_nonpk_cts;
    pp oc "@ t -> unit Lwt.t";
    close_query_val ()
  end;
  if go.go_update && ti.ti_nonpk_cts <> [] then begin
    open_query_val "update";
    List.iter
      (fun (cn, {ct_type = dt; ct_nullable = dn}) ->
        pp oc "@ ?%s: %s%s ->" cn (string_of_datatype dt)
           (if dn then " option" else ""))
      ti.ti_nonpk_cts;
    pp oc "@ t -> unit Lwt.t";
    close_query_val ()
  end;
  if go.go_delete then begin
    open_query_val "delete";
    pp oc "@ t -> unit Lwt.t";
    close_query_val ()
  end;
  if go.go_patch then begin
    open_query_val "patch";
    pp oc "@ t -> patch_in -> unit Lwt.t";
    close_query_val ()
  end;
  if go.go_event then
    pp oc "@ val patches : t -> patch_out React.E.t";
  pp oc "@]@ end"

let emit_query oc name emit =
  let pgparam_no = ref 0 in
  let pgparam () = incr pgparam_no; sprintf "$%d" !pgparam_no in
  let sqliteparam () = "?" in
  pp oc "@ @[<v 2>let %s = Caqti_query.prepare_fun @@@@ function" name;
  pp oc "@ | `Pgsql -> \""; emit pgparam; pp oc "\"";
  pp oc "@ | `Sqlite -> \""; emit sqliteparam; pp oc "\"";
  pp oc "@ | _ -> raise Caqti_query.Missing_query_string@]"

let emit_use_C_noarg oc =
  pp oc "@ P.use_db @@@@ fun (module C : Caqti_lwt.CONNECTION) ->"

let emit_use_C oc =
  match go.go_connection_arg with
  | None ->
    emit_use_C_noarg oc
  | Some s ->
    pp oc "@ (match %s with None -> P.use_db | Some db -> fun f -> f db)" s;
    pp oc "@   @@@@ fun (module C : Caqti_lwt.CONNECTION) ->"

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

  pp oc "@ @[<v 2>module %s = struct" (String.capitalize (snd ti.ti_tqn));
  pp oc "@ @[<v 2>module Q = struct";
  emit_query oc "fetch" emit_fetch;
  emit_query oc "delete" emit_delete;
  pp oc "@]@ end";
  begin match go.go_types_module with
  | None ->
    emit_type_pk oc ti;
    emit_type_patch_etc oc ti
  | Some tm ->
    pp oc "@ include %s.%s" tm (String.capitalize (snd ti.ti_tqn))
  end;
  emit_type_nonpk ~in_intf:false oc ti;
  pp oc "@ @[<v 2>include Cache (struct";
  pp oc "@ type _t0 = key\ttype key = _t0";
  pp oc "@ type _t1 = state\ttype state = _t1";
  pp oc "@ type _t2 = value\ttype value = _t2";
  pp oc "@ type _t3 = change\ttype change = _t3";
  pp oc "@ let key_size = %d" (List.length ti.ti_pk_cts);
  pp oc "@ let state_size = %d" (List.length ti.ti_nonpk_cts);
  pp oc "@ @[<v 2>let fetch key =";
  emit_use_C_noarg oc;
  pp oc "@ C.find_opt Q.fetch ";
  emit_detuple oc ti.ti_nonpk_cts; fprint oc " ";
  emit_param oc ti "key" ti.ti_pk_cts;
  pp oc "@]@]@ end)";

  pp oc "@ let key {key} = key";
  if go.go_raise_on_absent then begin
    pp oc "@ @[<v 2>let absent op o =";
    if go.go_log_debug <> None then
      pp oc "@ Lwt_log.ign_debug_f ~section \
                \"Called %%s on absent row of %s.\" op;"
         (snd ti.ti_tqn);
    pp oc "@ raise Not_present@]"
  end;
  pp oc "@ let is_present = function {state = Present _} -> true | _ -> false";
  if go.go_raise_on_absent then
    pp oc "@ let state = \
               function {state = Present x} -> x | o -> absent \"state\" o"
  else
    pp oc "@ let state = function {state = Present x} -> Some x | _ -> None";
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

  let open_query_let fn =
    pp oc "@ @[<v 2>let "; fprint oc fn;
    Option.iter (fprintf oc " ?%s") go.go_connection_arg in
  let close_query_let () = pp oc "@]" in

  if go.go_select_cache then begin
    pp oc "@ let select_cache = Prime_cache.create P.Beacon.cache_metric 19";
    pp oc "@ let clear_select_cache () = Prime_cache.clear select_cache"
  end;

  if go.go_insert || go.go_patch then begin
    open_query_let "insert";
    List.iter
      (fun (cn, ct) ->
        fprintf oc " %c%s"
                (if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_nonpk_cts;
    fprint oc " o =";
    pp oc "@ @[<v 2>let rec retry () =";
    pp oc "@ match o.state with";
    pp oc "@ @[<v 2>| Absent ->";
    pp oc "@ let _c = Lwt_condition.create () in";
    pp oc "@ o.state <- Inserting _c;";
    emit_use_C oc;
    pp oc "@ let module Ib = Insert_buffer (C) in";
    pp oc "@ let ib = Ib.create C.backend_info \"%s\" in"
             (Episql.string_of_qname ti.ti_tqn);
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      let cn, ct = List.hd ti.ti_pk_cts in
      pp oc "@ Ib.set ib \"%s\" C.Param.(%s o.key);"
         cn (convname_of_datatype ct.ct_type)
    else
      List.iter
        (fun (cn, ct) ->
          pp oc "@ Ib.set ib \"%s\" C.Param.(%s o.key.%s%s);"
             cn (convname_of_datatype ct.ct_type) go.go_pk_prefix cn)
        ti.ti_pk_cts;
    List.iter
      (fun (cn, ct) ->
        if ct.ct_nullable || ct.ct_defaultable then
          pp oc "@ (match %s with \
                              None -> () \
                              | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
                   cn cn (convname_of_datatype ct.ct_type)
        else
          pp oc "@ Ib.set ib \"%s\" C.Param.(%s %s);"
             cn (convname_of_datatype ct.ct_type) cn)
      ti.ti_nonpk_cts;
    List.iter
      (fun (cn, ct) -> if ct.ct_defaultable then pp oc "@ Ib.ret ib \"%s\";" cn)
      ti.ti_nonpk_cts;
    pp oc "@ let q, p = Ib.contents ib in";

    let emit_field pfx i (cn, ct) =
      if i > 0 then fprint oc "; ";
      fprintf oc "%s%s = %s" pfx cn cn in
    if ti.ti_nonpk_def_cts <> [] then begin
      pp oc "@ @[<v 2>let decode t =";
      pp oc "@ let _i = ref (-1) in";
      pp oc "@ let _df c = function Some x -> x | None -> incr _i; c !_i t in";
      List.iter
        (fun (cn, ct) ->
          pp oc "@ let %s" cn;
          pp oc (if ct.ct_nullable then " = Some (" else " = ");
          pp oc "_df C.Tuple.%s %s" (convname_of_datatype ct.ct_type) cn;
          pp oc (if ct.ct_nullable then ") in" else " in"))
        ti.ti_nonpk_def_cts;
      if ti.ti_nonpk_cts = [] then
        fprint oc " () in"
      else begin
        pp oc "@ @[<hv 2>{";
        List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
        pp oc "@]} in"
      end;
      pp oc "@]";
      pp oc "@ C.find q decode p >|= fun state ->";
      if go.go_select_cache then
        pp oc "@ clear_select_cache ();";
    end else begin
      pp oc "@ C.exec q p >|= fun () ->";
      if go.go_select_cache then
        pp oc "@ clear_select_cache ();";
      if ti.ti_nonpk_cts = [] then
        pp oc "@ let state = () in"
      else begin
        pp oc "@ @[<hv 2>let state = {";
        List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
        pp oc "@]} in"
      end
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
          (fun (cn, ct) ->
            pp oc "@ %s%s = state.%s%s;"
               go.go_value_prefix cn go.go_state_prefix cn)
          ti.ti_nonpk_cts;
        pp oc "@]@ })"
      end
    end;
    pp oc "@]";
    pp oc "@ | Inserting _c -> Lwt_condition.wait _c >|= fun _ -> ()";
    pp oc "@ | Present x -> Lwt.return_unit";
    pp oc "@ | Deleting _c -> Lwt_condition.wait _c >>= retry in";
    pp oc "@]@ retry ()";
    close_query_let ()
  end;

  if go.go_create then begin
    open_query_let "create";
    List.iter
      (fun (cn, ct) ->
        fprintf oc " %c%s"
          (if ct.ct_nullable || ct.ct_defaultable then '?' else '~') cn)
      ti.ti_cts;
    fprint oc " () =";
    emit_use_C oc;
    pp oc "@ let module Ib = Insert_buffer (C) in";
    pp oc "@ let ib = Ib.create C.backend_info \"%s\" in"
          (Episql.string_of_qname ti.ti_tqn);
    List.iter
      (fun (cn, ct) ->
        if ct.ct_defaultable then
          pp oc "@ (match %s with None -> Ib.ret ib \"%s\" \
                           | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
             cn cn cn (convname_of_datatype ct.ct_type)
        else if ct.ct_nullable then
          pp oc "@ (match %s with None -> () \
                           | Some x -> Ib.set ib \"%s\" C.Param.(%s x));"
             cn cn (convname_of_datatype ct.ct_type)
        else
          pp oc "@ Ib.set ib \"%s\" C.Param.(%s %s);"
             cn (convname_of_datatype ct.ct_type) cn)
      ti.ti_cts;
    if have_default then
      pp oc "@ if not (Ib.have_ret ib) then Ib.ret ib \"0\";";
    pp oc "@ let q, p = Ib.contents ib in";
    pp oc "@ @[<v 2>let decode t =";
    if have_default then begin
      pp oc "@ let _i = ref (-1) in";
      pp oc "@ let _df c = function Some x -> x | None -> incr _i; c !_i t in";
    end;
    List.iter
      (fun (cn, ct) ->
        if ct.ct_defaultable then begin
          pp oc "@ let "; fprint oc cn;
          pp oc (if ct.ct_nullable then " = Some (" else " = ");
          fprintf oc "_df C.Tuple.%s %s" (convname_of_datatype ct.ct_type) cn;
          fprint oc (if ct.ct_nullable then ") in" else " in")
        end)
      ti.ti_cts;
    let emit_field pfx i (cn, ct) =
      if i > 0 then fprint oc "; ";
      fprintf oc "%s%s = %s" pfx cn cn in
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ %s," (fst (List.hd ti.ti_pk_cts))
    else begin
      pp oc "@ {";
      List.iteri (emit_field go.go_pk_prefix) ti.ti_pk_cts;
      pp oc "},"
    end;
    if ti.ti_nonpk_cts = [] then
      fprint oc " () in"
    else begin
      pp oc "@ {";
      List.iteri (emit_field go.go_state_prefix) ti.ti_nonpk_cts;
      pp oc "} in"
    end;
    pp oc "@]";
    if have_default then begin
      pp oc "@ C.find q decode p >>= fun (key, state) ->\n\t";
      if go.go_select_cache then fprint oc "clear_select_cache (); ";
      fprint oc "merge_created (key, state)"
    end else begin
      pp oc "@ C.exec q p >>= fun () -> ";
      if go.go_select_cache then fprint oc "clear_select_cache (); ";
      fprint oc "merge_created (decode ())"
    end;
    close_query_let ()
  end;

  if go.go_select then begin
    open_query_let "select";
    List.iter (fun (cn, ct) -> fprint oc " ?"; fprint oc cn) ti.ti_cts;
    fprint oc " ?(order_by = []) ?limit ?offset () =";
    if go.go_select_cache then begin
      pp oc "@ let args = ";
      List.iteri
        (fun i (cn, _) ->
          if i <> 0 then fprint oc ", ";
          fprint oc cn)
        ti.ti_cts;
      fprint oc " in";
      pp oc "@ try Lwt.return (Prime_cache.find select_cache args)";
      pp oc "@ @[<v 2>with Not_found ->";
    end;
    emit_use_C oc;
    pp oc "@ let module Sb = Select_buffer (C) in";
    pp oc "@ let sb = Sb.create C.backend_info \"%s\" in"
       (Episql.string_of_qname ti.ti_tqn);
    let emit_ret (cn, _) = pp oc "@ Sb.ret sb \"%s\";" cn in
    List.iter emit_ret ti.ti_pk_cts;
    List.iter emit_ret ti.ti_nonpk_cts;
    List.iter
      (fun (cn, ct) ->
        let conv = convname_of_datatype ct.ct_type in
        pp oc "@ begin match %s with" cn;
        pp oc "@ | None -> ()";
        if ct.ct_nullable then
          pp oc "@ | Some `Null -> Sb.(where sb [S\"%s IS NULL\"])" cn;
        let mk_binary (on, op) =
          pp oc "@ | Some (`%s x) -> \
                     Sb.(where sb [S\"%s %s \"; P C.Param.(%s x)])"
             on cn op conv in
        let mk_ternary (on, op) =
          pp oc "@ | Some (`%s (x, y)) -> \
                     Sb.(where sb [S\"%s %s \"; P C.Param.(%s x); \
                                   S\" AND \"; P C.Param.(%s y)])"
             on cn op conv conv in
        List.iter mk_binary
          ["Eq", "="; "Ne", "<>"; "Lt", "<"; "Le", "<="; "Ge", ">="; "Gt", ">"];
        List.iter mk_ternary
          ["Between", "BETWEEN"; "Not_between", "NOT BETWEEN"];
        if ct.ct_type = `Text then
          List.iter mk_binary ["Like", "LIKE"; "Ilike", "ILIKE"];
        pp oc "@ end;")
      ti.ti_cts;
    if go.go_obsolete_order_by then begin
      pp oc "@ List.iter (fun col -> Sb.obsolete_order_by sb (match col with ";
      List.iteri
        (fun i (cn, _) ->
          if i > 0 then fprint oc " | ";
          fprintf oc "`%s -> \"%s\"" (variant_of_colname cn) cn)
        ti.ti_cts;
      pp oc ")) order_by;"
    end else begin
      pp oc "@ List.iter (Sb.order_by sb (function ";
      List.iteri
        (fun i (cn, _) ->
          if i > 0 then fprint oc " | ";
          fprintf oc "`%s -> \"%s\"" (variant_of_colname cn) cn)
        ti.ti_cts;
      pp oc ")) order_by;"
    end;
    pp oc "@ (match limit with None -> () | Some n -> Sb.limit sb n);";
    pp oc "@ (match offset with None -> () | Some n -> Sb.offset sb n);";
    pp oc "@ let q, p = Sb.contents sb in";
    pp oc "@ @[<v 2>let decode t acc =";
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      pp oc "@ let key = C.Tuple.(%s 0 t) in"
         (convname_of_coltype (snd (List.hd ti.ti_cts)))
    else begin
      pp oc "@ let key = C.Tuple.({";
      List.iteri
        (fun i (cn, ct) ->
          if i > 0 then fprint oc "; ";
          fprintf oc "%s%s = %s %d t"
                  go.go_pk_prefix cn (convname_of_coltype ct) i)
        ti.ti_pk_cts;
      fprint oc "}) in"
    end;
    if ti.ti_nonpk_cts = [] then
      pp oc "@ let state = () in"
    else begin
      let n_pk = List.length ti.ti_pk_cts in
      pp oc "@ let state = C.Tuple.({";
      List.iteri
        (fun i (cn, ct) ->
          if i > 0 then fprint oc "; ";
          fprintf oc "%s%s = %s %d t"
                  go.go_state_prefix cn (convname_of_coltype ct) (i + n_pk))
        ti.ti_nonpk_cts;
      fprint oc "}) in"
    end;
    pp oc "@ merge (key, Present state) :: acc in";
    pp oc "@]";
    if go.go_select_cache then begin
      pp oc "@ C.fold q decode p [] >|= fun r_rev ->";
      pp oc "@ let r = List.rev r_rev in";
      pp oc "@ let g = !select_grade (List.length r * %d + %d) in"
         (List.length ti.ti_nonpk_cts) (List.length ti.ti_cts + 2);
      pp oc "@ Prime_cache.replace select_cache g args r; r"
    end else
      pp oc "@ C.fold q decode p []";
    pp oc "@]";
    close_query_let ()
  end;

  if (go.go_update || go.go_patch) && ti.ti_nonpk_cts <> [] then begin
    open_query_let "update";
    List.iter (fun (cn, ct) -> fprintf oc " ?%s" cn) ti.ti_nonpk_cts;
    pp oc " o =";
    pp oc "@ match o.state with";
    pp oc "@ | Inserting _ -> Lwt.fail (Conflict `Update_insert)";
    pp oc "@ | Deleting _ -> Lwt.fail (Conflict `Update_delete)";
    pp oc "@ @[<v 2>| Absent ->";
    if go.go_insert_upserts then begin
      List.iter
        (fun (cn, ct) ->
          if ct.ct_nullable then
            pp oc "@ let %s = match %s with None -> None | Some x -> x in"
               cn cn)
        ti.ti_nonpk_nonreq_cts;
      if ti.ti_nonpk_req_cts = [] then begin
        pp oc "@ insert";
        List.iter (fun (cn, _) -> fprintf oc " ?%s" cn) ti.ti_nonpk_cts;
        fprint oc " o"
      end else begin
        pp oc "@ begin match ";
        List.iteri
          (fun i (cn, _) -> if i > 0 then fprint oc ", "; fprint oc cn)
          ti.ti_nonpk_req_cts;
        pp oc " with";
        pp oc "@ @[<v 2>| ";
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
        pp oc "@ @[<v 2>| _ ->";
        pp oc "@ Lwt.fail (Failure \"Attempt to update an absent row \
                                     with insufficient data to insert.\")";
        pp oc "@]@ end";
      end
    end else
      pp oc "@ Lwt.fail (Failure \"Update of absent row.\")";
    pp oc "      | Present state -> ";
    emit_use_C oc;
    pp oc "@ let module Ub = Update_buffer (C) in";
    pp oc "@ let ub = Ub.create C.backend_info \"%s\" in"
       (Episql.string_of_qname ti.ti_tqn);
    if go.go_event then
      pp oc "@ let changes = ref [] in";
    List.iter
      (fun (cn, ct) ->
        pp oc "@ begin match %s with" cn;
        pp oc "@ | Some x when x <> state.%s%s ->" go.go_state_prefix cn;
        pp oc "@   Ub.set ub \"%s\" C.Param.(%s x);"
           cn (convname_of_coltype ct);
        if go.go_event then
          pp oc "@   changes := `Set_%s x :: !changes;" cn;
        pp oc "@ | _ -> ()";
        pp oc "@ end;")
      ti.ti_nonpk_cts;
    if go.go_collapse_pk && List.length ti.ti_pk_cts = 1 then
      let (cn, ct) = List.hd ti.ti_pk_cts in
      pp oc "@ Ub.where ub \"%s\" C.Param.(%s o.key);"
         cn (convname_of_coltype ct)
    else
      List.iter
        (fun (cn, ct) ->
          pp oc "@ Ub.where ub \"%s\" C.Param.(%s o.key.%s%s);"
             cn (convname_of_coltype ct) go.go_pk_prefix cn)
        ti.ti_pk_cts;
    pp oc "@ begin match Ub.contents ub with";
    pp oc "@ | None -> Lwt.return_unit";
    pp oc "@ @[<v 2>| Some (q, params) ->";
    pp oc "@ C.exec q params >|= fun () ->";
    if go.go_select_cache then pp oc "@ clear_select_cache ();";
    List.iteri
      (fun i (cn, _) ->
        if i > 0 then fprint oc ";\n";
        pp oc "@ (match %s with None -> () | Some v -> state.%s%s <- v)"
           cn go.go_state_prefix cn)
      ti.ti_nonpk_cts;
    if go.go_event then
      pp oc ";@ o.notify (`Update (List.rev !changes))";
    pp oc "@]@ end@]";
    close_query_let ()
  end;

  if go.go_delete || go.go_patch then begin
    open_query_let "delete";
    pp oc " ({key} as o) =";
    emit_use_C oc;
    pp oc "@ @[<v 2>let rec retry () =";
    pp oc "@ match o.state with";
    pp oc "@ | Absent -> Lwt.return_unit";
    pp oc "@ | Inserting _c -> Lwt_condition.wait _c >>= fun _ -> retry ()";
    pp oc "@ | Present _ ->";
    pp oc "@   let _c = Lwt_condition.create () in";
    pp oc "@   o.state <- Deleting _c;";
    pp oc "@   C.exec Q.delete ";
    emit_param oc ti "key" ti.ti_pk_cts; fprint oc " >|= fun () ->";
    if go.go_select_cache then pp oc "@   clear_select_cache ();";
    pp oc "@   o.state <- Absent;";
    pp oc "@   Lwt_condition.broadcast _c ();";
    pp oc "@   o.notify `Delete";
    pp oc "@ | Deleting _c -> Lwt_condition.wait _c in";
    pp oc "@]@ retry ()";
    close_query_let ()
  end;

  if go.go_patch then begin
    open_query_let "patch";
    pp oc " o p =";
    pp oc "@ match p with";
    if ti.ti_nonpk_cts = [] then begin
      pp oc "@ | `Insert ((), ()) -> insert o";
      pp oc "@ | `Update [] -> Lwt.return_unit";
      pp oc "@ | `Update (x :: _) -> Prime.absurd x"
    end else begin
      pp oc "@ @[<v 2>| `Insert (r, d) ->";
      pp oc "@ insert";
      List.iter
        (fun (cn, ct) -> fprintf oc " ~%s:r.%s%s" cn go.go_value_r_prefix cn)
        ti.ti_nonpk_req_cts;
      List.iter
        (fun (cn, ct) -> fprintf oc " ?%s:d.%s%s" cn go.go_value_d_prefix cn)
        ti.ti_nonpk_nonreq_cts;
      pp oc " o;@]";
      pp oc "@ @[<v 2>| `Update changes ->";
      List.iter
        (fun (cn, _) -> pp oc "@ let %s = ref None in" cn)
        ti.ti_nonpk_cts;
      pp oc "@ List.iter";
      pp oc "@   @[<hv 2>(function";
      List.iter
        (fun (cn, _) -> pp oc "@ | `Set_%s v -> %s := Some v" cn cn)
        ti.ti_nonpk_cts;
      pp oc ")@]";
      pp oc "@   changes;";
      pp oc "@ update";
      List.iter (fun (cn, _) -> fprintf oc " ?%s:!%s" cn cn) ti.ti_nonpk_cts;
      pp oc " o@]";
    end;
    pp oc "@ | `Delete -> delete o";
    close_query_let ()
  end;

  if go.go_event then pp oc "@ let patches {patches} = patches";

  if go.go_value then begin
    pp oc "@ @[<v 2>let value o =";
    pp oc "@ match o.state with";
    pp oc "@ @[<v 2>| Present state ->";
    if ti.ti_nonpk_cts = [] then begin
      if go.go_raise_on_absent then
        pp oc "@ ()"
      else
        pp oc "@ Some ()"
    end else begin
      if go.go_raise_on_absent then
        pp oc "@ @[<v 2>{"
      else
        pp oc "@ @[<v 2>Some {";
      List.iter
        (fun (cn, _) ->
          pp oc "@ %s%s = state.%s%s;"
             go.go_value_prefix cn go.go_state_prefix cn)
        ti.ti_nonpk_cts;
      pp oc "@]@ }"
    end;
    pp oc "@]";
    if go.go_raise_on_absent then
      pp oc "@ | _ -> raise Not_present"
    else
      pp oc "@ | _ -> None";
    pp oc "@]"
  end;

  pp oc "@]@ end"

let generate emit stmts oc =
  let emit_top = function
    | Create_schema _ | Create_sequence _ | Create_enum _
    | Drop_schema _ | Drop_table _ | Drop_sequence _ | Drop_type _
    | Create_table {table_scope = `Temporary} -> ()
    | Create_table {table_qname = ti_tqn; table_items = items} ->
      if not (Filter.test_qname go.go_filter_tables ti_tqn) then () else
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
  pp oc "@ open Printf";
  pp oc "@ open Lwt.Infix";
  emit_custom_open oc;
  pp oc "@ @[<v 2>module type S = sig";
  generate emit_intf stmts oc;
  pp oc "@]@ end@\n";
  Option.iter (pp oc "@ let section = Lwt_log.Section.make \"%s\"")
              go.go_log_debug;
  pp oc "@ @[<v 2>module Make (P : P) = struct";
  pp oc "@ module Cache = Make_pk_cache (P.Beacon)";
  generate emit_impl stmts oc;
  pp oc "@]@ end@]@\n@."

let generate_types ~in_intf stmts oc =
  pp oc "@[<v 0>";
  pp oc "@ (* Generated by episql. *)\n";
  emit_custom_open oc;
  pp oc "@ @[<v 2>type ('value_r, 'value_d, 'change) persist_patch_in =";
  pp oc "@ [ `Insert of 'value_r * 'value_d";
  pp oc "@ | `Update of 'change list";
  pp oc "@ | `Delete ]@]";
  emit_deriving oc;
  pp oc "@ @[<v 2>type ('value, 'change) persist_patch_out =";
  pp oc "@ [ `Insert of 'value";
  pp oc "@ | `Update of 'change list";
  pp oc "@ | `Delete ]@]";
  emit_deriving oc;
  generate (emit_types ~in_intf) stmts oc;
  pp oc "@."

let generate_intf' stmts oc =
  if go.go_obsolete_order_by then
    printf "warning: Please adapt to -new-order-by, \
            it will become the default.\n";
  generate_intf stmts (formatter_of_out_channel oc)
let generate_impl' stmts oc =
  generate_impl stmts (formatter_of_out_channel oc)
let generate_types' ~in_intf stmts oc =
  generate_types ~in_intf stmts (formatter_of_out_channel oc)

let () =
  let set_types_module mn =
    go.go_types_module <-
      Some (String.capitalize (if Filename.check_suffix mn ".mli"
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
      "Deprecated.";
    "-open", Arg.String (fun m -> go.go_open <- m :: go.go_open),
      "M Open M at top of the generated files but after other open statements.";
    "-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- true),
      "Raise Not_present instead of returning options for state, value, and \
       getters. The exception is also raised for getters of nullable fields \
       for consistency, even though they return options.";
    "-no-raise-on-absent", Arg.Unit (fun () -> go.go_raise_on_absent <- false),
      "Inversion of -raise-on-absent and the default for now.";
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
    "-new-order-by", Arg.Unit (fun () -> go.go_obsolete_order_by <- false),
      " Adapt the new ?order_by specification for generated select statements.";
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
