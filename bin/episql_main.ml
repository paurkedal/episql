(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Printf

let disable_keyword kw =
  let kw = String.uppercase_ascii kw in
  if not (Hashtbl.mem Episql.Lexer.keywords kw)
  then eprintf "warning: %s is not a keyword.\n" kw
  else Hashtbl.remove Episql.Lexer.keywords kw

let disable_keywords kws =
  kws |> String.split_on_char ',' |> List.iter disable_keyword

let () =
  let arg_inputs = ref [] in
  let arg_output = ref "-" in
  let arg_generator = ref None in
  let arg_specs = ref [] in
  let gens = String.concat ", " (Episql.generator_names ()) in
  let set_generator gn =
    try
      let generate, subarg_specs = Episql.find_generator gn in
      arg_generator := Some generate;
      arg_specs := Arg.align (!arg_specs @ subarg_specs)
    with Not_found -> raise (Arg.Bad (gn ^ " is not a supported generator")) in
  arg_specs := Arg.align
    [ "-g", Arg.String set_generator,
        "GENERATOR Select generator among: " ^ gens;
      "-o", Arg.Set_string arg_output,
        "PATH Output path.";
      "-disable-keywords", Arg.String disable_keywords,
        "KW,...,KW Disable the given keywords, in case they clash with \
                   column, table, or other names in your schema." ];
  Arg.parse_dynamic arg_specs (fun fp -> arg_inputs := fp :: !arg_inputs)
                    Sys.argv.(0);
  let generator =
    match !arg_generator with
    | None -> fprintf stderr "The -g option is mandatory.\n"; exit 64
    | Some g -> g in
  flush stderr;
  let stmts =
    let add_rev acc fp = List.rev_append (Episql.parse_file fp) acc in
    List.fold_left add_rev [] !arg_inputs |> List.rev
  in
  match !arg_output with
  | "-" -> generator stmts stdout
  | fp -> Prime_io.with_file_out (generator stmts) fp

[@@@ocaml.warning "-33"]
open Gen_caqti_persist
open Gen_shell
open Gen_xml
