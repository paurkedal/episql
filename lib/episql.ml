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

let parse_file = Episql_lexer.parse_file

let string_of_qname = function
  | (None, name) -> name
  | (Some ns, name) -> ns ^ "." ^ name

type generator = statement list -> out_channel -> unit
let generators : (string, generator) Hashtbl.t = Hashtbl.create 11
let generate gn = Hashtbl.find generators gn
let register_generator gn g = Hashtbl.add generators gn g
