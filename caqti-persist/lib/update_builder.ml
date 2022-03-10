(* Copyright (C) 2021--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Prereq
open Unprime_list

type request =
  Request : ('a, unit, Caqti_mult.zero) Caqti_request.t * 'a -> request

type t = {
  mutable latest_index: int;
  mutable params: Params.t;
  mutable rev_settings: (string * int) list;
  mutable rev_conditions: (string * int) list;
}

let create () = {
  latest_index = -1;
  params = Params.empty;
  rev_settings = [];
  rev_conditions = [];
}

let add_param ub (pt, pv) =
  ub.params <- Params.add pt pv ub.params;
  ub.latest_index <- ub.latest_index + 1;
  ub.latest_index

let set ub cn p =
  let i = add_param ub p in
  ub.rev_settings <- (cn, i) :: ub.rev_settings

let where ub cn p =
  let i = add_param ub p in
  ub.rev_conditions <- (cn, i) :: ub.rev_conditions

let finish ~table_name ub =
  if ub.rev_settings = [] then None else
  let () = assert (ub.rev_conditions <> []) in
  let query =
    let open Caqti_query in
    let mk (cn, i) = S[L cn; L" = "; P i] in
    let settings = List.rev_map mk ub.rev_settings in
    let conditions = List.rev_map mk ub.rev_conditions in
    S[L"UPDATE "; L table_name;
      L" SET "; S (List.interfix (L", ") settings);
      L" WHERE "; S (List.interfix (L" AND ") conditions)]
  in
  let Params.V (pt, pv) = ub.params in
  let request =
    Caqti_request.create ~oneshot:true pt Caqti_type.unit Caqti_mult.zero
      (Fun.const query)
  in
  Some (Request (request, pv))
