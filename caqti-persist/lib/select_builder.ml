(* Copyright (C) 2021--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Types

type query_fragment =
  | L : string -> query_fragment
  | P : 'a Caqti_type.t * 'a -> query_fragment

type t = {
  mutable latest_index: int;
  mutable params: Params.t;
  mutable rev_where_list: Caqti_query.t list;
}

type 'a predicate = [
  | `Is_null | `Is_not_null
  | 'a order_predicate
  | `Like of 'a
  | `Ilike of 'a
]

type _ request =
  Request :
    ('a, 'b, Caqti_mult.zero_or_more) Caqti_request.t * 'a -> 'b request

let create () =
  {latest_index = -1; params = Params.empty; rev_where_list = []}

let add_param sb (pt, pv) =
  sb.params <- Params.add pt pv sb.params;
  sb.latest_index <- sb.latest_index + 1;
  sb.latest_index

let where' sb qfs =
  let recode = function
   | L s -> Caqti_query.L s
   | P (pt, pv) -> let i = add_param sb (pt, pv) in Caqti_query.P i
  in
  let where_item = List.map recode qfs in
  sb.rev_where_list <- S where_item :: sb.rev_where_list

let where
  : type a. t -> string -> a Caqti_type.t -> a predicate -> unit =
  fun sb cn t ->
  (function
   | `Is_null -> where' sb [L cn; L" IS NULL"]
   | `Is_not_null -> where' sb [L cn; L" IS NOT NULL"]
   | `Eq x -> where' sb [L cn; L" = "; P (t, x)]
   | `Ne x -> where' sb [L cn; L" <> "; P (t, x)]
   | `Lt x -> where' sb [L cn; L" < "; P (t, x)]
   | `Gt x -> where' sb [L cn; L" > "; P (t, x)]
   | `Le x -> where' sb [L cn; L" <= "; P (t, x)]
   | `Ge x -> where' sb [L cn; L" >= "; P (t, x)]
   | `Between (x, y) -> where' sb [L cn; L" BETWEEN "; P (t, x); L" AND "; P (t, y)]
   | `Not_between (x, y) -> where' sb [L cn; L" NOT BETWEEN "; P (t, x); L" AND "; P (t, y)]
   | `In xs ->
      let xs' = List.map (fun x -> P (t, x)) xs in
      where' sb ([L cn; L" IN ("] @ Prime_list.interfix (L", ") xs' @ [L")"])
   | `Like x -> where' sb [L cn; L" LIKE "; P (t, x)]
   | `Ilike x -> where' sb [L cn; L" ILIKE "; P (t, x)])

let where sb cn t p = where sb cn t (p : [< 'a predicate] :> 'a predicate)

let finish
      ~table_name ~select_list ~select_type
      ~order_column_name ~order_by ?limit ?offset sb =
  let query =
    let open Caqti_query in
    let select_clause =
      let select_list = List.map (fun cn -> L cn) select_list in
      S[L"SELECT "; concat ", " select_list];
    in
    let where_clause =
      if sb.rev_where_list = [] then S[] else
      S[L" WHERE "; concat " AND " (List.rev sb.rev_where_list)]
    in
    let order_by_clause =
      if order_by = [] then S[] else
      let to_sql = function
       | Asc col -> quote_column (order_column_name col)
       | Desc col -> S[quote_column (order_column_name col); L" DESC"]
       | Asc_sql sql -> L sql
       | Desc_sql sql -> S[L sql; L" DESC"]
      in
      let order_list = List.rev_map to_sql order_by in
      S[L" ORDER BY "; concat ", " order_list]
    in
    let limit_clause =
      (match limit with
       | None -> S[]
       | Some limit -> S[L" LIMIT "; L (string_of_int limit)])
    in
    let offset_clause =
      (match offset with
       | None -> S[]
       | Some offset -> S[L" OFFSET "; L (string_of_int offset)])
    in
    S[select_clause; L" FROM "; L table_name;
      where_clause; order_by_clause; limit_clause; offset_clause]
  in
  let Params.V (pt, pv) = sb.params in
  let request =
    Caqti_request.create ~oneshot:true pt select_type Caqti_mult.zero_or_more
      (Fun.const query)
  in
  Request (request, pv)
