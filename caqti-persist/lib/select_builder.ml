(* Copyright (C) 2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Prereq
open Printf
open Unprime

open Types

type query_fragment =
  | S : string -> query_fragment
  | P : 'a Caqti_type.t * 'a -> query_fragment

type state = Init | Ret | Where | Order_by | Final

type t = {
  buf: Buffer.t;
  table_name: string;
  mutable params: Params.t;
  mutable state: state;
}

type _ request =
  Request :
    ('a, 'b, Caqti_mult.zero_or_more) Caqti_request.t * 'a -> 'b request

let create _driver_info table_name =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "SELECT ";
  {buf; table_name; params = Params.empty; state = Init}

let ret sb pn =
  match sb.state with
  | Init -> sb.state <- Ret; Buffer.add_string sb.buf pn
  | Ret -> Buffer.add_string sb.buf ", "; Buffer.add_string sb.buf pn
  | Where | Order_by | Final -> assert false

let emit_from sb =
  Buffer.add_string sb.buf " FROM ";
  Buffer.add_string sb.buf sb.table_name

let where sb qfs =
  begin match sb.state with
  | Init | Order_by | Final -> assert false
  | Ret ->
    emit_from sb;
    Buffer.add_string sb.buf " WHERE ";
    sb.state <- Where
  | Where ->
    Buffer.add_string sb.buf " AND "
  end;
  qfs |> List.iter begin function
   | S s -> Buffer.add_string sb.buf s
   | P (pt, pv) ->
      Buffer.add_string sb.buf "?";
      sb.params <- Params.add pt pv sb.params
  end

let order_by sb f order_item =
  (match sb.state with
   | Init | Final -> assert false
   | Ret | Where ->
      if sb.state = Ret then emit_from sb;
      sb.state <- Order_by;
      Buffer.add_string sb.buf " ORDER BY "
   | Order_by ->
      Buffer.add_string sb.buf ", ");
  (match order_item with
   | Asc col -> bprintf sb.buf "\"%s\"" (f col)
   | Desc col -> bprintf sb.buf "\"%s\" DESC" (f col)
   | Asc_sql sql -> Buffer.add_string sb.buf sql
   | Desc_sql sql -> bprintf sb.buf "%s DESC" sql)

let limit sb n =
  (match sb.state with
   | Init -> assert false
   | Ret -> emit_from sb
   | Where | Order_by | Final -> ());
  bprintf sb.buf " LIMIT %d" n;
  sb.state <- Final

let offset sb n =
  (match sb.state with
   | Init -> assert false
   | Ret -> emit_from sb
   | Where | Order_by | Final -> ());
  bprintf sb.buf " OFFSET %d" n;
  sb.state <- Final

let contents sb rt =
  (match sb.state with
   | Init -> assert false
   | Ret ->
      Buffer.add_string sb.buf " FROM ";
      Buffer.add_string sb.buf sb.table_name
   | Where | Order_by | Final -> ());
  let qs = Buffer.contents sb.buf in
  let Params.V (pt, pv) = sb.params in
  let m = Caqti_mult.zero_or_more in
  Request (Caqti_request.create_p ~oneshot:true pt rt m (konst qs), pv)
