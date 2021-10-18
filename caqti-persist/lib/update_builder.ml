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
open Unprime

type state = Init | Set | Where | Noop

type request =
  Request : ('a, unit, Caqti_mult.zero) Caqti_request.t * 'a -> request

type t = {
  buf : Buffer.t;
  mutable params : Params.t;
  mutable state : state;
}

let create _driver_info table_name =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "UPDATE ";
  Buffer.add_string buf table_name;
  Buffer.add_string buf " SET ";
  {buf; params = Params.empty; state = Init}

let assign ub pn (pt, pv) =
  Buffer.add_string ub.buf pn;
  Buffer.add_string ub.buf " = ?";
  ub.params <- Params.add pt pv ub.params

let set ub pn pv =
  begin match ub.state with
  | Init -> ub.state <- Set
  | Set -> Buffer.add_string ub.buf ", "
  | _ -> assert false
  end;
  assign ub pn pv

let where ub pn pv =
  begin match ub.state with
  | Init | Noop -> ub.state <- Noop
  | Set -> Buffer.add_string ub.buf " WHERE "; ub.state <- Where
  | Where -> Buffer.add_string ub.buf " AND "
  end;
  assign ub pn pv

let contents ub =
  let qs = konst (Buffer.contents ub.buf) in
  (match ub.state with
   | Init | Noop -> None
   | Set -> assert false (* Precaution, we don't need unconditional update. *)
   | Where ->
      let Params.V (pt, pv) = ub.params in
      let rt, m = Caqti_type.unit, Caqti_mult.zero in
      Some (Request (Caqti_request.create_p ~oneshot:true pt rt m qs, pv)))
