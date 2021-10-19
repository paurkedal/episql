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

module Params = struct
  type t = V : 'a Caqti_type.t * 'a -> t
  let empty = V (Caqti_type.unit, ())
  let add pt pv (V (pt', pv')) = V (Caqti_type.tup2 pt' pt, (pv', pv))
end

let quote_column column =
  let buf = Buffer.create (String.length column + 2) in
  String.iter
    (fun ch -> if ch = '"' then Buffer.add_char buf '"'; Buffer.add_char buf ch)
    column;
  Caqti_query.L (Buffer.contents buf)
