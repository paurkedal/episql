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

type not_present = {
  operation: string;
  table: string;
}
[@@deriving show]

type conflict = {
  conflict_type: [`Insert_insert | `Update_insert | `Update_delete];
  conflict_table: string;
}
[@@deriving show]

type t = [Caqti_error.t | `Conflict of conflict]
[@@deriving show]

val or_fail : ('a, [< t]) result -> 'a Lwt.t

exception Not_present of not_present
exception Conflict of conflict
