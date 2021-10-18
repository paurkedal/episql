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

type t

type request =
  Request : ('a, unit, Caqti_mult.zero) Caqti_request.t * 'a -> request

val create : Caqti_driver_info.t -> string -> t
val set : t -> string -> 'a Caqti_type.t * 'a -> unit
val where : t -> string -> 'a Caqti_type.t * 'a -> unit
val contents : t -> request option
