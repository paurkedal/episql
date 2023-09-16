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

type wd = Wd
type wod = Wod

module Spec : sig
  type 'q t =
    | Done : string -> unit t
    | Field : {
        cn: string;
        ct: 'a Caqti_type.t;
        next: 'q t;
      } -> (('a * wod) * 'q) t
    | Field_default : {
        cn: string;
        ct: 'a Caqti_type.t;
        next: 'q t;
      } -> (('a * wd) * 'q) t
end

module Request : sig
  type (_, _, _) t =
    | Done :
        ('p, unit, Caqti_mult.zero) Caqti_request.t -> (unit, 'p, unit) t
    | Done_default :
        ('p, 'r, Caqti_mult.one) Caqti_request.t -> (unit, 'p, 'r) t
    | Field : {
        set: ('q, 'p * 'a, 'r) t;
      } -> (('a * wod) * 'q, 'p, 'r) t
    | Field_default : {
        set: ('q, 'p * 'a, 'r) t Lazy.t;
        ret: ('q, 'p, 'r * 'a) t Lazy.t;
      } -> (('a * wd) * 'q, 'p, 'r) t

  val create : 'q Spec.t -> ('q, unit, unit) t
end

type (_, _) app =
  | App : {
      request: ('q, 'p, 'r) Request.t;
      param: 'p;
      default: 'r -> 'd;
    } -> ('q, 'd) app

val init : ('q, unit, unit) Request.t -> ('q, unit) app
val ($) : (('a * _) * 'q, 'd) app -> 'a -> ('q, 'd) app
val ($?) : (('a * wd) * 'q, 'd) app -> 'a option -> ('q, 'd * 'a) app
