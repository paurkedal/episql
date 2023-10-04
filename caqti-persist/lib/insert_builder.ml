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

type wd = Wd
type wod = Wod

module Spec = struct
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

module Request = struct
  type (_, _, _) t =
    | Done :
        ('p, unit, Caqti_mult.zero) Caqti_request.t -> (unit, 'p, unit) t
    | Done_default :
        ('p, 'a * 'r, Caqti_mult.one) Caqti_request.t -> (unit, 'p, 'a * 'r) t
    | Field : {
        set: ('q, 'p * 'a, 'r) t;
      } -> (('a * wod) * 'q, 'p, 'r) t
    | Field_default : {
        set: ('q, 'p * 'a, 'r) t Lazy.t;
        ret: ('q, 'p, 'r * 'a) t Lazy.t;
      } -> (('a * wd) * 'q, 'p, 'r) t

  let build_values =
    let open Caqti_query in
    (function
     | [] -> L" DEFAULT VALUES"
     | pcns ->
        let cols = List.map quote_column pcns in
        let vals = List.mapi (fun i _ -> P i) pcns in
        S[L" ("; concat ", " cols; L") VALUES ("; concat ", " vals; L")"])

  type 'a columns = 'a Caqti_type.t * string list

  let rec create''
    : type q p a r.
      p columns -> (a * r) columns -> q Spec.t -> (q, p, (a * r)) t =
    fun (pt, pcns) (rt, rcns) ->
    (function
     | Spec.Done tn ->
        let pcns = List.rev pcns in
        let rcns = List.rev rcns in
        let q =
          let open Caqti_query in
          S[L"INSERT INTO "; L tn; build_values pcns;
            L" RETURNING "; concat ", " (List.map (fun cn -> L cn) rcns)]
        in
        let q _ = q in
        Done_default (Caqti_request.create pt rt Caqti_mult.one q)
     | Spec.Field {cn; ct; next} ->
        let set =
          (create'' (Caqti_type.(t2 pt ct), cn :: pcns) (rt, rcns) next) in
        Field {set}
     | Spec.Field_default {cn; ct; next} ->
        let set = lazy
          (create'' (Caqti_type.(t2 pt ct), cn :: pcns) (rt, rcns) next) in
        let ret = lazy
          (create'' (pt, pcns) (Caqti_type.(t2 rt ct), cn :: rcns) next) in
        Field_default {set; ret})

  let rec create' : type q p. p columns -> q Spec.t -> (q, p, unit) t =
    fun (pt, pcns) ->
    (function
     | Spec.Done tn ->
        let pcns = List.rev pcns in
        let q = Caqti_query.(S[L"INSERT INTO "; L tn; build_values pcns]) in
        let q _ = q in
        Done (Caqti_request.create pt Caqti_type.unit Caqti_mult.zero q)
     | Spec.Field {cn; ct; next} ->
        let set = create' (Caqti_type.(t2 pt ct), cn :: pcns) next in
        Field {set}
     | Spec.Field_default {cn; ct; next} ->
        let set = lazy
          (create' (Caqti_type.(t2 pt ct), cn :: pcns) next) in
        let ret = lazy
          (create'' (pt, pcns) (Caqti_type.(t2 unit ct), [cn]) next) in
        Field_default {set; ret})

  let create spec = create' (Caqti_type.unit, []) spec
end

type (_, _) app =
  | App : {
      request: ('q, 'p, 'r) Request.t;
      param: 'p;
      default: 'r -> 'd;
    } -> ('q, 'd) app

let init request = App {request; param = (); default = Fun.id}

let ($) : type a ad q d. ((a * ad) * q, d) app -> a -> (q, d) app =
  fun request arg ->
  (match request with
   | App {request = Request.Field {set; _}; param; default} ->
      App {request = set; param = (param, arg); default}
   | App {request = Request.Field_default {set; _}; param; default} ->
      App {request = Lazy.force set; param = (param, arg); default})

let ($?) : type a q d. ((a * wd) * q, d) app -> a option -> (q, d * a) app =
  fun request arg ->
  (match request with
   | App {request = Request.Field_default {set; ret; _}; param; default} ->
      (match arg with
       | Some arg ->
          let default rs = (default rs, arg) in
          App {request = Lazy.force set; param = (param, arg); default}
       | None ->
          let default (rs, r) = (default rs, r) in
          App {request = Lazy.force ret; param; default}))
