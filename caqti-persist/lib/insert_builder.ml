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

open Unprime
open Unprime_list

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

  let rec create'
    : type q pt rt. (pt Caqti_type.t * _ * rt Caqti_type.t * _) -> q Spec.t ->
      (q, pt, rt) t =
    fun (pt, pcns, rt, rcns) ->
    (function
     | Spec.Done tn ->
        let pcns = List.rev pcns in
        let rcns = List.rev rcns in
        let query =
          let open Caqti_query in
          let values =
            if pcns = [] then
              L" DEFAULT VALUES"
            else
              let cols = List.map quote_column pcns in
              let vals = List.mapi (fun i _ -> P i) pcns in
              S[L" ("; S (List.interfix (L", ") cols); L") VALUES (";
                S (List.interfix (L", ") vals); L")"]
          in
          S[L"INSERT INTO "; L tn; values]
        in
        (match rt with
         | Caqti_type.Unit ->
            Done (Caqti_request.create pt rt Caqti_mult.zero (Fun.const query))
         | Caqti_type.Tup2 (_, _) ->
            let query =
              let open Caqti_query in
              S(query :: L" RETURNING " ::
                List.interfix (L", ") (List.map (fun cn -> L cn) rcns))
            in
            Done_default
              (Caqti_request.create pt rt Caqti_mult.one (Fun.const query))
         | _ -> assert false)
     | Spec.Field {cn; ct; next} ->
        let set =
          (create' (Caqti_type.(tup2 pt ct), cn :: pcns, rt, rcns) next) in
        Field {set}
     | Spec.Field_default {cn; ct; next} ->
        let set = lazy
          (create' (Caqti_type.(tup2 pt ct), cn :: pcns, rt, rcns) next) in
        let ret = lazy
          (create' (pt, pcns, Caqti_type.(tup2 rt ct), cn :: rcns) next) in
        Field_default {set; ret})

  let create spec = create' (Caqti_type.unit, [], Caqti_type.unit, []) spec
end

type (_, _) app =
  | App : {
      request: ('q, 'p, 'r) Request.t;
      param: 'p;
      default: 'r -> 'd;
    } -> ('q, 'd) app

let init request = App {request; param = (); default = ident}

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
