(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_persist
open Unprime_list

let section = Lwt_log.Section.make "Test_caqti_persist"

module P = struct
  module Cm = struct
    let cache_metric =
      Prime_cache_metric.create
        ~current_time:Unix.time
        ~current_memory_pressure:(fun () -> 1.0)
        ()
  end
  module Beacon = Prime_beacon.Make (Cm)

  let () = Dynlink.allow_unsafe_modules true
  let pool = Caqti_lwt.connect_pool (Uri.of_string "postgresql://")
  let use_db f = Caqti_lwt.Pool.use f pool
end

module M = Schema_one_persist.Make (P)
open M

let test_serial () =
  let%lwt a = Cp_1d_0.create () in
  Cp_1d_0.delete a >>

  let%lwt b = Cp_1d_1o.create () in
  Cp_1d_1o.update ~v0:(Some "updated") b >>
  Cp_1d_1o.patch b `Delete >>

  let%lwt c = Cp_1d_1o.create ~v0:"created" () in
  Cp_1d_1o.delete c >>
  Cp_1d_1o.insert c >>
  Cp_1d_1o.patch c `Delete >>

  let%lwt d = Cp_1d_1r.create ~v0:1.0 () in
  Cp_1d_1r.patch d `Delete >>
  Cp_1d_1r.(patch d (`Insert ({r_v0 = 5.25}, defaults))) >>
  Cp_1d_1r.delete d >>

  let now = CalendarLib.Calendar.now () in
  let%lwt e1 = Cp_1d_1o1r1d.create ~v1:"zap" ~v2:now () in
  let%lwt e2 = Cp_1d_1o1r1d.create ~v1:"paz" ~v2:now () in
  let%lwt e3 = Cp_1d_1o1r1d.create ~v1:"zzz" ~v2:now () in
  begin
    match%lwt Cp_1d_1o1r1d.select ~v2:(`Eq now) ~order_by:[Asc `v2; Desc `v1]
                                  ~limit:3 () with
    | [e1'; e2'; e3'] ->
      assert (e1' == e3);
      assert (e2' == e1);
      assert (e3' == e2);
      Lwt.return_unit
    | _ -> assert false
  end >>
  let e = e1 in
  Lwt_list.iter_s (Cp_1d_1o1r1d.patch e)
    [ `Delete;
      `Insert (Cp_1d_1o1r1d.({r_v1 = "sixty-one"}, defaults));
      `Update [`Set_v0 (Some (Int32.of_int 61))] ] >>
  begin
    let open Cp_1d_1o1r1d in
    let nonpk = state e in
    assert (nonpk.s_v0 = Some (Int32.of_int 61));
    assert (nonpk.s_v1 = "sixty-one");
    Lwt.return_unit
  end >>
  Cp_1d_1o1r1d.delete e

let test_parallel_inner a j =
  if Tensor1.is_present a then
    if Random.bool () then
      Lwt_log.debug ~section "Tensor1.update" >>
      Tensor1.update ~x:(float_of_int j) a
    else
      Lwt_log.debug ~section "Tensor1.delete" >>
      Tensor1.delete a
  else
    Lwt_log.debug ~section "Tensor1.insert" >>
    Tensor1.insert ~x:(float_of_int j) a

let test_parallel i =
  let%lwt a = Tensor1.create ~i:(Int32.of_int i) ~x:0.0 () in
  Lwt.join (List.sample (test_parallel_inner a) 100) >>
  Tensor1.delete a

let main =
  test_serial () >>
  Lwt.join (List.sample test_parallel 100)

let () = Lwt_main.run main
