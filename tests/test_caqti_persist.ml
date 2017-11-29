(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Infix
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
  let pool = Caqti1_lwt.connect_pool (Uri.of_string "postgresql://")
  let use_db f = Caqti1_lwt.Pool.use f pool
end

module M = Schema_one_persist.Make (P)
open M

let test_serial () =
  Cp_1d_0.create () >>= fun a ->
  Cp_1d_0.delete a >>= fun () ->

  Cp_1d_1o.create () >>= fun b ->
  Cp_1d_1o.update ~v0:(Some "updated") b >>= fun () ->
  Cp_1d_1o.patch b `Delete >>= fun () ->

  Cp_1d_1o.create ~v0:"created" () >>= fun c ->
  Cp_1d_1o.delete c >>= fun () ->
  Cp_1d_1o.insert c >>= fun () ->
  Cp_1d_1o.patch c `Delete >>= fun () ->

  Cp_1d_1r.create ~v0:1.0 () >>= fun d ->
  Cp_1d_1r.patch d `Delete >>= fun () ->
  Cp_1d_1r.(patch d (`Insert ({r_v0 = 5.25}, defaults))) >>= fun () ->
  Cp_1d_1r.delete d >>= fun () ->

  let now = CalendarLib.Calendar.now () in
  Cp_1d_1o1r1d.create ~v1:"zzzz" ~v2:now () >>= fun _e0 ->
  Cp_1d_1o1r1d.create ~v1:"zap" ~v2:now () >>= fun e1 ->
  Cp_1d_1o1r1d.create ~v1:"paz" ~v2:now () >>= fun e2 ->
  Cp_1d_1o1r1d.create ~v1:"zzz" ~v2:now () >>= fun e3 ->
  begin
    Cp_1d_1o1r1d.select ~v2:(`Eq now) ~order_by:[Asc `v2; Desc `v1]
                                  ~limit:3 ~offset:1 () >>= function
     | [e1'; e2'; e3'] ->
        assert (e1' == e3);
        assert (e2' == e1);
        assert (e3' == e2);
        Lwt.return_unit
     | _ -> assert false
  end >>= fun () ->
  let e = e1 in
  Lwt_list.iter_s (Cp_1d_1o1r1d.patch e)
    [ `Delete;
      `Insert (Cp_1d_1o1r1d.({r_v1 = "sixty-one"}, defaults));
      `Update [`Set_v0 (Some (Int32.of_int 61))] ] >>= fun () ->
  begin
    let open Cp_1d_1o1r1d in
    let nonpk = state e in
    assert (nonpk.s_v0 = Some (Int32.of_int 61));
    assert (nonpk.s_v1 = "sixty-one");
    Lwt.return_unit
  end >>= fun () ->
  Cp_1d_1o1r1d.delete e

let test_parallel_inner a j =
  if Tensor1.is_present a then
    if Random.bool () then
      Lwt_log.debug ~section "Tensor1.update" >>= fun () ->
      Tensor1.update ~x:(float_of_int j) a
    else
      Lwt_log.debug ~section "Tensor1.delete" >>= fun () ->
      Tensor1.delete a
  else
    Lwt_log.debug ~section "Tensor1.insert" >>= fun () ->
    Tensor1.insert ~x:(float_of_int j) a

let test_parallel i =
  Tensor1.create ~i:(Int32.of_int i) ~x:0.0 () >>= fun a ->
  Lwt.join (List.sample (test_parallel_inner a) 100) >>= fun () ->
  Tensor1.delete a

let main =
  test_serial () >>= fun () ->
  Lwt.join (List.sample test_parallel 100)

let () = Lwt_main.run main
