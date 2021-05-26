(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Lwt.return r

module Result_list_lwt = struct

  let rec iter_s f = function
   | [] -> Lwt.return_ok ()
   | x :: xs -> f x >>=? fun () -> iter_s f xs

  let rec join = function
   | [] -> Lwt.return_ok ()
   | m :: ms -> m >>=? fun () -> join ms

end

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
  let pool =
    (match Caqti_lwt.connect_pool (Uri.of_string "postgresql://") with
     | Ok pool -> pool
     | Error err -> raise (Caqti_error.Exn err))
  let use_db f =
    let aux c =
      Lwt.catch
        (fun () -> f c >|= fun y -> Ok y)
        (fun exn -> Lwt.return_error (`Exn exn)) in
    Caqti_lwt.Pool.use aux pool >>= function
     | Ok y -> Lwt.return y
     | Error (`Exn exn) -> Lwt.fail exn
     | Error (#Caqti_error.t as err) -> Lwt.fail (Caqti_error.Exn err)
end

module M = Schema_one_persist.Make (P)
open M

let test_serial () =
  Cp_1d_0.create () >>=? fun a ->
  Cp_1d_0.select ~k0:(`Eq (Cp_1d_0.get_k0 a)) () >>=? fun a' ->
  assert (List.length a' = 1);
  Cp_1d_0.fetch (Cp_1d_0.get_k0 a) >>=? fun a'' ->
  assert (Cp_1d_0.is_present a'');
  Cp_1d_0.uncache_key (Cp_1d_0.get_k0 a);
  Cp_1d_0.uncache a;
  Cp_1d_0.uncache_all ();
  Cp_1d_0.delete a >>=? fun () ->

  Cp_1d_1o.create () >>=? fun b ->
  Cp_1d_1o.update ~v0:(Some "updated") b >>=? fun () ->
  Cp_1d_1o.patch b `Delete >>=? fun () ->

  Cp_1d_1o.create ~v0:"created" () >>=? fun c ->
  Cp_1d_1o.delete c >>=? fun () ->
  Cp_1d_1o.insert c >>=? fun () ->
  Cp_1d_1o.patch c `Delete >>=? fun () ->

  Cp_1d_1r.create ~v0:1.0 () >>=? fun d ->
  Cp_1d_1r.patch d `Delete >>=? fun () ->
  Cp_1d_1r.(patch d (`Insert ({r_v0 = 5.25}, defaults))) >>=? fun () ->
  Cp_1d_1r.delete d >>=? fun () ->

  let now = CalendarLib.Calendar.now () in
  Cp_1d_1o1r1d.create ~v1:"zzzz" ~v2:(Some now) () >>=? fun _e0 ->
  Cp_1d_1o1r1d.create ~v1:"zap" ~v2:(Some now) () >>=? fun e1 ->
  Cp_1d_1o1r1d.create ~v1:"paz" ~v2:(Some now) () >>=? fun e2 ->
  Cp_1d_1o1r1d.create ~v1:"zzz" ~v2:(Some now) () >>=? fun e3 ->
  begin
    Cp_1d_1o1r1d.select
      ~v2:(`Eq now) ~order_by:[Asc `v2; Desc `v1] ~limit:3 ~offset:1 () >>=?
    (function
     | [e1'; e2'; e3'] ->
        assert (e1' == e3);
        assert (e2' == e1);
        assert (e3' == e2);
        Lwt.return_ok ()
     | _ ->
        assert false)
  end >>=? fun () ->
  let e = e1 in
  Result_list_lwt.iter_s (Cp_1d_1o1r1d.patch e)
    [ `Delete;
      `Insert (Cp_1d_1o1r1d.({r_v1 = "sixty-one"}, defaults));
      `Update [`Set_v0 (Some (Int32.of_int 61))] ] >>=? fun () ->
  begin
    let open Cp_1d_1o1r1d in
    let nonpk = state e in
    assert (nonpk.s_v0 = Some (Int32.of_int 61));
    assert (nonpk.s_v1 = "sixty-one");
    Lwt.return_ok ()
  end >>=? fun () ->
  Cp_1d_1o1r1d.delete e >>=? fun () ->

  Tensor2.create ~i:1l ~j:2l ~x:33.5 () >>=? fun t2' ->
  Tensor2.select ~i:(`Eq 1l) ~j:(`Eq 2l) () >>=? fun t2s ->
  begin
    assert (List.length t2s = 1);
    let t2 = List.hd t2s in
    assert (Tensor2.is_present t2);
    assert (Tensor2.get_i t2 = 1l);
    assert (Tensor2.get_j t2 = 2l);
    assert (Tensor2.get_x t2 = 33.5);
    Tensor2.delete t2'
  end

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
  Tensor1.create ~i:(Int32.of_int i) ~x:0.0 () >>=? fun a ->
  Result_list_lwt.join (List.sample (test_parallel_inner a) 100) >>=? fun () ->
  Tensor1.delete a

let main =
  begin
    test_serial () >>=? fun () ->
    Result_list_lwt.join (List.sample test_parallel 100)
  end >>= Caqti_persist.or_fail

let () = Lwt_main.run main

module Also_compile = struct
  include Schema_one_persist_alt_types
  include Schema_one_persist_alt
end
