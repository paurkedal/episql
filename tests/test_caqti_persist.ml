(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
  lwt a = Cp_1d_0.create () in
  Cp_1d_0.delete a >>

  lwt b = Cp_1d_1o.create () in
  Cp_1d_1o.update ~v0:(Some "updated") b >>
  Cp_1d_1o.patch b `Delete >>

  lwt c = Cp_1d_1o.create ~v0:"created" () in
  Cp_1d_1o.delete c >>
  Cp_1d_1o.insert c >>
  Cp_1d_1o.patch c `Delete >>

  lwt d = Cp_1d_1r.create ~v0:1.0 () in
  Cp_1d_1r.patch d `Delete >>
  Cp_1d_1r.(patch d (`Insert ({r_v0 = 5.25}, defaults))) >>
  Cp_1d_1r.delete d >>

  let now = CalendarLib.Calendar.now () in
  lwt e = Cp_1d_1o1r1d.create ~v1:"zap" ~v2:now () in
  begin match_lwt Cp_1d_1o1r1d.select ~v2:(`Eq now) () with
  | [e'] -> assert (e == e'); Lwt.return_unit
  | _ -> assert false
  end >>
  Lwt_list.iter_s (Cp_1d_1o1r1d.patch e)
    [ `Delete;
      `Insert (Cp_1d_1o1r1d.({r_v1 = "sixty-one"}, defaults));
      `Update [`Set_v0 (Some (Int32.of_int 61))] ] >>
  begin match Cp_1d_1o1r1d.get_nonpk e with
  | None -> assert false
  | Some nonpk ->
    let open Cp_1d_1o1r1d in
    assert (nonpk.s_v0 = Some (Int32.of_int 61));
    assert (nonpk.s_v1 = "sixty-one");
    Lwt.return_unit
  end >>
  Cp_1d_1o1r1d.delete e

let main =
  test_serial () >>
  Lwt.return_unit

let () = Lwt_main.run main
