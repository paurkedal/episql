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

type 'a presence =
  | Absent
  | Inserting of 'a Lwt_condition.t
  | Present of 'a
  | Deleting of unit Lwt_condition.t

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let cache_hertz = Int64.to_float ExtUnixSpecific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
let fetch_grade = 1e-3 *. cache_second

module type PK_CACHABLE = sig
  type pk
  type nonpk
  val fetch : pk -> nonpk option Lwt.t
end

module type PK_CACHED = sig
  type pk
  type nonpk
  type beacon
  type t = {pk : pk; mutable nonpk : nonpk presence; beacon : beacon}
  val find : pk -> t option
  val make : pk -> t Lwt.t
  val merge : pk -> nonpk option -> t
end

module Make_pk_cache (Beacon : Prime_beacon.S) (P : PK_CACHABLE) = struct

  type t = {
    pk : P.pk;
    mutable nonpk : P.nonpk presence;
    beacon : Beacon.t;
  }

  module W = Weak.Make (struct
    type tmp = t type t = tmp
    let equal a b = a.pk = b.pk
    let hash a = Hashtbl.hash a.pk
  end)

  let cache = W.create 17

  let find pk =
    try Some (W.find cache {pk; nonpk = Absent; beacon = Beacon.dummy})
    with Not_found -> None

  let make pk =
    try
      Lwt.return (W.find cache {pk; nonpk = Absent; beacon = Beacon.dummy})
    with Not_found ->
      P.fetch pk >|= fun nonpk ->
      let nonpk = match nonpk with None -> Absent | Some x -> Present x in
      Beacon.embed fetch_grade (fun beacon -> W.merge cache {pk; nonpk; beacon})

  let merge pk nonpk =
    let nonpk = match nonpk with None -> Absent | Some x -> Present x in
    W.merge cache (Beacon.embed fetch_grade (fun beacon -> {pk; nonpk; beacon}))
end
