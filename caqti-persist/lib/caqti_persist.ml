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

include Types
module Error = Error

(**/**)

module Caqti_persist_internal = struct
  module Pk_cache = Pk_cache

  module Ib = Insert_builder
  module Ub = Update_builder
  module Sb = Select_builder

  let (>>=?) = Lwt_result.(>>=)
  let (>|=?) = Lwt_result.(>|=)

  let qualify ns name =
    (match ns with
     | None -> name
     | Some ns -> ns ^ "." ^ name)
end
