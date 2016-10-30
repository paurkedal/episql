#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let with_caqti = Conf.with_pkg ~default:true "caqti"
let with_extunix = Conf.with_pkg ~default:true "extunix"
let with_react = Conf.with_pkg ~default:true "react"
let with_macaque = Conf.with_pkg "macaque"

let () = Pkg.describe ~licenses "episql" @@ fun c ->
  let cond_caqti_persist =
       Conf.value c with_caqti
    && Conf.value c with_extunix
    && Conf.value c with_react in
  let cond_macaque = Conf.value c with_macaque in
  Ok [
    Pkg.mllib ~api:["Episql"] "lib/episql.mllib";
    Pkg.mllib ~cond:cond_caqti_persist "lib/caqti-persist.mllib";
    Pkg.bin ~dst:"episql" "bin/episql_main";
    Pkg.test ~cond:cond_caqti_persist "tests/test_caqti_persist";
    Pkg.test ~cond:cond_macaque "tests/schema_one_macaque";
  ]
