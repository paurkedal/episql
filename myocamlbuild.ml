open Ocamlbuild_plugin

let episql_prog = "bin/episql_main.native"

let episql ?types gen sql ml env build =
  let sql = env sql and ml = env ml in
  let types =
    match types with
    | None -> []
    | Some types -> [A"-t"; P(String.capitalize (env types))] in
  let tags = Tags.union (tags_of_pathname sql) (tags_of_pathname ml) in
  Cmd (S [P episql_prog; T (tags ++ "episql"); A"-g"; A gen; S types;
          A "-new-order-by";
          A "-raise-on-absent"; A "-connection-arg"; A "c";
          A "-ppx-deriving"; A "show";
          A "-pk-module"; A"Key";
          P sql; A"-o"; Px ml])

let episql_macaque sql ml env build =
  let sql = env sql and ml = env ml in
  let tags = Tags.union (tags_of_pathname sql) (tags_of_pathname ml) in
  Cmd (S [P episql_prog; T (tags ++ "episql"); A"-g"; A "macaque";
          P sql; A"-o"; Px ml])

let () =
  mark_tag_used "tests";
  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    (fun env _ -> cp (env "%.mli") (env "%.ml"));
  rule ".sql -> _persist_types.mli"
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist_types.mli"
       (episql "caqti-persist-types-mli" "tests/%.sql"
               "tests/%_persist_types.mli");
  rule ".sql -> _persist_types.ml"
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist_types.ml"
       (episql "caqti-persist-types-ml" "tests/%.sql"
               "tests/%_persist_types.ml");
  rule ".sql -> _persist.mli"
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist.mli"
       (episql ~types:"%_persist_types"
               "caqti-persist-mli" "tests/%.sql" "tests/%_persist.mli");
  rule ".sql -> _persist.ml"
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist.ml"
       (episql ~types:"%_persist_types"
               "caqti-persist-ml" "tests/%.sql" "tests/%_persist.ml");
  rule ".sql -> _macaque.ml"
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_macaque.ml"
       (episql_macaque "tests/%.sql" "tests/%_macaque.ml")

let () = dispatch begin function
  | Before_options ->
    Options.use_ocamlfind := true
  | After_rules ->
    flag ["ocamlyacc"] & S[A"-v"];
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"Tool to derive code from SQL schemas"];
    ocaml_lib ~dir:"lib" "lib/episql";
    ocaml_lib ~dir:"lib" "lib/caqti-persist"
  | _ -> ()
end
