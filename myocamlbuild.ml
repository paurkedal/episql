(* OASIS_START *)
(* OASIS_STOP *)

let episql_prog = "bin/episql_main.native"

let episql gen sql ml env build =
  let sql = env sql and ml = env ml in
  let tags = Tags.union (tags_of_pathname sql) (tags_of_pathname ml) in
  Cmd (S [P episql_prog; T (tags ++ "episql"); A"-g"; A gen;
          P sql; A"-o"; Px ml])
let () =
  rule ".sql -> _persist.mli" ~tags:["episql"]
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist.mli"
       (episql "caqti-persist-mli" "tests/%.sql" "tests/%_persist.mli");
  rule ".sql -> _persist.ml" ~tags:["episql"]
       ~deps:["tests/%.sql"; episql_prog] ~prod:"tests/%_persist.ml"
       (episql "caqti-persist-ml" "tests/%.sql" "tests/%_persist.ml")

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    flag ["ocamlyacc"] & S[A"-v"];
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"Tool to derive code from SQL schemas"];
    dispatch_default e

  | e ->
    dispatch_default e

end
