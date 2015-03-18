(* OASIS_START *)
(* OASIS_STOP *)

let episql_prog = "bin/episql_main.native"

let episql ?types gen sql ml env build =
  let sql = env sql and ml = env ml in
  let types =
    match types with
    | None -> []
    | Some types -> [A"-t"; P(String.capitalize (env types))] in
  let tags = Tags.union (tags_of_pathname sql) (tags_of_pathname ml) in
  Cmd (S [P episql_prog; T (tags ++ "episql"); A"-g"; A gen; S types;
	  P "-raise-on-absent";
          P sql; A"-o"; Px ml])
let () =
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
	       "caqti-persist-ml" "tests/%.sql" "tests/%_persist.ml")

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
