(* Copyright (C) 2014--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

{
  open Episql_parser
  open Printf

  type dialect_traits = {
    dt_has_nested_comments : bool;
  }
  let dialect_traits_of_dialect = function
    | `Pgsql -> { dt_has_nested_comments = true }
    | _ -> { dt_has_nested_comments = false }

  let keywords = Hashtbl.create 31
  let () =
    List.iter (fun (kw, token) -> Hashtbl.add keywords kw token)
      [ "AND", (fun _ -> L8 "AND");
        "AS", (fun idr -> AS idr);
        "AT", (fun idr -> AT idr);
        "BY", (fun idr -> BY idr);
        "CACHE", (fun idr -> CACHE idr);
        "CASCADE", (fun idr -> CASCADE idr);
        "CHECK", (fun idr -> CHECK idr);
        "COLLATE", (fun idr -> COLLATE idr);
        "CONSTRAINT", (fun idr -> CONSTRAINT idr);
        "CREATE", (fun idr -> CREATE idr);
        "CYCLE", (fun idr -> CYCLE idr);
        "DEFAULT", (fun idr -> DEFAULT idr);
        "DELETE", (fun idr -> DELETE idr);
        "DROP", (fun idr -> DROP idr);
        "ENUM", (fun idr -> ENUM idr);
        "EXISTS", (fun idr -> EXISTS idr);
        "FOREIGN", (fun idr -> FOREIGN idr);
        "IF", (fun idr -> IF idr);
        "ILIKE", (fun idr -> R4 idr);
        "IN", (fun idr -> R4 idr);
        "INCREMENT", (fun idr -> INCREMENT idr);
        "INHERIT", (fun idr -> INHERIT idr);
        "IS", (fun idr -> R0 idr); (* TODO: Right operand. *)
        "KEY", (fun idr -> KEY idr);
        "LIKE", (fun idr -> R4 idr);
        "MINVALUE", (fun idr -> MINVALUE idr);
        "MAXVALUE", (fun idr -> MAXVALUE idr);
        "NO", (fun idr -> NO idr);
        "NOT", (fun idr -> NOT idr);
        "NULL", (fun idr -> NULL idr);
        "ON", (fun idr -> ON idr);
        "OR", (fun _ -> L6 "OR");
        "PRIMARY", (fun idr -> PRIMARY idr);
        "REFERENCES", (fun idr -> REFERENCES idr);
        "RESTRICT", (fun idr -> RESTRICT idr);
        "UNIQUE", (fun idr -> UNIQUE idr);
        "SCHEMA", (fun idr -> SCHEMA idr);
        "SEQUENCE", (fun idr -> SEQUENCE idr);
        "START", (fun idr -> START idr);
        "TABLE", (fun idr -> TABLE idr);
        "TEMP", (fun idr -> TEMPORARY idr);
        "TEMPORARY", (fun idr -> TEMPORARY idr);
        "TIME", (fun idr -> TIME idr);
        "TYPE", (fun idr -> TYPE idr);
        "UNLOGGED", (fun idr -> UNLOGGED idr);
        "UPDATE", (fun idr -> UPDATE idr);
        "WITH", (fun idr -> WITH idr);
        "ZONE", (fun idr -> ZONE idr);
        "YEAR", (fun idr -> YEAR idr);
        "MONTH", (fun idr -> MONTH idr);
        "DAY", (fun idr -> DAY idr);
        "HOUR", (fun idr -> HOUR idr);
        "MINUTE", (fun idr -> MINUTE idr);
        "SECOND", (fun idr -> SECOND idr);
        "SIMILAR", (fun idr -> R4 idr);
        "TO", (fun idr -> TO idr);

        (* Type-forming keywords *)
        "BOOLEAN", (fun idr -> BOOLEAN idr);
        "REAL", (fun idr -> REAL idr);
        "FLOAT", (fun idr -> FLOAT idr);
        "DOUBLE", (fun idr -> DOUBLE idr);
        "PRECISION", (fun idr -> PRECISION idr);
        "CHAR", (fun idr -> CHAR idr);
        "VARCHAR", (fun idr -> VARCHAR idr);
        "TEXT", (fun idr -> TEXT idr);
        "BYTEA", (fun idr -> BYTEA idr);
        "SMALLINT", (fun idr -> SMALLINT idr);
        "INTEGER", (fun idr -> INTEGER idr);
        "BIGINT", (fun idr -> BIGINT idr);
        "SMALLSERIAL", (fun idr -> SMALLSERIAL idr);
        "SERIAL", (fun idr -> SERIAL idr);
        "BIGSERIAL", (fun idr -> BIGSERIAL idr);
        "NUMERIC", (fun idr -> NUMERIC idr);
        "DECIMAL", (fun idr -> DECIMAL idr);
        "TIME", (fun idr -> TIME idr);
        "DATE", (fun idr -> DATE idr);
        "TIMESTAMP", (fun idr -> TIMESTAMP idr);
        "TIMEZONE", (fun idr -> TIMEZONE idr);
        "INTERVAL", (fun idr -> INTERVAL idr); ]
}

let digit = ['0'-'9']
let uint = digit+
let ufix = (digit+ '.' digit* | '.' digit+)
let wordfst = ['a'-'z' 'A'-'Z' '_']
let wordcnt = ['a'-'z' 'A'-'Z' '_' '0'-'9']

rule lex_main dt = parse
  | "--" [^ '\n']* | [' ' '\t']+ { lex_main dt lexbuf }
  | "/*" { lex_cstyle_comment dt (Lexing.lexeme_start_p lexbuf) lexbuf;
           lex_main dt lexbuf; }
  | '\n' { Lexing.new_line lexbuf; lex_main dt lexbuf }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | ("=" | "<>" | "!=" | "<=" | "<" | ">=" | ">") as op { R2 op }
  | "||" as op { A0 op }
  | '-' { MINUS }
  | ("+") as op { A2 (String.make 1 op) }
  | ("*" | "/" | "%") as op { A4 (String.make 1 op) }
  | ("^") as op { A6 (String.make 1 op) }
  | '.' { DOT }
  | "::" { CAST }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | wordfst wordcnt* as word
    { try Hashtbl.find keywords (String.uppercase_ascii word) word
      with Not_found -> IDENTIFIER word }
  | "'" { lex_string (Buffer.create 64) lexbuf }
  | '"' { lex_identifier (Buffer.create 32) lexbuf }
  | uint as s { INT (Int64.of_string s) }
  | uint  'e' ['-' '+']? uint   as s { FLT (float_of_string s) }
  | ufix ('e' ['-' '+']? uint)? as s { FLT (float_of_string s) }
  | eof { EOF }
  | _ as c
    { let open Lexing in
      let lxm = lexeme_start_p lexbuf in
      ksprintf failwith "%s:%d,%d: Lexical error at '%c'."
               lxm.pos_fname lxm.pos_lnum (lxm.pos_cnum - lxm.pos_bol) c; }
and lex_string buf = parse
  | "''" { Buffer.add_char buf '\''; lex_string buf lexbuf }
  | "'" { STRING (Buffer.contents buf) }
  | [^'\'']+ as s { Buffer.add_string buf s; lex_string buf lexbuf }
and lex_identifier buf = parse
  | "\"\"" { Buffer.add_char buf '"'; lex_identifier buf lexbuf }
  | '"' { IDENTIFIER (Buffer.contents buf) }
  | [^'"']+ as s { Buffer.add_string buf s; lex_identifier buf lexbuf }
and lex_cstyle_comment dt lxm = parse
  | "/*"
    { lex_cstyle_comment dt lxm lexbuf;
      if dt.dt_has_nested_comments then
        lex_cstyle_comment dt lxm lexbuf; }
  | "*/" { () }
  | '\n' { Lexing.new_line lexbuf; lex_cstyle_comment dt lxm lexbuf; }
  | [^'/' '*' '\n']+ { lex_cstyle_comment dt lxm lexbuf; }
  | '/' | '*' { lex_cstyle_comment dt lxm lexbuf; }
  | eof
    { let open Lexing in
      ksprintf failwith "%s:%d,%d: Unmatched \"/*\"."
               lxm.pos_fname lxm.pos_lnum (lxm.pos_cnum - lxm.pos_bol); }

{
  open Lexing

  let parse_lexbuf ?(dialect = `Sql) lexbuf =
    schema (lex_main (dialect_traits_of_dialect dialect)) lexbuf

  let report_error lexbuf msg =
    let lxm = lexeme_start_p lexbuf in
    Printf.ksprintf failwith "%s:%d,%d: %s" lxm.pos_fname
                    lxm.pos_lnum (lxm.pos_cnum - lxm.pos_bol) msg

  let parse_file ?dialect path =
    let ic = open_in path in
    let lexbuf = from_channel ic in
    lexbuf.lex_curr_p <- {
      pos_fname = path;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    try parse_lexbuf ?dialect lexbuf with
    | Parsing.Parse_error -> close_in ic; report_error lexbuf "Syntax error."
    | xc -> close_in ic; raise xc
}
