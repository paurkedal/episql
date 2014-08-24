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

{
  open Episql_parser
  open Episql_types
  open Printf

  let keywords = Hashtbl.create 31
  let () =
    List.iter (fun (kw, token) -> Hashtbl.add keywords kw token)
      [ "AS", AS;
	"BY", BY;
	"CACHE", CACHE;
	"CREATE", CREATE;
	"CYCLE", CYCLE;
	"DEFAULT", DEFAULT;
	"ENUM", ENUM;
	"INCREMENT", INCREMENT;
	"KEY", KEY;
	"MINVALUE", MINVALUE;
	"MAXVALUE", MAXVALUE;
	"NO", NO;
	"NOT", NOT;
	"NULL", NULL;
	"PRIMARY", PRIMARY;
	"REFERENCES", REFERENCES;
	"UNIQUE", UNIQUE;
	"SCHEMA", SCHEMA;
	"SEQUENCE", SEQUENCE;
	"START", START;
	"TABLE", TABLE;
	"TEMP", TEMPORARY;
	"TEMPORARY", TEMPORARY;
	"TYPE", TYPE;
	"WITH", WITH;

	(* Type-forming keywords *)
	"BOOLEAN", BOOLEAN;
	"REAL", REAL;
	"DOUBLE", DOUBLE;
	"PRECISION", PRECISION;
	"CHAR", CHAR;
	"VARCHAR", VARCHAR;
	"TEXT", TEXT;
	"BYTEA", BYTEA;
	"SMALLINT", SMALLINT;
	"INTEGER", INTEGER;
	"BIGINT", BIGINT;
	"SMALLSERIAL", SMALLSERIAL;
	"SERIAL", SERIAL;
	"BIGSERIAL", BIGSERIAL;
	"NUMERIC", NUMERIC;
	"DECIMAL", DECIMAL;
	"TIME", TIME;
	"DATE", DATE;
	"TIMESTAMP", TIMESTAMP;
	"TIMEZONE", TIMEZONE;
	"INTERVAL", INTERVAL; ]
}

let digit = ['0'-'9']
let wordfst = ['a'-'z' 'A'-'Z' '_']
let wordcnt = ['a'-'z' 'A'-'Z' '_' '0'-'9']

rule lex_main = parse
  | "--" [^ '\n']* | [' ' '\t']+ { lex_main lexbuf }
  | '\n' { Lexing.new_line lexbuf; lex_main lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '.' { DOT }
  | wordfst wordcnt* as word
    { try Hashtbl.find keywords (String.uppercase word)
      with Not_found -> IDENTIFIER word }
  | "'" { lex_string (Buffer.create 64) lexbuf }
  | digit+ as i { INT (int_of_string i) }
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

{
  open Lexing

  let parse_lexbuf lexbuf = schema lex_main lexbuf

  let report_error lexbuf msg =
    let lxm = lexeme_start_p lexbuf in
    Printf.ksprintf failwith "%s:%d,%d: %s" lxm.pos_fname
		    lxm.pos_lnum (lxm.pos_cnum - lxm.pos_bol) msg

  let parse_file path =
    let ic = open_in path in
    let lexbuf = from_channel ic in
    lexbuf.lex_curr_p <- {
      pos_fname = path;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    try parse_lexbuf lexbuf with
    | Parsing.Parse_error -> close_in ic; report_error lexbuf "Syntax error."
    | xc -> close_in ic; raise xc
}
