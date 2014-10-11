/* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
 */

%{
  open Episql_types
%}

/* Operators and special */
%token COMMA DOT EOF SEMICOLON LPAREN RPAREN

/* Keywords */
%token<string> AS AT BY CACHE CHECK CREATE CYCLE DEFAULT ENUM FOREIGN
%token<string> INCREMENT INHERIT KEY
%token<string> MINVALUE MAXVALUE NO NOT NULL WITH
%token<string> PRIMARY REFERENCES UNIQUE SCHEMA SEQUENCE START
%token<string> TABLE TEMPORARY TYPE ZONE

/* Type-Forming Words */
%token<string> BOOLEAN
%token<string> CHAR VARCHAR TEXT BYTEA
%token<string> SMALLINT INTEGER BIGINT SMALLSERIAL SERIAL BIGSERIAL
%token<string> REAL FLOAT DOUBLE PRECISION
%token<string> NUMERIC DECIMAL
%token<string> TIME DATE TIMESTAMP TIMEZONE INTERVAL

/* Literals */
%token<string> IDENTIFIER
%token<int> INT
%token<string> STRING

%type<Episql_types.statement list> schema
%start schema
%%

schema: statements EOF { List.rev $1 }

statements:
    /* empty */ { [] }
  | statements statement SEMICOLON { $2 :: $1 }
  ;

statement:
    CREATE SCHEMA tfname { Create_schema $3 }
  | CREATE temporary SEQUENCE tfqname seq_attrs
    { Create_sequence ($4, $2, List.rev $5) }
  | CREATE TABLE tfqname LPAREN table_items RPAREN
    { Create_table ($3, List.rev $5) }
  | CREATE TYPE tfqname AS ENUM LPAREN enum_cases RPAREN
    { Create_enum ($3, $7) }
  ;

seq_attrs: /* empty */ { [] } | seq_attrs seq_attr { $2 :: $1 };
seq_attr:
    TEMPORARY { `Temporary }
  | INCREMENT by_opt INT { `Increment $3 }
  | MINVALUE INT { `Minvalue $2 }
  | MAXVALUE INT { `Maxvalue $2 }
  | START with_opt INT { `Start $3 }
  | CACHE INT { `Cache $2 }
  | CYCLE { `Cycle }
  | NO CYCLE { `No_cycle }
  ;

temporary: /* empty */ { false } | TEMPORARY { true };
by_opt: /* empty */ {()} | BY {()};
with_opt: /* empty */ {()} | WITH {()};

table_items: /* empty */ { [] } | nonempty_table_items { $1 };
nonempty_table_items:
    table_item { [$1] }
  | table_items COMMA table_item { $3 :: $1 }
  ;
table_item:
    tfname datatype column_constraints { Column ($1, $2, List.rev $3) }
  | table_constraint { Constraint $1 }
  ;
column_constraints:
    /* empty */ { [] }
  | column_constraints column_constraint { $2 :: $1 }
  ;
column_constraint:
    NOT NULL { `Not_null }
  | NULL { `Null }
  | UNIQUE { `Unique }
  | PRIMARY KEY { `Primary_key }
  | DEFAULT expr { `Default $2 }
  | REFERENCES tfqname { `References ($2, None) }
  | REFERENCES tfqname LPAREN tfname RPAREN {`References ($2, Some $4)}
  ;
table_constraint:
    CHECK LPAREN expr RPAREN check_attr { `Check ($3, $5) }
  | UNIQUE LPAREN nonempty_tfnames RPAREN
    { `Unique (List.rev $3) }
  | PRIMARY KEY LPAREN nonempty_tfnames RPAREN
    { `Primary_key (List.rev $4) }
  | FOREIGN KEY LPAREN nonempty_tfnames RPAREN
    REFERENCES tfqname paren_column_names_opt
    { `Foreign_key (List.rev $4, $7, $8) }
  ;
paren_column_names_opt:
    /* empty */ { [] }
  | LPAREN nonempty_tfnames RPAREN { List.rev $2 }
  ;
check_attr: /* empty */ { [] } | NO INHERIT { [`No_inherit] };

nonempty_tfnames:
    tfname { [$1] }
  | nonempty_tfnames COMMA tfname { $3 :: $1 }
  ;

enum_cases:
    STRING { [$1] }
  | enum_cases COMMA STRING { $3 :: $1 }
  ;

qname:
    identifier { (None, $1) }
  | identifier DOT identifier { (Some $1, $3) }
  ;
tfqname:
    tfname { (None, $1) }
  | tfname DOT tfname { (Some $1, $3) }
  ;
datatype:
    BOOLEAN { `Boolean }
  | REAL { `Real }
  | FLOAT { `Double_precision }
  | FLOAT LPAREN INT RPAREN { if $3 <= 24 then `Real else `Double_precision }
  | DOUBLE PRECISION { `Double_precision }
  | VARCHAR LPAREN INT RPAREN { `Varchar $3 }
  | CHAR LPAREN INT RPAREN { `Char $3 }
  | TEXT { `Text }
  | BYTEA { `Bytea }
  | SMALLINT { `Smallint }
  | INTEGER { `Integer }
  | BIGINT { `Bigint }
  | SMALLSERIAL { `Smallserial }
  | SERIAL { `Serial }
  | BIGSERIAL { `Bigserial }
  | NUMERIC LPAREN INT COMMA INT RPAREN { `Numeric ($3, $5) }
  | DECIMAL LPAREN INT COMMA INT RPAREN { `Decimal ($3, $5) }
  | TIME { `Time }
  | DATE { `Date }
  | TIMESTAMP { `Timestamp }
  | TIMESTAMP WITH TIMEZONE { `Timestamp_with_timezone }
  | INTERVAL { `Interval }
  | qname { `Custom $1 }
  ;

expr:
    literal { Expr_literal $1 }
  | qname { Expr_qname $1 }
  | qname LPAREN expr_params RPAREN { Expr_app ($1, $3) }
  | expr AT TIME ZONE STRING
    { Expr_app ((None, "__at_time_zone"), [$1; Expr_literal (Lit_text $5)]) }
  | LPAREN expr RPAREN { $2 }
  ;
expr_params: /* empty */ {[]} | expr_nonempty_params {List.rev $1};
expr_nonempty_params:
    expr { [$1] }
  | expr_nonempty_params COMMA expr { $3 :: $1 }
  ;

literal:
    INT { Lit_integer $1 }
  | STRING { Lit_text $1 }
  ;
identifier:
    IDENTIFIER { $1 }
  | CACHE { $1 }
  | ENUM { $1 }
  | INCREMENT { $1 }
  | INHERIT { $1 }
  | KEY { $1 }
  | MINVALUE { $1 }
  | MAXVALUE { $1 }
  | SCHEMA { $1 }
  | SEQUENCE { $1 }
  | TEMPORARY { $1 }
  | TYPE { $1 }
  | ZONE { $1 }
  ;

tfname:
    identifier { $1 }
  | BOOLEAN { $1 }
  | REAL { $1 }
  | FLOAT { $1 }
  | DOUBLE { $1 }
  | PRECISION { $1 }
  | CHAR { $1 }
  | TIME { $1 }
  | VARCHAR { $1 }
  ;
