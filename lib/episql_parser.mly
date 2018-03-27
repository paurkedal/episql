/* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Unprime_list
%}

/* Operators and special */
%token COMMA DOT EOF SEMICOLON LPAREN RPAREN
%token<string> L6 L8 R0 A2 A4 A6

/* Keywords */
%token<string> AS AT BY CACHE CASCADE COLLATE CHECK CREATE CYCLE
%token<string> DEFAULT DELETE DROP ENUM EXISTS FOREIGN
%token<string> IF INCREMENT INHERIT KEY
%token<string> MINVALUE MAXVALUE NO NOT NULL ON WITH
%token<string> PRIMARY REFERENCES RESTRICT UNIQUE UPDATE SCHEMA SEQUENCE START
%token<string> TABLE TEMPORARY UNLOGGED TYPE ZONE
%token<string> YEAR MONTH DAY HOUR MINUTE SECOND TO

/* Type-Forming Words */
%token<string> BOOLEAN
%token<string> CHAR VARCHAR TEXT BYTEA
%token<string> SMALLINT INTEGER BIGINT SMALLSERIAL SERIAL BIGSERIAL
%token<string> REAL FLOAT DOUBLE PRECISION
%token<string> NUMERIC DECIMAL
%token<string> TIME DATE TIMESTAMP TIMEZONE INTERVAL

/* Literals */
%token<string> IDENTIFIER
%token<int64> INT
%token<float> FLT
%token<string> STRING

/* Precedence */
%left L6
%left L8
%nonassoc R0
%left A2
%left A4
%left A6
%nonassoc AT_TIME_ZONE
%nonassoc APPLICATION

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
  | CREATE sequence_scope SEQUENCE tfqname seq_attrs
    { Create_sequence {sequence_qname = $4; sequence_scope = $2;
                       sequence_attrs = List.rev $5} }
  | CREATE table_scope TABLE IF NOT EXISTS tfqname LPAREN table_items RPAREN
    { Create_table {table_qname = $7; table_scope = $2;
                    table_if_not_exists = true; table_items = List.rev $9} }
  | CREATE table_scope TABLE tfqname LPAREN table_items RPAREN
    { Create_table {table_qname = $4; table_scope = $2;
                    table_if_not_exists = false; table_items = List.rev $6} }
  | CREATE TYPE tfqname AS ENUM LPAREN enum_cases RPAREN
    { Create_enum ($3, $7) }
  | DROP SCHEMA nonempty_tfnames_r drop_options_r
    { Drop_schema (List.rev $3, List.rev $4) }
  | DROP SCHEMA IF EXISTS nonempty_tfnames_r drop_options_r
    { Drop_schema (List.rev $5, `If_exists :: List.rev $6) }
  | DROP TABLE nonempty_tfqnames_r drop_options_r
    { Drop_table (List.rev $3, List.rev $4) }
  | DROP TABLE IF EXISTS nonempty_tfqnames_r drop_options_r
    { Drop_table (List.rev $5, `If_exists :: List.rev $6) }
  | DROP SEQUENCE nonempty_tfqnames_r drop_options_r
    { Drop_sequence (List.rev $3, List.rev $4) }
  | DROP SEQUENCE IF EXISTS nonempty_tfqnames_r drop_options_r
    { Drop_sequence (List.rev $5, `If_exists :: List.rev $6) }
  | DROP TYPE nonempty_tfqnames_r drop_options_r
    { Drop_type (List.rev $3, List.rev $4) }
  | DROP TYPE IF EXISTS nonempty_tfqnames_r drop_options_r
    { Drop_type (List.rev $5, `If_exists :: List.rev $6) }
  ;

drop_options_r:
    /* empty */ { [] }
  | drop_options_r CASCADE { `Cascade :: $1 }
  | drop_options_r RESTRICT { `Restrict :: $1 }
  ;

seq_attrs: /* empty */ { [] } | seq_attrs seq_attr { $2 :: $1 };
seq_attr:
    INCREMENT by_opt INT { `Increment $3 }
  | MINVALUE INT { `Minvalue $2 }
  | MAXVALUE INT { `Maxvalue $2 }
  | START with_opt INT { `Start $3 }
  | CACHE INT { `Cache $2 }
  | CYCLE { `Cycle }
  | NO CYCLE { `No_cycle }
  ;

sequence_scope: /* empty */ { `Permanent } | TEMPORARY { `Temporary };
table_scope:
    /* empty */ { `Permanent }
  | UNLOGGED { `Permanent_unlogged }
  | TEMPORARY { `Temporary };
by_opt: /* empty */ {()} | BY {()};
with_opt: /* empty */ {()} | WITH {()};

table_items: /* empty */ { [] } | nonempty_table_items { $1 };
nonempty_table_items:
    table_item { [$1] }
  | table_items COMMA table_item { $3 :: $1 }
  ;
table_item:
    tfname datatype collate column_constraints
    { Column { column_name = $1; column_type = $2;
               column_collate = $3; column_constraints = List.rev $4 } }
  | table_constraint { Constraint $1 }
  ;
collate:
    /* empty */ { None }
  | COLLATE identifier { Some $2; }
  ;
column_constraints:
    /* empty */ { [] }
  | column_constraints column_constraint { $2 :: $1 }
  ;
check_constraint:
    LPAREN expr RPAREN { {condition = $2; no_inherit = false} }
  | LPAREN expr RPAREN NO INHERIT { {condition = $2; no_inherit = true} }
  ;
column_constraint:
    NOT NULL { `Not_null }
  | NULL { `Null }
  | CHECK check_constraint { `Check $2 }
  | UNIQUE { `Unique }
  | PRIMARY KEY { `Primary_key }
  | DEFAULT expr { `Default $2 }
  | REFERENCES tfqname { `References ($2, None) }
  | REFERENCES tfqname LPAREN tfname RPAREN {`References ($2, Some $4)}
  | ON DELETE action { `On_delete $3 }
  | ON UPDATE action { `On_update $3 }
  ;
table_constraint:
    CHECK check_constraint { `Check $2 }
  | UNIQUE LPAREN nonempty_tfnames_r RPAREN
    { `Unique (List.rev $3) }
  | PRIMARY KEY LPAREN nonempty_tfnames_r RPAREN
    { `Primary_key (List.rev $4) }
  | FOREIGN KEY LPAREN nonempty_tfnames_r RPAREN
    REFERENCES tfqname paren_column_names_opt
    { `Foreign_key (List.rev $4, $7, $8) }
  ;
paren_column_names_opt:
    /* empty */ { [] }
  | LPAREN nonempty_tfnames_r RPAREN { List.rev $2 }
  ;

action:
    CASCADE { `Cascade }
  | RESTRICT { `Restrict }
  ;

nonempty_tfnames_r:
    tfname { [$1] }
  | nonempty_tfnames_r COMMA tfname { $3 :: $1 }
  ;

nonempty_tfqnames_r:
    tfqname { [$1] }
  | nonempty_tfqnames_r COMMA tfqname { $3 :: $1 }
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
  | FLOAT LPAREN INT RPAREN { if $3 <= 24L then `Real else `Double_precision }
  | DOUBLE PRECISION { `Double_precision }
  | VARCHAR LPAREN INT RPAREN { `Varchar (Int64.to_int $3) }
  | CHAR LPAREN INT RPAREN { `Char (Int64.to_int $3) }
  | TEXT { `Text }
  | BYTEA { `Bytea }
  | SMALLINT { `Smallint }
  | INTEGER { `Integer }
  | BIGINT { `Bigint }
  | SMALLSERIAL { `Smallserial }
  | SERIAL { `Serial }
  | BIGSERIAL { `Bigserial }
  | NUMERIC { `Numeric_auto }
  | DECIMAL { `Numeric_auto }
  | NUMERIC LPAREN INT RPAREN { `Numeric (Int64.to_int $3, 0) }
  | DECIMAL LPAREN INT RPAREN { `Numeric (Int64.to_int $3, 0) }
  | NUMERIC LPAREN INT COMMA INT RPAREN
    { `Numeric (Int64.to_int $3, Int64.to_int $5) }
  | DECIMAL LPAREN INT COMMA INT RPAREN
    { `Numeric (Int64.to_int $3, Int64.to_int $5) }
  | TIME { `Time (None, false) }
  | TIME LPAREN INT RPAREN { `Time (Some (Int64.to_int $3), false) }
  | TIME WITH TIMEZONE { `Time (None, true) }
  | TIME LPAREN INT RPAREN WITH TIMEZONE
    { `Time (Some (Int64.to_int $3), true) }
  | DATE { `Date }
  | TIMESTAMP { `Timestamp (None, false) }
  | TIMESTAMP WITH TIMEZONE { `Timestamp (None, true) }
  | TIMESTAMP LPAREN INT RPAREN { `Timestamp (Some (Int64.to_int $3), false) }
  | TIMESTAMP LPAREN INT RPAREN WITH TIMEZONE
    { `Timestamp (Some (Int64.to_int $3), true) }
  | INTERVAL interval_fields { `Interval ($2, None) }
  | INTERVAL interval_fields LPAREN INT RPAREN
    { `Interval ($2, Some (Int64.to_int $4)) }
  | qname { `Custom $1 }
  ;

interval_fields:
    /* empty */ { `Default }
  | YEAR { `Year }
  | MONTH { `Month }
  | DAY { `Day }
  | HOUR { `Hour }
  | MINUTE { `Minute }
  | SECOND { `Second }
  | YEAR TO MONTH { `Year_to_month }
  | DAY TO HOUR { `Day_to_hour }
  | DAY TO MINUTE { `Day_to_minute }
  | DAY TO SECOND { `Day_to_second }
  | HOUR TO MINUTE { `Hour_to_minute }
  | HOUR TO SECOND { `Hour_to_second }
  | MINUTE TO SECOND { `Minute_to_second }
  ;

expr:
    expr_top { $1 }
  | expr L6 expr { Expr_app ((None, $2), [$1; $3]) }
  | expr L8 expr { Expr_app ((None, $2), [$1; $3]) }
  | NOT expr %prec L8 { Expr_app ((None, "NOT"), [$2]) }
  | expr R0 expr { Expr_app ((None, $2), [$1; $3]) }
  | expr A2 expr { Expr_app ((None, $2), [$1; $3]) }
  | A2 expr { Expr_app ((None, $1), [$2]) }
  | expr A4 expr { Expr_app ((None, $2), [$1; $3]) }
  | expr A6 expr { Expr_app ((None, $2), [$1; $3]) }
  ;
expr_top:
    literal { Expr_literal $1 }
  | qname { Expr_qname $1 }
  | qname LPAREN expr_params RPAREN %prec APPLICATION { Expr_app ($1, $3) }
  | expr_top AT TIME ZONE STRING %prec AT_TIME_ZONE
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
  | FLT { Lit_float $1 }
  | STRING { Lit_text $1 }
  ;
identifier:
    IDENTIFIER { $1 }
  | IF { $1 }
  | CACHE { $1 }
  | CASCADE { $1 }
  | ENUM { $1 }
  | INCREMENT { $1 }
  | INHERIT { $1 }
  | KEY { $1 }
  | MINVALUE { $1 }
  | MAXVALUE { $1 }
  | RESTRICT { $1 }
  | SCHEMA { $1 }
  | SEQUENCE { $1 }
  | TEMPORARY { $1 }
  | TYPE { $1 }
  | UNLOGGED { $1 }
  | ZONE { $1 }
  | YEAR { $1 }
  | MONTH { $1 }
  | DAY { $1 }
  | HOUR { $1 }
  | MINUTE { $1 }
  | SECOND { $1 }
  | TO { $1 }
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
