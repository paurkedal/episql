DROP SCHEMA episql_tests CASCADE;
CREATE SCHEMA episql_tests;

CREATE TABLE episql_tests.cp_1d_0 (
  k0 SERIAL PRIMARY KEY
);
CREATE TABLE episql_tests.cp_1d_1o (
  k0 SERIAL PRIMARY KEY,
  v0 text
);
CREATE TABLE episql_tests.cp_1d_1r (
  k0 SERIAL PRIMARY KEY,
  v0 real NOT NULL
);
CREATE TABLE episql_tests.cp_1d_1o1r1d (
  k0 SERIAL PRIMARY KEY,
  v0 integer,
  v1 text NOT NULL,
  v2 timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
  v3 boolean NOT NULL DEFAULT true
);

CREATE TABLE episql_tests.tensor1 (
  i integer CONSTRAINT tensor1_pk PRIMARY KEY,
  x double precision NOT NULL,
  note text
);

CREATE TABLE episql_tests.tensor2 (
  i integer NOT NULL CONSTRAINT tensor2_check_i_ge_zero CHECK (i >= 0),
  x double precision NOT NULL,
  j integer NOT NULL,
  note text,
  CONSTRAINT tensor2_pk PRIMARY KEY (i, j),
  CHECK (note IS NULL OR note IS NOT NULL OR i = i IS NOT UNKNOWN)
);

CREATE TABLE episql_tests.tensor3c (
  i integer NOT NULL,
  j integer NOT NULL,
  k integer NOT NULL,
  re double precision NOT NULL,
  im double precision NOT NULL,
  "note'" text COLLATE "C",
  PRIMARY KEY (i, j, k)
);

CREATE SEQUENCE episql_tests.defaulted_seq;

CREATE TABLE episql_tests.defaulted (
  i16 smallint NOT NULL DEFAULT 0,
  i32 integer NOT NULL DEFAULT 0,
  i64 bigint NOT NULL DEFAULT 0,
  s16 smallint NOT NULL DEFAULT nextval('episql_tests.defaulted_seq'),
  s32 integer NOT NULL DEFAULT nextval('episql_tests.defaulted_seq'),
  s64 bigint NOT NULL DEFAULT nextval('episql_tests.defaulted_seq')
);

CREATE TABLE episql_tests.ents (
  ent_id integer PRIMARY KEY,
  ent_name text UNIQUE NOT NULL
);
CREATE TABLE episql_tests.mysts (
  myst_id integer NOT NULL,
  ent1_id integer NOT NULL,
  ent2_id integer NOT NULL,
  PRIMARY KEY (myst_id, ent1_id, ent2_id),
  FOREIGN KEY (ent1_id) REFERENCES episql_tests.ents (ent_id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  FOREIGN KEY (ent2_id) REFERENCES episql_tests.ents
    ON DELETE CASCADE ON UPDATE RESTRICT
);
