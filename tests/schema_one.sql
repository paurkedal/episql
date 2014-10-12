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
  v2 timestamp DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'utc'),
  v3 integer NOT NULL DEFAULT 0
);

CREATE TABLE episql_tests.tensor1 (
  i integer PRIMARY KEY,
  x double precision NOT NULL,
  note text
);

CREATE TABLE episql_tests.tensor2 (
  i integer NOT NULL,
  j integer NOT NULL,
  x double precision NOT NULL,
  note text,
  PRIMARY KEY (i, j)
);

CREATE TABLE episql_tests.tensor3c (
  i integer NOT NULL,
  j integer NOT NULL,
  k integer NOT NULL,
  re double precision NOT NULL,
  im double precision NOT NULL,
  note text,
  PRIMARY KEY (i, j, k)
);
