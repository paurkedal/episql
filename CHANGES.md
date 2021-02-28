## v0.12.0 - 2021-02-28

- Fix precedence for relations.
- Make `NULL` a literal in the grammar.
- Add suffix `IS...`, infix `IS NOT? DISTINCT FROM`, and `<>` operators.
- Add `-public-state` option to caqti-persist generator.
- Add option to use the result type where relevant.
- Add connection arg to `fetch` function when requested.
- Remove deprecated -new-order-by option.

## v0.11.2 - 2019-11-20

- Fixed matching of columns in SQL and extraction in generated search
  function when primary key columns do not precede other columns.
- The Macaque generator is now removed.
- Adjustments to packaging, in particular make dune a non-build dependency.

## v0.11.1 - 2018-10-02

- Fix clearing of select cache for caqti-persist create operation.
- Upgrade to opam 2, dune, and other build adjustments.

## v0.11.0 - 2018-03-27

- Switch to Caqti v2 API (breaking change).
- Fix quoting issue with nextval argument.
- Accept column `CHECK` constraints.
- Accept named `CONSTRAINT` clauses.
- Accept common expressions and floating point literals.

## v0.10.1 - 2018-02-03

- Support jbuilder >= 1.0+beta17.
- Support Caqti 0.9.0.

## v0.10.0 - 2017-12-04

- Adjust for new Caqti release.

## v0.9.2 - 2017-11-05

- Fix warnings from generated (and non-generated) code.

## v0.9.1 - 2017-11-03

- Fix findlib name caqti.lwt to caqti-lwt.
- Adjust for caqti 0.7.0 and some tweaks.

## v0.9.0 - 2017-06-05

- Move caqti-persist into a separate package to avoid depopts.
- Support placement of primary key in submodule for caqti-persist.
