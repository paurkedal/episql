## v0.15.1 - 2023-12-13

  - Support IN (...) for caqti-persist select.

## v0.15.0 - 2023-09-19

  - Upgrade to Caqti 2.0.

## v0.14.0 - 2023-03-27

  - Eliminate dependency of `lwt_log` for generated code.
  - Add information to the `Not_present` exception.

## v0.13.0 - 2021-10-26

This is a backwards incompatible release dune to items marked (\*) and
(\*\*), while the latter can be fixed by adjusting options.

  - Change license exception to the LGPL-3.0 Linking Exception.
  - Wrap modules in episql library (\*).
  - Split `Caqti_persist` into wrapped modules and revise the API (\*).
  - Add function to rename schema to functor domain (\*).
  - Generate patch and event only when explicitly enabled (\*\*).
  - Remove an obsolete option `-new-order-by` (\*\*).
  - Change `use_db` to use result in persist functor argument (\*).
  - Fix race between acquiring a connection and inserting.
  - Reduce the amount of generated code.

## v0.12.2 - 2021-10-11

  - Fix support for connection argument names other than `c`.
  - Fix parsing and representation of `ON (DELETE|UPDATE)`.
  - Fix parsing of `timestamp with time zone`.

## v0.12.1 - 2021-03-15

  - Add cache-invalidation features to caqti-persist generator.
  - Fix type generalization issue under recent compilers.

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
  - Adjustments to packaging, in particular make dune a non-build
    dependency.

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
