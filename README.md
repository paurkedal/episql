# epiSQL - SQL schema parser and code generator

## Synopsis

epiSQL /ˈɛpɪˈsiːkwəl/ is an SQL schema parser and code generator.  It
supports a subset of SQL `CREATE`-statements with focus on PostgreSQL.  It
contains generators for

  * [Caqti][], providing a persistence-style API with caching,
  * [Macaque][], providing `<:table< >>` and `<:sequence< >>` definitions,
  * XML, providing easy access for external applications to the schema, and
  * Unix shells, providing a series of function calls for general ad-hoc use.

## Caqti Persistence

**Note**: This generator will likely be replaced with one providing an
incompatible API, so don't use it for new projects.

This backend maps database rows to memory-cached objects, and provides an
interface to fetch, insert, update, and delete individual objects.  It is
assumed that only one application access the cached tables.

Cache-coherent search functions limited to single-table queries are
provided, as well.

Definitions are only generated for tables which explicitly specifies a
primary key.  The primary key is used to address objects.

It is recommended to generate four separate files, e.g.

``` sh
episql -g caqti-persist-types-mli -o foo_types.mli foo.sql
episql -g caqti-persist-types-ml  -o foo_types.ml  foo.sql
episql -g caqti-persist-mli -t Foo_types -o foo_functor.mli foo.sql
episql -g caqti-persist-ml  -t Foo_types -o foo_functor.ml  foo.sql
```

The `Foo_types` module will contain pure types and related functions, which
supports deriving annotations (see `-deriving`).  The `Foo_functor` module
contains a functor which can be instantiated with caching and database
traits.  For more details see

``` sh
episql -g caqti-persist-ml -help
```

## Macaque Output

**Note**: This generator will likely be dropped in the near future.

Use the command line switch `-g macaque`.  An table definition like

``` sql
CREATE TABLE testarea.note (note_id SERIAL PRIMARY KEY, note text NOT NULL);
```

translates to

``` ocaml
let note_note_id_seq =
  <:sequence< serial "testarea.note_note_id_seq" >>
let note =
  <:table< testarea.note (
        note_id integer NOT NULL,
        note text NOT NULL ) >>
```

Only a limited selection of default expression forms are supported.  If the
generator does not know how to handle a certain expression, it the default
specification is omitted from the Macaque description.

## XML Output

The switch `-g xml` produces an XML dump of the database schema, making it
possible for external software to extract information from the schema
without parsing SQL.  E.g. this could be used to implement an alternative
code generator using an XSLT.

The above example translates to

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<episql>
  <create-table name="testarea.note">
    <column name="note_id" type="serial" primary-key="true"/>
    <column name="note" type="text" nullable="false"/>
  </create-table>
</episql>
```

## Shell Output

Use `-g shell` to create a sequence of command invocations corresponding to
the table definitions.  E.g. the above example produce

``` sh
enter_create_table testarea.note
add_column testarea.note note_id 'serial' "PRIMARY KEY"
add_column testarea.note note 'text' "NOT NULL"
leave_create_table testarea.note
```

This is not meant as a permanent solution, but provides a quick way to
inspect or generate unsupported code.  The XML generator described above
provides a more faithful representation and is more appropriate for external
applications.


[Caqti]: https://github.com/paurkedal/ocaml-caqti
[Macaque]: http://ocsigen.org/macaque/
