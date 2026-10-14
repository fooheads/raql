# raql

`raql` is a small relational algebra query language implemented in Clojure.  A
query is represented as a vector in prefix notation where the first element is
an operator followed by its arguments.  The language can be compiled to an
abstract syntax tree (AST) and analysed or rewritten programmatically.

## Expression language

The following operators are available:

* `relation` – reference a base relation by keyword
* `restrict` – filter a relation using boolean expressions (`=`, `<>`, `<`, `<=`,
  `>=`, `>`, `and`, `or`, `in`)
* `project` – keep the listed attributes
* `project-away` – remove the listed attributes
* `rename` – rename attributes using `[old new]` pairs
* `distinct` – remove duplicate tuples
* `join`, `full-join`, `left-join`, `right-join` – relational joins
* `union` – combine two relations with identical headings
* `order-by` – order tuples by attributes
* `limit` – limit the number of tuples, optionally with an offset
* `offset` – skip the specified number of tuples

A query can be compiled with `fooheads.raql.core/compile` which returns an AST
enriched with heading information.  The AST can be transformed back with
`fooheads.raql.core/decompile`.

### Example

```clojure
(raql/compile
  heading-relmap
  inferrers
  '[project
     [restrict
      [join
       [relation :artist]
       [relation :album]
       [= :artist/id :album/artist-id]]
      [= :artist/name "Jimi Hendrix"]]
     [:artist/name]])
```

### Threading syntax

The function `fooheads.raql.syntactic-sugar/expand-threads` expands `->`
threading forms to normal prefix expressions.  This makes it possible to write
queries in a more natural flow.

```clojure
(sugar/expand-threads
  '[-> [relation :artist]
       [restrict [< :artist/id 1000]]
       [rename {:artist/id :id}]
       [project [:id]]
       [offset 100]
       [limit 10]])
;; => '[limit
;;      [offset
;;       [project
;;        [rename
;;         [restrict [relation :artist] [< :artist/id 1000]]
;;         {:artist/id :id}]
;;        [:id]]
;;       100]
;;      10]
```

### Rewriting with views

`fooheads.raql.rewrite/apply-views` replaces `[relation <name>]` expressions with
predefined query expressions.  This enables view like abstractions when building
larger queries.

## Pagination

Limit and offset are separate, heading-preserving operators:

```clojure
'[-> [relation :artist]
     [order-by [:artist/artist-id]]
     [offset 20]
     [limit 10]]
```

This skips 20 rows, then returns at most 10. The existing `[limit 10 20]`
threading step is an equivalent shorthand. Either operator can be used alone.
Ordering by a unique attribute, or a combination of attributes that is unique,
is needed for predictable pages.

Operator order matters: `[limit 10]` followed by `[offset 3]` leaves at most
seven rows from the original first ten, not ten rows starting at the fourth.
Pagination can also appear inside subqueries and other relational expressions.
An `order-by` after pagination sorts the selected subset instead of changing
which rows were originally selected.

Counts must be nonnegative integers or named parameter symbols such as
`?page-size` and `?start`. Zero limit produces an empty relation; zero offset
skips nothing. Invalid counts and operator arities are rejected during
compilation. Named counts are validated when their values are bound by the
translator.

```clojure
'[-> [relation :artist]
     [order-by [:artist/artist-id]]
     [offset ?start]
     [limit ?page-size]]
```

RAQL syntax and ASTs do not select a database. For SQL, see
[`raql-honeysql`](https://github.com/fooheads/raql-honeysql) for database
configuration, named parameter binding, and database-specific ordering rules.

## Development

This project uses [Malli](https://github.com/metosin/malli) for validation and
Clojure for implementation.  Run the test suite with:

```bash
clojure -X:test
```

## License

Distributed under the [Eclipse Public License 2.0](https://www.eclipse.org/
legal/epl-2.0/).
