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
* `divide` – return the tuples associated with every tuple of another relation
* `aggregate` – fold a relation into one tuple of named counts, sums, or averages
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

## Relational division

`[divide R S]` expresses "has every required value", where both operands are
base or derived relation expressions. The divisor S's attribute names must be a
subset of R's, with identical `:attr/type` values. The result heading is R's
heading minus S's heading. Attribute identity is the complete keyword, not just
its unqualified name; use `rename` to align separately named attributes.

For example:

```text
ItemTags(item-id, tag): (1, A), (1, B), (2, A),
                       (3, A), (3, B), (3, C)
RequiredTags(tag):      (A), (B)
```

With headings `[:ItemTags/item-id :ItemTags/tag]` and `[:RequiredTags/tag]`,
these canonical and threaded forms return distinct item IDs 1 and 3:

```clojure
'[divide
  [relation :ItemTags]
  [rename [relation :RequiredTags] [[:RequiredTags/tag :ItemTags/tag]]]]

'[-> [relation :ItemTags]
     [divide
      [rename [relation :RequiredTags] [[:RequiredTags/tag :ItemTags/tag]]]]]
```

Extra tags in R are allowed; duplicates in either operand do not affect the
answer. Multi-attribute divisors require every complete tuple, not independent
matches for each attribute. Division composes with projection, restrictions,
ordering, limit, offset, and other division expressions.

**Empty-divisor caveat:** if S is empty, division returns the distinct
projection of R onto the result attributes. An empty R always gives an empty
result, including when S is empty. It never invents IDs absent from R. A search
where "no selected tags" must include items without tags needs to handle that
case outside this binary operator.

RAQL keeps database behavior out of the compiler. `raql-honeysql` uses ordinary
SQL equality for division matching: NULL does not match NULL in either result
keys or divisor attributes. A required tuple containing NULL therefore cannot
be satisfied; with an empty divisor, NULL candidate keys are still retained.
The SQL adapter documents its unit-column representation for an empty result
heading (when S covers all of R's attributes).

Parameters can be used inside operand expressions using the translator's
existing binding support. An `in` collection parameter is not automatically a
relation; there is no relation-valued parameter API. Supply S as an existing
base or derived relation instead.

## Relational aggregation

`aggregate` is an ungrouped, declarative fold over a base or derived relation.
Its second operand maps output attribute keywords to inspectable aggregate
expressions:

```clojure
'[aggregate [relation :person]
  {:summary/count [count]
   :summary/total-age [sum :person/age]
   :summary/average-age [avg :person/age]}]

'[-> [relation :person]
     [restrict [>= :person/age 18]]
     [aggregate {:summary/count [count]
                 :summary/average-age [avg :person/age]}]]
```

Use existing canonical relation expressions, not a bare `:person` operand.
Predicates restrict the input; they are not aggregate arguments. The result
is a relation containing **exactly one tuple**, with only the named aggregate
attributes, unless an aggregate raises an error.

| Expression | Logical result type | Empty input | Invalid selected value |
| --- | --- | --- | --- |
| `[count]` | `:integer` | 0 | Counts tuples, regardless of field values |
| `[sum attribute]` | Input's `:integer` or `:double` | Numeric zero | Error |
| `[avg attribute]` | `:double` | **Error** | Error |

Selectors must name an existing attribute with logical type `:integer` or
`:double`. Missing/nil and nonnumeric selected values are errors, not values
to skip. Nonfinite floating values/results are outside the supported numeric
domain. Count does not mean SQL's NULL-skipping `COUNT(column)`. Exact decimal
types and optional-result types are not provided in this first implementation.
Integer counts and sums are exact within the backend's representable range;
overflow must fail rather than wrap or silently become floating point.
Averages and double sums are approximate, with backend floating-point rounding
and range limits. Integer averages can be fractional, but converting large
integers to double can lose precision.

**Set semantics apply to complete source tuples before the fold.** People
`(id=1, age=20)`, `(id=2, age=20)`, `(id=3, age=50)` have count 3 and average
age 30, not 35. Duplicate copies of the same person tuple contribute once;
different tuples with the same age both contribute. Project first to count
distinct values:

```clojure
'[-> [relation :person]
     [project [:person/age]]
     [aggregate {:summary/distinct-ages [count]}]]
```

Aggregation composes with restrictions, projection, views, ordering, pagination,
and other aggregates. Operator order remains meaningful: aggregating a page
summarizes that page; paginating an aggregate pages its one-tuple result.
For a total count and a page, derive two expressions from the same distinct,
unpaged matching relation:

```clojure
(let [matching '[-> [relation :person]
                    [restrict [>= :person/age ?minimum]]
                    [project [:person/id]]
                    [distinct]]]
  {:total ['aggregate matching '{:summary/count [count]}]
   :page ['-> matching ['order-by [:person/id]] ['offset 20] ['limit 10]]})
```

An empty aggregate map yields one empty tuple, even for empty input.
Counting a zero-attribute input yields 0 or 1 according to whether that input
relation is empty or contains the empty tuple.

`fooheads.raql.aggregate/definitions` describes the standard aggregate arities,
types, and empty/error contracts as backend-independent data. No Clojure
closures, arbitrary database functions, or SQL types are part of these
expressions. Backends implement these contracts or explicitly report an
unsupported capability. There is no public aggregate registration API yet.
Grouping is deferred; the intended future form is
`[aggregate-by R [:person/team] {:summary/count [count]}]`, not currently
supported syntax. Ordinary global aggregation does not require a grouping
relation.

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
