(ns fooheads.raql.rewrite
  "Functions for manipulating and rewriting raql expressions"
  (:require
    [clojure.walk :refer [postwalk]]
    [fooheads.stdlib :refer [apply-if]]))


(defn- relation-expression? [expr]
  (and (vector? expr) (= 'relation (first expr))))


(defn apply-views
  "Rewrites a raql expression by replacing relation expressions
  like `[relation :album-with-long-tracks]` with a a raql expression
  provided by the `relname->expr` arg.

  `relname->expr` can be a map or a function. If a relname shouldn't be
  rewritten, `nil` must be returned.

  E.g. `{:album-with-long-tracks some-raql-expression}`."
  [relname->expr raql]
  (postwalk
    (apply-if
      relation-expression?
      (fn [expr]
        (let [relvar-name (second expr)
              view (or (relname->expr relvar-name) expr)]
          (if (= view expr)
            expr
            (apply-views relname->expr view)))))
    raql))

