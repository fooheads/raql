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
  in the `view-map`.

  The view map can look like this:
  `{:album-with-long-tracks some-raql-expression}`"
  [view-map raql]
  (postwalk
    (apply-if
      relation-expression?
      (fn [expr]
        (let [relvar-name (second expr)
              view (get view-map relvar-name expr)]
          (if (= view expr)
            expr
            (apply-views view-map view)))))
    raql))

