(ns fooheads.raql.core
  (:refer-clojure :exclude [compile])
  (:require
    [fooheads.raql.ast :as ast]
    [fooheads.raql.heading :as heading]
    [fooheads.raql.syntactic-sugar :as sugar]))


(defn- expand [expr]
  (sugar/expand-threads expr))


(defn compile
  "Compiles a raql expression into an AST"
  ([heading-relmap expr]
   (compile heading-relmap {} expr))
  ([heading-relmap inferrers expr]
   (let [expr (expand expr)]
     #_(when-let [explanation (schema/explain (schema/schema) expr)]
         (throw (ex-info (str explanation) {:explanation explanation
                                            :expr expr})))

     (->>
       expr
       (ast/raql->ast)
       (heading/decorate heading-relmap inferrers)))))

