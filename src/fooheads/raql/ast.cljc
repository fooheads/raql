(ns fooheads.raql.ast
  (:require
    [clojure.walk :refer [postwalk]]
    [fooheads.raql.pagination :as pagination]
    [fooheads.stdlib :refer [const apply-if]]
    [fooheads.tolk :as tolk]))


(def operators
  #{'relation
    'restrict
    'project
    'project-away
    'rename
    'distinct
    'join
    'full-join
    'left-join
    'right-join
    'union
    'divide
    'limit
    'offset
    'order-by
    'extend})


(defn operator? [x]
  (boolean (operators x)))


(defn- resolve-var [v]
  (if (operator? v)
    (fn [& args]
      (when (= 'divide v)
        (when-not (= 2 (count args))
          (throw (ex-info "divide requires exactly two operands"
                          {:operator v :args (vec args)})))
        (when-not (every? #(operator? (:operator %)) args)
          (throw (ex-info "divide requires two relational queries"
                          {:operator v :args (vec args)}))))
      (when (#{'limit 'offset} v)
        (pagination/validate-args! v args)
        (when-not (operator? (:operator (first args)))
          (throw (ex-info "Pagination requires a relational query"
                          {:operator v :relation (first args)}))))
      {:operator v
       :args (vec args)})
    v))


(defn raql->ast [expr]
  (let [interpret (tolk/interpreter resolve-var)]
    (tolk/get! (interpret expr))))


(defn node? [x]
  (and (map? x) (contains? x :operator) (contains? x :args)))


(defn ast->raql [ast]
  (postwalk
    (apply-if
      node?
      (fn [node] (const (:operator node) (:args node))))
    ast))
