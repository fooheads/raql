(ns fooheads.raql.ast
  (:require
    [clojure.walk :refer [postwalk]]
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
    'limit
    'order-by
    'extend})


(defn operator? [x]
  (boolean (operators x)))


(defn- resolve-var [v]
  (if (operator? v)
    (fn [& args]
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

