(ns fooheads.raql.schema
  "Defines malli schemas for the raql language and provides functions to
  validate and explain errors."
  (:require
    [clojure.walk :refer [postwalk]]
    [fooheads.raql.pagination :as pagination]
    [fooheads.stdlib :refer [apply-if]]
    [malli.core :as m]
    [malli.error :as me]))


(defn- form?
  [x]
  (and (vector? x) (= 'form (first x))))


(defn- form
  "Create a raql form schema.

  Also, wraps each element in a body in [:schema ...] so that they're not
  inlined in the cat. Don't know why we need this, but it works:
  https://clojurians-log.clojureverse.org/malli/2021-11-16"
  [namn & body]
  (into
    [:tuple (if (symbol? namn) [:enum namn] namn)]
    (map (fn [schema] [:schema schema]) body)))


(def schemas
  (->>
    (assoc
      '{::ident    :symbol
        ::constant [:or :int :double :string :boolean :string]
        ::operand  [:or ::constant :keyword]

        ::binary-operator-sym
        [:enum = <> < <= >= >]

        ::binary-operator
        [form ::binary-operator-sym ::operand ::operand]

        ::boolean-binary-operator-sym
        [:enum and or]

        ::boolean-binary-operator
        [form ::boolean-binary-operator-sym [:ref ::restriction] [:ref ::restriction]]

        ::in-operator
        [form in ::operand [:or [:vector :any] [:ref ::canonical]]]

        ::restriction
        [:or
         [:ref ::binary-operator]
         [:ref ::boolean-binary-operator]
         [:ref ::in-operator]]

        ::expr
        [:and
         [:vector :any]
         [:cat ::ident [:* [:or ::operand [:ref ::expr]]]]]

        ::relation
        [form relation :keyword]

        ::join
        [form join [:ref ::canonical] [:ref ::canonical] [:ref ::restriction]]

        ::full-join
        [form full-join [:ref ::canonical] [:ref ::canonical] [:ref ::restriction]]

        ::left-join
        [form left-join [:ref ::canonical] [:ref ::canonical] [:ref ::restriction]]

        ::right-join
        [form right-join [:ref ::canonical] [:ref ::canonical] [:ref ::restriction]]

        ::project
        [form project [:ref ::canonical] [:vector :keyword]]

        ::project-away
        [form project-away [:ref ::canonical] [:vector :keyword]]

        ::rename
        [form rename [:ref ::canonical] [:vector [:tuple :keyword :keyword]]]

        ::restrict
        [form restrict [:ref ::canonical] ::restriction]

        ::extend
        [form extend [:ref ::canonical] [:vector [:tuple :keyword ::expr]]]

        ::union
        [form union [:ref ::canonical] [:ref ::canonical]]

        ::distinct
        [form distinct [:ref ::canonical]]

        ::limit
        [:or
         [form limit [:ref ::canonical] ::pagination-value]
         [form limit [:ref ::canonical] ::pagination-value ::pagination-value]]

        ::offset
        [form offset [:ref ::canonical] ::pagination-value]

        ::order-by
        [form order-by [:ref ::canonical]
         [:vector [:or :keyword [:tuple :keyword [:enum :asc :desc]]]]]

        ;; start rules
        ::canonical
        [:or
         [:ref ::relation]
         [:ref ::restrict]
         [:ref ::project]
         [:ref ::project-away]
         [:ref ::rename]
         [:ref ::distinct]
         [:ref ::join]
         [:ref ::full-join]
         [:ref ::left-join]
         [:ref ::right-join]
         [:ref ::union]
         [:ref ::extend]
         [:ref ::limit]
         [:ref ::offset]
         [:ref ::order-by]]}
      ::pagination-value
      [:fn {:error/message "should be a nonnegative integer or named parameter"}
       pagination/value?])

    ;; apply all [form ...] elements
    (postwalk (apply-if form? #(apply form (rest %))))))


(defn schema
  "Create a schema for the specified start rule"
  ([]
   (m/schema [:schema {:registry schemas} ::canonical]))
  ([rule-name]
   (m/schema [:schema {:registry schemas} rule-name])))


(defn valid?
  "Validates the form against the raql schema."
  [schema form]
  (m/validate schema form))


(defn explain-data
  "Returns raw error descriptions as data or nil if the form is valid."
  [schema form]
  (m/explain schema form))


(defn explain
  "Returns humanized error descriptions as data or nil if the form is valid."
  [schema form]
  (me/humanize (explain-data schema form)))
