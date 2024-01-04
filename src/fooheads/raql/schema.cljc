(ns fooheads.raql.schema
  "Defines malli schemas for the raql language and provides functions to
  validate and explain errors."
  (:require
    [clojure.walk :refer [postwalk]]
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

      ::restriction
      [:or
       [:ref ::binary-operator]
       [:ref ::boolean-binary-operator]]

      ::expr
      [:and
       [:vector :any]
       [:cat ::ident [:* [:or ::operand [:ref ::expr]]]]]

      ::relation
      [form relation :keyword]

      ::join
      [form join [:ref ::relation] [:ref ::relation] [:ref ::restriction]]

      ::full-join
      [form full-join [:ref ::relation] [:ref ::relation] [:ref ::restriction]]

      ::project
      [form project [:ref ::relation] [:vector :keyword]]

      ::project-away
      [form project-away [:ref ::relation] [:vector :keyword]]

      ::rename
      [form rename [:ref ::relation] [:vector [:tuple :keyword :keyword]]]

      ::restrict
      [form restrict [:ref ::relation] ::restriction]

      ::extend
      [form extend [:ref ::relation] [:vector [:tuple :keyword ::expr]]]

      ::union
      [form union [:ref ::relation] [:ref ::relation]]

      ::limit
      [form limit [:ref ::relation] :int]

      ::offset
      [form offset [:ref ::relation] :int]

      ::order-by
      [form order-by [:ref ::relation] [:vector :keyword]]

      ;; start rules
      ::canonical
      [:or
       [:ref ::relation]
       [:ref ::restrict]
       [:ref ::project]
       [:ref ::project-away]
       [:ref ::rename]
       [:ref ::join]
       [:ref ::full-join]
       [:ref ::union]
       [:ref ::extend]
       [:ref ::limit]
       [:ref ::offset]
       [:ref ::order-by]]}

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

