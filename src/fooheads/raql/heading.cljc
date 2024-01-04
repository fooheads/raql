(ns fooheads.raql.heading
  (:refer-clojure :exclude [alias distinct extend])
  (:require
    [clojure.walk :as walk]
    [fooheads.raql.ast :as ast]
    [fooheads.setish :as set]
    [fooheads.stdlib :as stdlib :refer [apply-if throw-ex]]))


(defn- -infer-type [heading inferrers expr]
  (let [infer-type (partial -infer-type heading inferrers)]
    (if (:operator expr)
      (let [operator (:operator expr)
            args (:args expr)
            types (mapv infer-type args)
            infer (get inferrers operator)]
        (if infer
          (let [inferred (apply infer types)]
            (if (map? inferred)
              (throw (ex-info (:error inferred) (merge {:expr expr} inferred)))
              inferred))
          (throw-ex "infer: Can't resolve symbol '{operator}'")))
      :string)))


(defn- validate-attrs-exists! [operation-name xh attr-names]
  (let [attr-names (vec attr-names)
        xh-attr-names (map :attr/name xh)
        missing-attrs (set/difference attr-names xh-attr-names)]
    (when-not (empty? missing-attrs)
      (throw-ex
        "Can't {operation-name} attrs not present in relation: {missing-attrs}"
        xh attr-names operation-name))))


(defn- relation [heading-relmap relvar-name]
  (if-not (empty? (set/restrict (:relvar heading-relmap) #(= (:relvar/name %) relvar-name)))
    (->> heading-relmap :attr (filterv #(= relvar-name (:attr/relvar-name %))))
    (throw-ex "No relation named {relvar-name}")))


(defn- join
  ([xh yh]
   (join xh yh nil))
  ([xh yh _]
   (set/union xh yh)))


(defn- rename
  [xh renames]
  (let [rename-map (into {} renames)]
    (validate-attrs-exists! "rename" xh (keys rename-map))
    (set/update xh :attr/name (fn [v] (get rename-map v v)))))


(defn- distinct
  [xh]
  xh)


(defn- project
  [xh header-names]
  (validate-attrs-exists! "project" xh header-names)
  (let [header-names (set header-names)]
    (set/select
      (fn [t]
        (header-names (:attr/name t)))
      xh)))


(defn- project-away
  [xh header-names]
  (validate-attrs-exists! "project-away" xh header-names)
  (let [header-names (set header-names)]
    (set/select
      (fn [t]
        (not (header-names (:attr/name t))))
      xh)))


(defn- union [xh _yh]
  xh)


(defn- order-by [xh ordering]
  (let [attrs (map (fn [x] (if (vector? x) (first x) x)) ordering)]
    (validate-attrs-exists! "order-by" xh attrs)
    xh))


(defn- limit [xh _n _offset]
  xh)


(defn- -infer-heading
  [heading-relmap heading inferrers expr]
  (let [infer-heading (partial -infer-heading heading-relmap heading inferrers)
        infer-type (partial -infer-type heading inferrers)
        operator (:operator expr)
        args (:args expr)]
    (case operator
      relation
      (relation heading-relmap (first args))

      restrict
      (let [[expr restriction] args
            heading (infer-heading expr)
            _ (-infer-heading heading-relmap heading inferrers restriction)]
        heading)

      project
      (let [[expr header-names] args]
        (project (infer-heading expr) header-names))

      project-away
      (let [[expr header-names] args]
        (project-away (infer-heading expr) header-names))

      rename
      (let [[expr renames] args
            heading (infer-heading expr)]
        (rename heading renames))

      distinct
      (let [[expr] args
            heading (infer-heading expr)]
        (distinct heading))

      join
      (apply join (map infer-heading (take 2 args)))

      full-join
      (apply join (map infer-heading (take 2 args)))

      left-join
      (apply join (map infer-heading (take 2 args)))

      right-join
      (apply join (map infer-heading (take 2 args)))

      union
      (let [[expr-x expr-y] args
            heading-x (infer-heading expr-x)
            heading-y (infer-heading expr-y)]
        (assert (= heading-x heading-y) "union: headings must be the same")
        heading-x)

      limit
      (let [[expr n offset] args
            heading (infer-heading expr)]
        (limit heading n offset))

      order-by
      (let [[expr attrs] args
            heading (infer-heading expr)]
        (order-by heading attrs))

      (infer-type expr))))


(defn infer
  "Infers the heading for an expression"
  ([heading-relmap expr]
   (infer heading-relmap {} expr))
  ([heading-relmap inferrers expr]
   (-infer-heading heading-relmap #{} inferrers expr)))


(defn decorate
  "Decorates a raql expression tree with the heading at each level
  as metadata"
  [heading-relmap inferrers expr]
  (walk/postwalk
    (apply-if ast/node? (fn [m] (assoc m :heading (infer heading-relmap inferrers m))))
    expr))

