(ns fooheads.raql.heading
  (:refer-clojure :exclude [alias extend])
  (:require
    [fooheads.setish :as set]
    [fooheads.stdlib :as stdlib :refer [map-vals throw-ex]]))


(defn- into* [to from]
  (let [from (if (list? to) (reverse from) from)]
    (into to from)))


(defn- -inline-references
  [m v breadcrumbs]
  (when (breadcrumbs v)
    (throw (ex-info (str "Circular reference: " v " already referred.")
                    {:breadcrumbs breadcrumbs
                     :v v
                     :m m})))

  (cond
    (map? v)
    (reduce-kv
      (fn [m' k v]
        (assoc m' (-inline-references m k breadcrumbs)
               (-inline-references m v breadcrumbs)))
      {}
      v)

    (coll? v)
    (into* (empty v)
           (mapv #(-inline-references m % breadcrumbs) v))

    :else
    (if-let [v' (get m v)]
      (-inline-references m v' (conj breadcrumbs v))
      v)))


(defn- inline-references
  "Takes a map m that contains a key and some sort of expression
  as values and recursively inlines anything in the expressions
  that references the keys."
  [m]
  (zipmap
    (keys m)
    (-inline-references m (vals m) #{})))


(defn- -infer-type [heading inferrers expr]
  (let [infer-type (partial -infer-type heading inferrers)]
    (cond
      (string? expr)
      :string

      (keyword? expr)
      (if-let [typ (->> heading
                        (filter #(= expr (:attr/name %)))
                        (first)
                        (:attr/type))]
        typ
        (throw-ex "Unable to infer type for {expr}" heading))

      (vector? expr)
      (let [[operator & args] expr
            types (mapv infer-type args)
            infer (get inferrers operator)]
        (if infer
          (let [inferred (apply infer types)]
            (if (map? inferred)
              (throw (ex-info (:error inferred) (merge {:expr expr} inferred)))
              inferred))
          (throw-ex "Can't resolve symbol '{operator}'")))

      (double? expr)  :double
      (integer? expr) :integer
      (string? expr)  :string
      (boolean? expr) :boolean

      :else
      (let [msg (str "Not an expression: " expr)]
        (throw (ex-info msg {:heading heading :inferrers inferrers :expr expr}))))))


(defn join
  ([xh yh]
   (join xh yh nil))
  ([xh yh _]
   (set/union xh yh)))


(defn full-join
  ([xh yh]
   (full-join xh yh nil))
  ([xh yh _]
   (set/union xh yh)))


(defn rename
  [xh renames]
  (let [rename-map (into {} renames)]
    (set/update xh :attr/name (fn [v] (get rename-map v v)))))


(defn alias
  [xh aliases]
  (let [index (set/index-unique xh [:attr/name])
        aliash (mapv
                 (fn [[from to]]
                   (let [attr (index {:attr/name from})]
                     (assoc attr :attr/name to)))
                 aliases)]
    (set/union xh aliash)))


(defn project
  [xh header-names]
  (let [header-names (set header-names)]
    (set/select
      (fn [t]
        (header-names (:attr/name t)))
      xh)))


(defn project-away
  [xh header-names]
  (let [header-names (set header-names)]
    (set/select
      (fn [t]
        (not (header-names (:attr/name t))))
      xh)))


(defn extend [xh extension-type-map]
  (set/union
    xh
    (->>
      extension-type-map
      (map (fn [[k v]] {:attr/name k :attr/type v}))
      (set))))


(defn union [xh _yh]
  xh)


(defn aggregate-by [inferrers xh attrs aggregation-map]
  (let [projected-attrs (set attrs)]
    (set/union
      (set/select (fn [t] (projected-attrs (:attr/name t))) xh)
      (->>
        aggregation-map
        (map (fn [[k v]] {:attr/name k :attr/type (-infer-type xh inferrers v)}))
        (set)))))


(defn- -infer-heading
  [heading-relmap heading inferrers expr]
  (let [infer-heading (partial -infer-heading heading-relmap heading inferrers)
        infer-type (partial -infer-type heading inferrers)
        [operator & args] expr]
    (case operator
      relation
      (get heading-relmap (first args))

      restrict
      (let [[expr restriction] args
            heading (infer-heading expr)
            _ (-infer-heading heading-relmap heading inferrers restriction)]
        heading)

      join
      (apply join (map infer-heading (take 2 args)))

      full-join
      (apply full-join (map infer-heading (take 2 args)))

      project
      (let [[expr header-names] args]
        (project (infer-heading expr) header-names))

      project-away
      (let [[expr header-names] args]
        (project-away (infer-heading expr) header-names))

      extend
      (let [[expr extensions] args
            heading (infer-heading expr)
            infer-heading (partial -infer-heading heading-relmap
                                   heading inferrers)
            extensions (inline-references extensions)
            extension-type-map (map-vals infer-heading extensions)]
        (extend heading extension-type-map))

      rename
      (let [[expr renames] args
            heading (infer-heading expr)]
        (rename heading renames))

      alias
      (let [[expr aliases] args
            heading (infer-heading expr)]
        (alias heading aliases))

      union
      (let [[expr-x expr-y] args
            heading-x (infer-heading expr-x)
            heading-y (infer-heading expr-y)]
        (assert (= heading-x heading-y) "union: headings must be the same")
        heading-x)

      aggregate-by
      (let [[expr attrs aggregation-map] args
            heading (infer-heading expr)]
        (aggregate-by inferrers heading attrs aggregation-map))

      order-by
      (let [[expr _attrs] args]
        (infer-heading expr))

      (infer-type expr))))


(defn infer
  "Infers the heading for an expression"
  ([heading-relmap expr]
   (infer heading-relmap {} expr))
  ([heading-relmap inferrers expr]
   (-infer-heading heading-relmap #{} inferrers expr)))

