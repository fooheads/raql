(ns fooheads.raql.aggregate)


(def definitions
  "Backend-independent contracts for the standard aggregates.
  Aggregate expressions are data; public extension registration is not provided."
  {'count {:arity 0 :result-type :integer :empty :zero :overflow :error}
   'sum {:arity 1 :input-types #{:integer :double}
         :result-type :input :empty :zero :invalid-value :error
         :non-finite :error :overflow :error}
   'avg {:arity 1 :input-types #{:integer :double}
         :result-type :double :empty :error :invalid-value :error
         :non-finite :error :overflow :error}})


(defn expression?
  [expr]
  (and (vector? expr)
       (let [{:keys [arity]} (get definitions (first expr))]
         (and (some? arity)
              (= arity (dec (count expr)))
              (or (zero? arity) (keyword? (second expr)))))))


(defn validate-args!
  [args]
  (when-not (= 2 (count args))
    (throw (ex-info "aggregate requires a relation and a named-aggregate map"
                    {:operator 'aggregate :args (vec args)})))
  (let [[_ aggregates] args]
    (when-not (map? aggregates)
      (throw (ex-info "aggregate requires a named-aggregate map"
                      {:operator 'aggregate :aggregates aggregates})))
    (doseq [[attribute expr] aggregates]
      (when-not (keyword? attribute)
        (throw (ex-info "Aggregate attribute names must be keywords"
                        {:operator 'aggregate :attribute attribute})))
      (when-not (expression? expr)
        (throw (ex-info "Expected [count], [sum attribute], or [avg attribute]"
                        {:operator 'aggregate :attribute attribute :expression expr}))))))


(defn infer-heading
  [heading aggregates]
  (validate-args! [nil aggregates])
  (let [attributes (into {} (map (juxt :attr/name identity)) heading)]
    (mapv
      (fn [[attribute [operator selector :as expr]]]
        (let [{:keys [arity input-types result-type]} (definitions operator)
              input (when (pos? arity) (get attributes selector))]
          (when (and (pos? arity) (not input))
            (throw (ex-info "Aggregate selector is not present in the input heading"
                            {:operator operator :attribute selector :expression expr})))
          (when (and input (not (contains? input-types (:attr/type input))))
            (throw (ex-info "Aggregate selector must have logical type :integer or :double"
                            {:operator operator :attribute selector
                             :type (:attr/type input) :expression expr})))
          #:attr{:name attribute
                 :type (if (= :input result-type) (:attr/type input) result-type)}))
      aggregates)))
