(ns fooheads.raql.pagination
  (:require
    [clojure.string :as str]))


(defn parameter?
  [value]
  (and (symbol? value)
       (str/starts-with? (str value) "?")
       (< 1 (count (str value)))))


(defn count?
  [value]
  (and (integer? value) (<= 0 value)))


(defn value?
  [value]
  (or (count? value) (parameter? value)))


(defn validate-args!
  [operator args]
  (let [arities (if (= 'limit operator) #{2 3} #{2})
        arguments (if (= 'limit operator) [:limit :offset] [:offset])]
    (when-not (arities (count args))
      (throw (ex-info (str operator ": invalid argument count")
                      {:operator operator :args args})))
    (doseq [[argument value] (map vector arguments (rest args))]
      (when-not (value? value)
        (throw (ex-info "Pagination requires a nonnegative integer or named parameter"
                        {:operator operator :argument argument :value value}))))))


(defn resolve-count
  [value params]
  (let [parameter (when (parameter? value) (keyword (subs (str value) 1)))
        resolved (if parameter
                   (if (contains? params parameter)
                     (get params parameter)
                     (throw (ex-info "Missing pagination parameter"
                                     {:parameter parameter})))
                   value)]
    (when-not (count? resolved)
      (throw (ex-info "Pagination requires a nonnegative integer"
                      (cond-> {:value resolved}
                        parameter (assoc :parameter parameter)))))
    resolved))
