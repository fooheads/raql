(ns fooheads.raql.syntactic-sugar
  (:require
    [clojure.walk :refer [postwalk]]
    [fooheads.stdlib :refer [apply-if]]))


(defn expand-threads
  "Expands all `->` in the form."
  [form]
  (let [macros {'-> (fn [forms]
                      (reduce
                        (fn [acc [op & args]]
                          (into [op acc] args))
                        forms))}

        macro? (fn [x] (and (sequential? x) ((set (keys macros)) (first x))))

        expand-macro
        (fn [[macro & forms]]
          (let [expand-macro (get macros macro)]
            (expand-macro forms)))]

    (postwalk (apply-if macro? expand-macro) form)))

