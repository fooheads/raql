(ns fooheads.raql.aggregate-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.aggregate :as aggregate]
    [fooheads.raql.core :as raql]
    [fooheads.raql.rewrite :as rewrite]
    [fooheads.raql.schema :as schema]))


(def heading-relmap
  {:relvar [#:relvar{:name :person}]
   :attr [#:attr{:name :person/id :type :integer :relvar-name :person}
          #:attr{:name :person/age :type :integer :relvar-name :person}
          #:attr{:name :person/score :type :double :relvar-name :person}
          #:attr{:name :person/name :type :string :relvar-name :person}
          #:attr{:name :person/decimal :type :decimal :relvar-name :person}]})


(def aggregates
  '{:summary/count [count]
    :summary/total-age [sum :person/age]
    :summary/average-age [avg :person/age]
    :summary/total-score [sum :person/score]
    :summary/average-score [avg :person/score]})


(def result-heading
  [#:attr{:name :summary/count :type :integer}
   #:attr{:name :summary/total-age :type :integer}
   #:attr{:name :summary/average-age :type :double}
   #:attr{:name :summary/total-score :type :double}
   #:attr{:name :summary/average-score :type :double}])


(deftest aggregate-compilation-test
  (doseq [source '[[relation :person]
                   [restrict [relation :person] [>= :person/age 18]]
                   [project [relation :person] [:person/age :person/score]]
                   [limit [offset [order-by [relation :person] [:person/id]] 1] 2]]
          :let [expr ['aggregate source aggregates]
                ast (raql/compile heading-relmap expr)]]
    (testing (str expr)
      (is (= result-heading (:heading ast)))
      (is (= expr (raql/decompile ast)))
      (is (= ast (raql/compile heading-relmap (raql/decompile ast))))
      (is (schema/valid? (schema/schema) expr))))
  (is (= ['aggregate '[relation :person] aggregates]
         (raql/decompile
           (raql/compile heading-relmap
                         ['-> '[relation :person] ['aggregate aggregates]])))))


(deftest aggregate-composition-test
  (doseq [expr '[[project [aggregate [relation :person] {:n [count]}] [:n]]
                 [restrict [aggregate [relation :person] {:n [count]}] [> :n 0]]
                 [limit [offset [order-by
                                 [aggregate [relation :person] {:n [count]}] [:n]] 0] 1]
                 [aggregate [aggregate [relation :person] {:n [count]}]
                  {:n [sum :n]}]
                 [aggregate [project [relation :person] []] {:n [count]}]]]
    (let [ast (raql/compile heading-relmap expr)]
      (is (= [#:attr{:name :n :type :integer}] (:heading ast)))
      (is (= expr (raql/decompile ast)))
      (is (schema/valid? (schema/schema) expr))))
  (let [expr '[aggregate [relation :adults] {:n [count]}]
        views '{:adults [restrict [relation :person] [>= :person/age 18]]}
        expanded (rewrite/apply-views views expr)]
    (is (= '[aggregate [restrict [relation :person] [>= :person/age 18]] {:n [count]}]
           (raql/decompile (raql/compile heading-relmap expanded))))))


(deftest aggregate-map-and-empty-heading-test
  (doseq [aggregates ['{:operator [count] :args [sum :person/age]} {}]]
    (let [expr ['aggregate '[relation :person] aggregates]
          ast (raql/compile heading-relmap expr)]
      (is (= (set (keys aggregates)) (set (map :attr/name (:heading ast)))))
      (is (= expr (raql/decompile ast)))
      (is (= ast (raql/compile heading-relmap (raql/decompile ast))))
      (is (schema/valid? (schema/schema) expr)))))


(deftest aggregate-contract-test
  (is (= :zero (get-in aggregate/definitions ['count :empty])))
  (is (= :zero (get-in aggregate/definitions ['sum :empty])))
  (is (= :error (get-in aggregate/definitions ['avg :empty])))
  (doseq [op '[sum avg]]
    (is (= :error (get-in aggregate/definitions [op :invalid-value])))))


(deftest invalid-aggregate-syntax-test
  (doseq [expr '[[aggregate]
                 [aggregate [relation :person]]
                 [aggregate [relation :person] {} {}]
                 [aggregate :person {:n [count]}]
                 [aggregate [] {:n [count]}]
                 [aggregate [= 1 1] {:n [count]}]
                 [aggregate [relation :person] []]
                 [aggregate [relation :person] {"n" [count]}]
                 [aggregate [relation :person] {:n count}]
                 [aggregate [relation :person] {:n [unknown]}]
                 [aggregate [relation :person] {:n [sql/count]}]
                 [aggregate [relation :person] {:n [count :person/age]}]
                 [aggregate [relation :person] {:n [sum]}]
                 [aggregate [relation :person] {:n [avg]}]
                 [aggregate [relation :person] {:n [avg :person/age :person/id]}]
                 [aggregate [relation :person] {:n [sum 1]}]
                 [aggregate [relation :person] {:n [avg ?age]}]
                 [aggregate [relation :person] {:n [avg [+ :person/age 1]]}]]]
    (testing (str expr)
      (is (not (schema/valid? (schema/schema) expr)))
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
            (raql/compile heading-relmap expr)))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
        (raql/compile heading-relmap
                      ['aggregate '[relation :person] {:n (fn [_] 0)}]))))


(deftest invalid-aggregate-types-test
  (doseq [op '[sum avg]
          attr [:person/missing :person/name :person/decimal]
          :let [expr ['aggregate '[relation :person] {:n [op attr]}]]]
    (testing (str expr)
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
            (raql/compile heading-relmap expr)))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
        (raql/compile heading-relmap
                      '[aggregate [project [relation :person] [:person/id]]
                        {:mean [avg :person/age]}]))))
