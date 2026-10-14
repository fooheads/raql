(ns fooheads.raql.pagination-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.core :as raql]
    [fooheads.raql.pagination :as pagination]
    [fooheads.raql.schema :as schema]))


(def heading
  [#:attr{:name :item/id :type :integer :relvar-name :item}])


(def heading-relmap
  {:relvar [#:relvar{:name :item}]
   :attr heading})


(deftest pagination-compilation-test
  (doseq [expr '[[limit [relation :item] 0]
                 [offset [relation :item] 0]
                 [limit [relation :item] 10 20]
                 [limit [relation :item] ?page-size ?start]
                 [offset [relation :item] ?paging/start]
                 [limit [offset [order-by [relation :item] [:item/id]] 20] 10]
                 [project [limit [relation :item] 10] [:item/id]]
                 [offset [distinct [relation :item]] 3]]]
    (testing (str expr)
      (let [ast (raql/compile heading-relmap expr)]
        (is (= heading (:heading ast)))
        (is (= expr (raql/decompile ast)))
        (is (schema/valid? (schema/schema) expr))))))


(deftest pagination-threading-test
  (is (= '[limit [offset [order-by [relation :item] [:item/id]] ?start] ?page-size]
         (raql/decompile
           (raql/compile heading-relmap
                         '[-> [relation :item]
                           [order-by [:item/id]]
                           [offset ?start]
                           [limit ?page-size]])))))


(deftest invalid-pagination-test
  (doseq [expr '[[limit [relation :item]]
                 [limit [relation :item] 1 2 3]
                 [offset [relation :item]]
                 [offset [relation :item] 1 2]
                 [limit [relation :item] -1]
                 [offset [relation :item] -1]
                 [limit [relation :item] 1 -1]
                 [limit [relation :item] 1.5]
                 [offset [relation :item] nil]
                 [limit [relation :item] "10"]
                 [limit [relation :item] true]
                 [limit [relation :item] ?]
                 [limit 1 10]
                 [offset [] 10]
                 [limit [= :item/id 1] 10]]]
    (testing (str expr)
      (is (not (schema/valid? (schema/schema) expr)))
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
            (raql/compile heading-relmap expr))))))


(deftest pagination-binding-test
  (is (= 0 (pagination/resolve-count '?count {:count 0})))
  (is (= 10 (pagination/resolve-count '?paging/count {:paging/count 10})))
  (doseq [params [{} {:count nil} {:count -1} {:count 1.5} {:count "10"}]]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
          (pagination/resolve-count '?count params)))))
