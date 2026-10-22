(ns fooheads.raql.divide-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.core :as raql]
    [fooheads.raql.schema :as schema]))


(def heading
  [#:attr{:name :item/id :type :integer :relvar-name :item-tags}
   #:attr{:name :tag :type :string :relvar-name :item-tags}
   #:attr{:name :locale :type :string :relvar-name :item-tags}])


(def heading-relmap
  {:relvar [#:relvar{:name :item-tags}
            #:relvar{:name :required-tags}
            #:relvar{:name :bad-tags}
            #:relvar{:name :other}]
   :attr (into heading
               [#:attr{:name :tag :type :string :relvar-name :required-tags}
                #:attr{:name :locale :type :string :relvar-name :required-tags}
                #:attr{:name :tag :type :integer :relvar-name :bad-tags}
                #:attr{:name :other/id :type :integer :relvar-name :other}])})


(deftest division-compilation-test
  (doseq [expr '[[divide [relation :item-tags] [relation :required-tags]]
                 [project [divide [relation :item-tags] [relation :required-tags]]
                  [:item/id]]
                 [divide
                  [restrict [relation :item-tags] [>= :item/id 1]]
                  [distinct [relation :required-tags]]]
                 [limit
                  [offset
                   [order-by
                    [divide [relation :item-tags] [relation :required-tags]]
                    [:item/id]]
                   1]
                  2]
                 [divide
                  [divide [relation :item-tags]
                   [project [relation :required-tags] [:locale]]]
                  [project [relation :required-tags] [:tag]]]]]
    (testing (str expr)
      (let [ast (raql/compile heading-relmap expr)]
        (is (= [(first heading)] (:heading ast)))
        (is (= expr (raql/decompile ast)))
        (is (= ast (raql/compile heading-relmap (raql/decompile ast))))
        (is (schema/valid? (schema/schema) expr)))))
  (is (= '[divide [relation :item-tags] [relation :required-tags]]
         (raql/decompile
           (raql/compile heading-relmap
                         '[-> [relation :item-tags]
                           [divide [relation :required-tags]]])))))


(deftest division-degenerate-headings-test
  (let [ast (raql/compile heading-relmap
                          '[divide [relation :item-tags] [relation :item-tags]])]
    (is (= [] (:heading ast))))
  (let [expr '[divide [relation :item-tags]
               [project [relation :required-tags] []]]
        ast (raql/compile heading-relmap expr)]
    (is (= heading (:heading ast)))
    (is (= expr (raql/decompile ast)))
    (is (schema/valid? (schema/schema) expr))))


(deftest invalid-division-syntax-test
  (doseq [expr '[[divide]
                 [divide [relation :item-tags]]
                 [divide [relation :item-tags] [relation :required-tags]
                  [relation :required-tags]]
                 [divide [relation :item-tags] 1]
                 [divide [] [relation :required-tags]]
                 [divide [= 1 1] [relation :required-tags]]
                 [divide [relation :item-tags] ?required-tags]
                 [divide [relation :item-tags] ["A" "B"]]]]
    (testing (str expr)
      (is (not (schema/valid? (schema/schema) expr)))
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
            (raql/compile heading-relmap expr))))))


(deftest invalid-division-headings-test
  (doseq [expr '[[divide [relation :item-tags] [relation :other]]
                 [divide [relation :item-tags] [relation :bad-tags]]
                 [divide
                  [project [relation :item-tags] [:item/id :tag]]
                  [relation :required-tags]]]]
    (testing (str expr)
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
            (raql/compile heading-relmap expr))))))
