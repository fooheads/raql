(ns fooheads.raql.schema-test
  (:require
    [clojure.test :refer [are deftest is testing]]
    [fooheads.raql.schema :as schema]
    [malli.core :as m]))


(defn valid? [rule-name value]
  (schema/valid? (schema/schema rule-name) value))


(defn explain [rule-name value]
  (schema/explain (schema/schema rule-name) value))


(defn explain-data [rule-name value]
  (schema/explain-data (schema/schema rule-name) value))


(deftest schemas-test
  (testing "individual schemas"
    (doseq [schema-name (keys schema/schemas)]
      (testing schema-name
        (is (m/schema? (schema/schema schema-name))))))

  (testing "canonical"
    (is (m/schema? (schema/schema ::schema/canonical)))))


(deftest schema-test
  (let [schema ::schema/ident]
    (are [expected value] (= expected (valid? schema value))
      true 'ident
      false :ident
      false "ident"))


  (let [schema ::schema/constant]
    (are [expected value] (= expected (valid? schema value))
      true 1
      true true
      false []
      false {}))


  (let [schema ::schema/operand]
    (are [expected value] (= expected (valid? schema value))
      true 1
      true true
      true :ident
      false 'ident
      false []
      false {}))


  (let [schema ::schema/binary-operator]
    (are [expected value] (= expected (valid? schema value))
      true '[= 1 2]
      true '[= true "true"]
      true '[= true :some-attr]))


  (let [schema ::schema/boolean-binary-operator]
    (are [expected value] (= expected (valid? schema value))
      true '[and [= 1 2] [= 2 9]]
      true '[or [= 1 2] [= 2 9]]))


  (let [schema ::schema/restriction]
    (are [expected value] (= expected (valid? schema value))
      true '[and [= 1 2] [= 2 9]]
      true '[or [= 1 2] [= 2 9]]))


  (let [schema ::schema/expr]
    (are [expected value] (= expected (valid? schema value))
      true '[if [= :a 2] :a :b]
      true '[or [= 1 2] [<> 2 9]]))


  (let [schema ::schema/relation]
    (are [expected value] (= expected (valid? schema value))
      true '[relation :artist]))


  (let [schema ::schema/join]
    (are [expected value] (= expected (valid? schema value))
      true '[join
             [relation :artist]
             [relation :album]
             [= :artist/id :album/artist-id]]))


  (let [schema ::schema/full-join]
    (are [expected value] (= expected (valid? schema value))
      true '[full-join
             [relation :artist]
             [relation :album]
             [= :artist/id :album/artist-id]]))


  (let [schema ::schema/project]
    (are [expected value] (= expected (valid? schema value))
      true '[project
             [relation :artist]
             [:artist/name]]))


  (let [schema ::schema/project-away]
    (are [expected value] (= expected (valid? schema value))
      true '[project-away
             [relation :artist]
             [:artist/id]]))


  (let [schema ::schema/rename]
    (are [expected value] (= expected (valid? schema value))
      true '[rename
             [relation :artist]
             [[:artist/id :id]]]))


  (let [schema ::schema/restrict]
    (are [expected value] (= expected (valid? schema value))
      true '[restrict
             [relation :artist]
             [or [= :artist/id 1] [= :artist/name "Jimi"]]]))


  (let [schema ::schema/extend]
    (are [expected value] (= expected (valid? schema value))
      true '[extend
             [relation :artist]
             [[:artist/first-name [first [split :artist/name " "]]]]]))


  (let [schema ::schema/union]
    (are [expected value] (= expected (valid? schema value))
      true '[union
             [relation :artist]
             [relation :artist]]))


  (let [schema ::schema/limit]
    (are [expected value] (= expected (valid? schema value))
      true '[limit
             [relation :artist]
             10]))

  (let [schema ::schema/offset]
    (are [expected value] (= expected (valid? schema value))
      true '[offset
             [relation :artist]
             10]))

  (let [schema ::schema/order-by]
    (are [expected value] (= expected (valid? schema value))
      true '[order-by
             [relation :artist]
             [:artist/name :artist/id]]))

  (let [schema ::schema/canonical]
    (are [expected value] (= expected (valid? schema value))
      true '[restrict
             [relation :artist]
             [or [= :artist/id 1] [= :artist/name "Jimi"]]]))

  ;; Not yet implemented
  #_(let [schema ::schema/canonical]
      (are [expected value] (= expected (valid? schema value))
        true '[project
               [restrict
                [relation :artist]
                [or [= :artist/id 1] [= :artist/name "Jimi"]]]
               [:artist/name]])))

