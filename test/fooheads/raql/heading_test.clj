(ns fooheads.raql.heading-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.ast :as ast]
    [fooheads.raql.chinook-schema :as chinook-schema]
    [fooheads.raql.heading :as h]
    [fooheads.setish :as set]
    [fooheads.test :refer [thrown-ex-data]]))


(def heading-relmap (chinook-schema/heading-relmap))


(defn heading [relvar-name]
  (->> heading-relmap :attr (filterv #(= relvar-name (:attr/relvar-name %)))))


(def inferrers
  {'< (constantly :boolean)
   '= (constantly :boolean)
   'and (constantly :boolean)
   'not (constantly :boolean)})


(defn infer [expr]
  (->>
    expr
    (ast/raql->ast)
    (h/infer heading-relmap inferrers)))


(def decorate (partial h/decorate heading-relmap inferrers))


(deftest infer-test
  (testing "relation"
    (is (= (heading :artist)
           (infer '[relation :artist])))

    (is (= {:msg "No relvar named :artist2. Did you mean :artist?"
            :relvar-name :artist2
            :suggestion :artist}
           (thrown-ex-data
             (infer '[relation :artist2])))))

  (testing "join"
    (is (= (set/union
             (heading :artist)
             (heading :album))
           (infer
             '[join
               [relation :artist]
               [relation :album]
               [= :artist/artist-id :album/artist-id]]))))

  (testing "full-join"
    (is (= (set/union
             (heading :artist)
             (heading :album))
           (infer
             '[full-join
               [relation :artist]
               [relation :album]
               [= :artist/artist-id :album/artist-id]]))))

  (testing "left-join"
    (is (= (set/union
             (heading :artist)
             (heading :album))
           (infer
             '[left-join
               [relation :artist]
               [relation :album]
               [= :artist/artist-id :album/artist-id]]))))

  (testing "right-join"
    (is (= (set/union
             (heading :artist)
             (heading :album))
           (infer
             '[right-join
               [relation :artist]
               [relation :album]
               [= :artist/artist-id :album/artist-id]]))))

  (testing "restrict"
    (is (= (heading :artist)
           (infer
             '[restrict
               [relation :artist]
               [= :artist/name "Jimi Hendrix"]])))

    (is (= (heading :artist)
           (infer
             '[restrict
               [relation :artist]
               [in :artist/artist-id [1 2 3]]]))))

  (testing "project"
    (testing "normal case"
      (is (= [{:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]
             (infer
               '[project
                 [relation :artist]
                 [:artist/name]]))))

    (testing "should not be possible to project non-existing attrs"
      (is (= {:msg "Can't project :artist/age, not present in relation [:artist/artist-id :artist/name]"}
             (thrown-ex-data
               [:msg]
               (infer
                 '[project
                   [relation :artist]
                   [:artist/age]]))))))

  (testing "project-away"
    (testing "normal case"
      (is (= [{:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]
             (infer
               '[project-away
                 [relation :artist]
                 [:artist/artist-id]]))))

    (testing "should not be possible to project-away non-existing attrs"
      (is (= {:msg "Can't project-away :artist/age, not present in relation [:artist/artist-id :artist/name]"}
             (thrown-ex-data
               [:msg]
               (infer
                 '[project-away
                   [relation :artist]
                   [:artist/age]]))))))

  (testing "rename"
    (testing "normal case"
      (is (= [{:attr/name :artist/some-id   :attr/type :integer :attr/relvar-name :artist}
              {:attr/name :artist/some-name :attr/type :string  :attr/relvar-name :artist}]
             (infer
               '[rename
                 [relation :artist]
                 [[:artist/artist-id :artist/some-id]
                  [:artist/name :artist/some-name]]]))))

    (testing "should not be possible to rename non-existing attrs"
      (is (= {:msg "Can't rename :artist/age, not present in relation [:artist/artist-id :artist/name]"}
             (thrown-ex-data
               [:msg]
               (infer
                 '[rename
                   [relation :artist]
                   [[:artist/age :artist/some-name]]])))))

    (testing "order should not matter"
      (is (= [{:attr/name :artist/some-id   :attr/type :integer :attr/relvar-name :artist}
              {:attr/name :artist/some-name :attr/type :string  :attr/relvar-name :artist}]
             (infer
               '[rename
                 [relation :artist]
                 [[:artist/name :artist/some-name]
                  [:artist/artist-id :artist/some-id]]])))))


  (testing "union"
    (is (= (mapv #(dissoc % :attr/relvar-name) (heading :album))
           (infer
             '[union
               [relation :album]
               [relation :album]])))

    (is (= [{:attr/name :some/string :attr/type :string}]
           (infer
             '[union
               [project [rename [relation :album] {:album/title :some/string}] [:some/string]]
               [project [rename [relation :track] {:track/name :some/string}] [:some/string]]]))))


  (testing "order-by"
    (testing "normal case"
      (is (= (heading :artist)
             (infer '[order-by [relation :artist] [:artist/artist-id]])))

      (is (= (heading :artist)
             (infer '[order-by [relation :artist] [[:artist/artist-id :desc]]]))))

    (testing "should not be possible to order-by non-existing attrs"
      (is (= {:msg "Can't order-by :artist/age, not present in relation [:artist/artist-id :artist/name]"}
             (thrown-ex-data
               [:msg]
               (infer '[order-by [relation :artist] [:artist/age]]))))))


  (testing "extend"
    (is (= (into
             (heading :artist)
             [{:attr/name :artist/rating :attr/type :string :attr/relvar-name :artist}])
           (infer
             '[extend
               [relation :artist]
               {:artist/rating [:string "Awesome"]}])))))

