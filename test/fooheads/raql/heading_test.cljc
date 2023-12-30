(ns fooheads.raql.heading-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.heading :as h]
    [fooheads.setish :as set]))


(def artist-heading
  [{:attr/name :artist/artist-id :attr/type :integer}
   {:attr/name :artist/name      :attr/type :string}])


(def album-heading
  [{:attr/name :album/album-id   :attr/type :integer}
   {:attr/name :album/title      :attr/type :string}
   {:attr/name :album/artist-id  :attr/type :integer}])


(def invoice-line
  [{:attr/name :invoice-line/invoice-line-id :attr/type :integer}
   {:attr/name :invoice-line/invoice-id      :attr/type :integer}
   {:attr/name :invoice-line/track-id        :attr/type :integer}
   {:attr/name :invoice-line/unit-price      :attr/type :decimal}
   {:attr/name :invoice-line/quantity        :attr/type :integer}])


(def heading-relmap
  {:artist artist-heading
   :album album-heading
   :invoice-line invoice-line})


(def inferrers
  {'< (constantly :boolean)
   '= (constantly :boolean)
   'and (constantly :boolean)
   'not (constantly :boolean)
   'sum (constantly :integer)})


(defn infer-heading [expr]
  (#'h/infer
   heading-relmap
   inferrers
   expr))


(deftest infer-type-test
  (is (= :boolean
         (#'h/-infer-type
          [{:attr/name :artist/age :attr/type :integer}]
          inferrers
          '[and [< 27 :artist/age]
            [< :artist/age 65]]))))


(deftest infer-heading-test
  (testing "relation"
    (is (= (:artist heading-relmap)
           (infer-heading '[relation :artist]))))

  (testing "join"
    (is (= (set/union
             (:artist heading-relmap)
             (:album heading-relmap))
           (infer-heading '[join
                            [relation :artist]
                            [relation :album]
                            [= :artist/artist-id :album/artist-id]]))))

  (testing "full-join"
    (is (= (set/union
             (:artist heading-relmap)
             (:album heading-relmap))
           (infer-heading '[full-join
                            [relation :artist]
                            [relation :album]
                            [= :artist/artist-id :album/artist-id]]))))

  (testing "restrict"
    (is (= (:artist heading-relmap)
           (infer-heading '[restrict
                            [relation :artist]
                            [= :artist/name "Jimi Hendrix"]]))))

  (testing "project"
    (is (= [{:attr/name :artist/name :attr/type :string}]
           (infer-heading '[project
                            [relation :artist]
                            [:artist/name]]))))


  (testing "project-away"
    (is (= [{:attr/name :artist/name :attr/type :string}]
           (infer-heading '[project-away
                            [relation :artist]
                            [:artist/artist-id]]))))

  (testing "rename"
    (is (= [{:attr/name :artist/some-id   :attr/type :integer}
            {:attr/name :artist/some-name :attr/type :string}]
           (infer-heading '[rename
                            [relation :artist]
                            [[:artist/artist-id :artist/some-id]
                             [:artist/name :artist/some-name]]])))

    (testing "order should not matter"
      (is (= [{:attr/name :artist/some-id   :attr/type :integer}
              {:attr/name :artist/some-name :attr/type :string}]
             (infer-heading '[rename
                              [relation :artist]
                              [[:artist/name :artist/some-name]
                               [:artist/artist-id :artist/some-id]]])))))


  (testing "alias"
    (is (= [{:attr/name :artist/artist-id :attr/type :integer}
            {:attr/name :artist/name      :attr/type :string}
            {:attr/name :artist/some-id   :attr/type :integer}
            {:attr/name :artist/some-name :attr/type :string}]
           (infer-heading '[alias
                            [relation :artist]
                            [[:artist/artist-id :artist/some-id]
                             [:artist/name :artist/some-name]]]))))


  (testing "union"
    (is (= (:album heading-relmap)
           (infer-heading '[union
                            [relation :album]
                            [relation :album]]))))


  (testing "aggregate-by"
    (is (= [{:attr/name :invoice-line/invoice-id :attr/type :integer}
            {:attr/name :quantity                :attr/type :integer}]
           (infer-heading '[aggregate-by
                            [relation :invoice-line]
                            [:invoice-line/invoice-id]
                            {:quantity [sum :invoice-line/quantity]}]))))

  (testing "order-by"
    (is (= (:artist heading-relmap)
           (infer-heading '[order-by [relation :artist] [:artist/id]]))))


  (testing "inference"
    (let [heading-relmap
          (merge-with
            set/union
            heading-relmap
            {:artist [{:attr/name :artist/age :attr/type :integer}]})
          infer-heading (partial h/infer heading-relmap inferrers)]
      (is (= (set/union
               (:artist heading-relmap)
               [{:attr/name :artist/adult? :attr/type :boolean}
                {:attr/name :artist/discount? :attr/type :boolean}])
             (infer-heading '[extend
                              [relation :artist]
                              {:artist/adult? [and [< 18 :artist/age]
                                               [< :artist/age 65]]
                               :artist/discount? [not :artist/adult?]}
                              (:artist/artist-id)]))))))


(comment
  (h/infer heading-relmap inferrers '[project
                                      [relation :artist]
                                      [:artist/age]]))

