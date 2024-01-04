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


;; Not yet implemented
#_(deftest infer-type-test
    (is (= :boolean
           (#'h/-infer-type
            [{:attr/name :artist/age :attr/type :integer}]
            inferrers
            '[and [< 27 :artist/age]
              [< :artist/age 65]]))))


(deftest infer-test
  (testing "relation"
    (is (= (heading :artist)
           (infer '[relation :artist])))

    (is (= {:msg "No relation named :artist2"
            :relvar-name :artist2}
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
               [= :artist/name "Jimi Hendrix"]]))))

  (testing "project"
    (testing "normal case"
      (is (= [{:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]
             (infer
               '[project
                 [relation :artist]
                 [:artist/name]]))))

    (testing "should not be possible to project non-existing attrs"
      (is (= {:msg "Can't project attrs not present in relation: [:artist/age]"}
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
      (is (= {:msg "Can't project-away attrs not present in relation: [:artist/age]"}
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
      (is (= {:msg "Can't rename attrs not present in relation: [:artist/age]"}
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


  ;; Not yet implmented
  #_(testing "alias"
      (testing "normal case"
        (is (= [{:attr/name :artist/artist-id :attr/type :integer}
                {:attr/name :artist/name      :attr/type :string}
                {:attr/name :artist/some-id   :attr/type :integer}
                {:attr/name :artist/some-name :attr/type :string}]
               (infer
                 '[alias
                   [relation :artist]
                   [[:artist/artist-id :artist/some-id]
                    [:artist/name :artist/some-name]]]))))

      (testing "should not be possible to alias non-existing attrs"
        (is (= {:msg "Can't alias attrs not present in relation: [:artist/age]"}
               (thrown-ex-data
                 [:msg]
                 (infer
                   '[alias
                     [relation :artist]
                     [[:artist/age :artist/some-age]]]))))))

  (testing "union"
    (is (= (heading :album)
           (infer
             '[union
               [relation :album]
               [relation :album]]))))


  ;; Not yet implemented
  #_(testing "aggregate-by"
      (testing "normal case"
        (is (= [{:attr/name :invoice-line/invoice-id :attr/type :integer}
                {:attr/name :quantity                :attr/type :integer}]
               (infer
                 '[aggregate-by
                   [relation :invoice-line]
                   [:invoice-line/invoice-id]
                   {:quantity [sum :invoice-line/quantity]}]))))

      (testing "should not be possible to aggregate-by non-existing attrs"
        (is (= {:msg "Can't aggregate-by attrs not present in relation: [:invoice-line/age]"}
               (thrown-ex-data
                 [:msg]
                 (infer
                   '[aggregate-by
                     [relation :invoice-line]
                     [:invoice-line/age]
                     {:quantity [sum :invoice-line/quantity]}]))))))


  (testing "order-by"
    (testing "normal case"
      (is (= (heading :artist)
             (infer '[order-by [relation :artist] [:artist/artist-id]])))

      (is (= (heading :artist)
             (infer '[order-by [relation :artist] [[:artist/artist-id :desc]]]))))

    (testing "should not be possible to order-by non-existing attrs"
      (is (= {:msg "Can't order-by attrs not present in relation: [:artist/age]"}
             (thrown-ex-data
               [:msg]
               (infer '[order-by [relation :artist] [:artist/age]]))))))


  ;; Not yet implemented
  #_(testing "extend"
      (testing "normal case"
        (is (= (set/union
                 (:invoice-line heading-relmap)
                 [{:attr/name :invoice-line/refund? :attr/type :boolean}])
               (infer
                 '[extend
                   [relation :invoice-line]
                   {:invoice-line/refund? [< :invoice-line/quantity 0]}]))))


      (testing "inference"
        (testing "types"
          (let [heading-relmap
                (merge-with
                  set/union
                  heading-relmap
                  {:artist [{:attr/name :artist/age :attr/type :integer}]})
                infer (partial h/infer heading-relmap inferrers)]
            (is (= (set/union
                     (:artist heading-relmap)
                     [{:attr/name :artist/adult? :attr/type :boolean}
                      {:attr/name :artist/discount? :attr/type :boolean}])
                   (infer
                     '[extend
                       [relation :artist]
                       {:artist/adult? [and [< 18 :artist/age]
                                        [< :artist/age 65]]
                        :artist/discount? [not :artist/adult?]}
                       (:artist/artist-id)])))))

        (testing "circular references should cause error"
          (is (= {:msg "Circular reference: :artist/child? already referred."}
                 (thrown-ex-data
                   [:msg]
                   (infer
                     '[extend
                       [relation :artist]
                       {:artist/adult? [not :artist/child?]
                        :artist/child? [not :artist/adult?]}
                       (:artist/artist-id)]))))))))


(comment
  (infer '[relation :artist])
  (decorate '[relation :artist])
  (decorate '[project
               [relation :artist]
               [:artist/name]])

  (decorate '[project
               [rename
                [relation :artist]
                {:artist/name :artist/full-name}]
               [:artist/full-name]]))


