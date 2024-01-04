(ns fooheads.raql.core-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.core :as raql]
    [fooheads.raql.heading-test :as heading-test]))


(deftest compile-test
  (testing "minimal expression"
    (is (= '{:operator relation
             :args [:artist]
             :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                       {:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]}
           (raql/compile
             heading-test/heading-relmap
             heading-test/inferrers
             '[relation :artist]))))

  (testing "simple expression"
    (is (= '{:operator rename
             :args [{:operator relation
                     :args [:artist]
                     :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                               {:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]}
                    [[:artist/name :artist/some-name]]]
             :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                       {:attr/name :artist/some-name :attr/type :string :attr/relvar-name :artist}]}

           (raql/compile
             heading-test/heading-relmap
             heading-test/inferrers
             '[rename
               [relation :artist]
               [[:artist/name :artist/some-name]]]))))

  (testing "arguments"
    (is (= '{:operator restrict
             :args [{:operator relation
                     :args [:artist]
                     :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                               {:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]}
                    [= :artist/name ?name]]
             :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                       {:attr/name :artist/name :attr/type :string :attr/relvar-name :artist}]}

           (raql/compile
             heading-test/heading-relmap
             heading-test/inferrers
             '[restrict
               [relation :artist]
               [= :artist/name ?name]]))))


  (testing "complex expression"
    (is (= '{:operator project
             :args [{:operator restrict
                     :args [{:operator rename
                             :args [{:operator join
                                     :args [{:operator relation
                                             :args [:artist]
                                             :heading [{:attr/name :artist/artist-id
                                                        :attr/type :integer
                                                        :attr/relvar-name :artist}
                                                       {:attr/name :artist/name
                                                        :attr/type :string
                                                        :attr/relvar-name :artist}]}
                                            {:operator relation
                                             :args [:album]
                                             :heading [{:attr/name :album/album-id
                                                        :attr/type :integer
                                                        :attr/relvar-name :album}
                                                       {:attr/name :album/title
                                                        :attr/type :string
                                                        :attr/relvar-name :album}
                                                       {:attr/name :album/artist-id
                                                        :attr/type :integer
                                                        :attr/relvar-name :album}]}
                                            [= :artist/id :album/artist-id]]
                                     :heading [{:attr/name :artist/artist-id
                                                :attr/type :integer
                                                :attr/relvar-name :artist}
                                               {:attr/name :artist/name
                                                :attr/type :string
                                                :attr/relvar-name :artist}
                                               {:attr/name :album/album-id
                                                :attr/type :integer
                                                :attr/relvar-name :album}
                                               {:attr/name :album/title
                                                :attr/type :string
                                                :attr/relvar-name :album}
                                               {:attr/name :album/artist-id
                                                :attr/type :integer
                                                :attr/relvar-name :album}]}
                                    [[:artist/name :artist/some-name]]]
                             :heading [{:attr/name :artist/artist-id
                                        :attr/type :integer
                                        :attr/relvar-name :artist}
                                       {:attr/name :artist/some-name
                                        :attr/type :string
                                        :attr/relvar-name :artist}
                                       {:attr/name :album/album-id :attr/type :integer :attr/relvar-name :album}
                                       {:attr/name :album/title :attr/type :string :attr/relvar-name :album}
                                       {:attr/name :album/artist-id
                                        :attr/type :integer
                                        :attr/relvar-name :album}]}
                            [= :artist/some-name "Jimi Hendrix"]]
                     :heading [{:attr/name :artist/artist-id :attr/type :integer :attr/relvar-name :artist}
                               {:attr/name :artist/some-name :attr/type :string :attr/relvar-name :artist}
                               {:attr/name :album/album-id :attr/type :integer :attr/relvar-name :album}
                               {:attr/name :album/title :attr/type :string :attr/relvar-name :album}
                               {:attr/name :album/artist-id :attr/type :integer :attr/relvar-name :album}]}
                    [:artist/some-name]]
             :heading [{:attr/name :artist/some-name :attr/type :string :attr/relvar-name :artist}]}

           (raql/compile
             heading-test/heading-relmap
             heading-test/inferrers
             '[project
               [restrict
                [rename
                 [join
                  [relation :artist]
                  [relation :album]
                  [= :artist/id :album/artist-id]]
                 [[:artist/name :artist/some-name]]]
                [= :artist/some-name "Jimi Hendrix"]]
               [:artist/some-name]])))))

