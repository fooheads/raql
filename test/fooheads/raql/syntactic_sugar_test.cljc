(ns fooheads.raql.syntactic-sugar-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.syntactic-sugar :as sugar]))


(deftest expand-threads-test
  (testing "one thread"
    (is (= '[limit
             [offset
              [project
               [rename
                [restrict
                 [relation :artist]
                 [< :artist/id 1000]]
                {:artist/id :id}]
               [:id]]
              100]
             10]

           (sugar/expand-threads
             '[->
               [relation :artist]
               [restrict [< :artist/id 1000]]
               [rename {:artist/id :id}]
               [project [:id]]
               [offset 100]
               [limit 10]]))))

  (testing "nested threads"
    (is (= '[union
             [relation :artist2]
             [join [relation :artist]
              [relation :album]
              [= :artist/id :album/artist-id]]]

           (sugar/expand-threads
             '[->
               [relation :artist2]
               [union
                [->
                 [relation :artist]
                 [join [relation :album] [= :artist/id :album/artist-id]]]]])))))


