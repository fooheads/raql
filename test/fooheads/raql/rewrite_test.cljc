(ns fooheads.raql.rewrite-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [fooheads.raql.rewrite :as rewrite]))


(def long-track
  '[->
    [relation :Track]
    [restrict [> :Track/Milliseconds 5000000]]])


(def album-with-long-tracks
  '[->
    [relation :long-track]
    [join
     [relation :Album]
     [= :Track/AlbumId :Album/AlbumId]]
    [project [:Album/AlbumId :Album/Title]]])


(deftest apply-views-test
  (testing "relname->expr as map"
    (is (= '[->
             [relation :artist]
             [join
              [->
               [-> [relation :Track] [restrict [> :Track/Milliseconds 5000000]]]
               [join [relation :Album] [= :Track/AlbumId :Album/AlbumId]]
               [project [:Album/AlbumId :Album/Title]]]
              [= :artist/id :album/artist-id]]
             [order-by [:artist-id]]]


           (rewrite/apply-views
             {:album-with-long-tracks album-with-long-tracks
              :long-track long-track}
             '[->
               [relation :artist]
               [join
                [relation :album-with-long-tracks]
                [= :artist/id :album/artist-id]]
               [order-by [:artist-id]]]))))

  (testing "relname->expr as function"
    (is (= '[->
             [relation :artist]
             [join
              [->
               [-> [relation :Track] [restrict [> :Track/Milliseconds 5000000]]]
               [join [relation :Album] [= :Track/AlbumId :Album/AlbumId]]
               [project [:Album/AlbumId :Album/Title]]]
              [= :artist/id :album/artist-id]]
             [order-by [:artist-id]]]


           (rewrite/apply-views
             (fn [relname]
               (case relname
                 :album-with-long-tracks album-with-long-tracks
                 :long-track long-track
                 nil))

             '[->
               [relation :artist]
               [join
                [relation :album-with-long-tracks]
                [= :artist/id :album/artist-id]]
               [order-by [:artist-id]]])))))

