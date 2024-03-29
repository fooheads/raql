(ns fooheads.raql.rewrite-test
  (:require
    [clojure.test :refer [deftest is]]
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

