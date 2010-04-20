;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns example.ideadb.users
  (:use (sandbar core forms util user-manager)
        (example.ideadb data
                        properties)))

(defn user-data-functions [k]
  (cond (= k :save)
        (fn [m]
          (if (= :app_user (:type m))
            (standard-save-user m
                                filter-and-sort-records
                                delete-rec
                                save)
            (save m)))
        (= k :load) filter-and-sort-records
        (= k :lookup)
        (fn [type id]
          (if (= :app_user type)
            (standard-lookup-user type id
                                  filter-and-sort-records
                                  find-by-id)
            (find-by-id type id)))
        (= k :delete)
        (fn [type id]
          (if (= :app_user type)
            (standard-delete-user type id
                                  filter-and-sort-records
                                  delete-by-id)
            (delete-by-id type id)))))
