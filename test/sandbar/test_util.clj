(ns sandbar.test_util
  (:use [clojure.test]
        [sandbar.util]))
 
(deftest test-remove-file-ext
  (is (= (remove-file-ext "test.txt") "test"))
  (is (= (remove-file-ext "test.file.txt") "test.file"))
  (is (= (remove-file-ext "test") "test")))
