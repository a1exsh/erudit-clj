(ns erudit-test
  (:require [clojure.test :refer [deftest is]]
            [erudit :as e]))

(deftest multi-subsets
  (is      (e/multi-subset? {} {\a 1}))
  (is      (e/multi-subset? {\a 1 \b 2} {\a 2 \b 3 \c 1}))
  (is (not (e/multi-subset? {\a 2 \b 4} {\a 2 \b 3 \c 1})))
  (is (not (e/multi-subset? {\a 1 \b 1} {\c 1}))))

(deftest enough-letters
  (is      (e/enough-letters-for-word? {\a 1 \b 2 \y 1} "baby"))
  (is (not (e/enough-letters-for-word? {\a 1 \d 2 \y 1} "daddy"))))

(deftest stems
  (is (= #{"abc" "ab" "bc" "a" "b" "c" ""} (e/enum-stems "abc")))
  (is (= #{"abc" "ab" "bc"} (set (e/enum-stems "abc" 1)))))

(deftest precursors
  (is (= #{"ab" "b"} (set (e/find-precursors #{"abc" "ab" "b"} "abc"))))
  (is (empty? (e/find-precursors #{"abc" "ab" "b" "def"} "def")))

  (is (= #{"ra" "crab" "scrab"}
         (set (e/find-precursors #{"crab" "ra" "scrab"} "scrabble")))))

(deftest word-chains
  (is (= {"scrab" {"crab" {"ra" {}} "ra" {}}
          "crab" {"ra" {}}
          "ra" {}}
         (e/find-word-chains #{"crab" "ra" "scrab"} "scrabble"))))

(deftest full-solutions
  (is (e/full-solution? {"scrab" {"crab" {}}}))
  (is (not (e/full-solution? {"somethinglong" {"something" {}}}))))
