(ns com.gfredericks.test.chuck.fns-test
  (:require [clojure.pprint :as pp]
            [clojure.test :refer [is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as qc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.fns :as sut]
            [com.gfredericks.test.chuck.generators :as gen']))

(deftest pure-fn-gen-test
  (is (= [#{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M}]
         (mapv (gen/generate (sut/pure-fn-gen gen/any-printable) 3 1)
               (repeat 10 45644666))))
  (let [log (atom [])
        {{[f n] :smallest} :shrunk
         :as shrunk} (qc/quick-check 100
                                     (prop/for-all [f (gen/fmap
                                                        (fn [f]
                                                          (swap! log conj ["generated fn"])
                                                          (fn [& args]
                                                            (let [res (try (apply f args)
                                                                           (catch Throwable e
                                                                             (swap! log conj ["fail" args e])
                                                                             (throw e)))]
                                                              (swap! log conj ["call" args :=> res])
                                                              res)))
                                                        (sut/pure-fn-gen gen/small-integer))
                                                    n gen/small-integer]
                                                   (not-any? (comp #{-88} f) (range (Math/abs n))))
                                     :seed 0
                                     :reporter-fn #(swap! log conj %))]
    (pp/pprint
      [(mapv f (range (Math/abs n)))
       shrunk
       @log]))
  )

(deftest impure-fn-gen-test
  (is (= '[{} {} [-2.0] [] [#uuid "e25dcee2-4b3c-493a-acb7-f5bbf30ca2d1"] {} [f1] () [] {13788N 1.625}]
         (mapv (gen/generate (sut/impure-fn-gen gen/any-printable) 3 1)
               (repeat 10 45644666))))
  (is (= '[#{} {} (3 \e) (#uuid "7274f537-d060-4e4b-8c9f-611b69c99efd" :s) {} 0 #{} [1.0] () []]
         (mapv (gen/generate (sut/impure-fn-gen gen/any-printable) 3 1)
               (repeat 10 4564466))))
  )
