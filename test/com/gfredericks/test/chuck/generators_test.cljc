(ns com.gfredericks.test.chuck.generators-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test
             #?(:clj :refer :cljs :refer-macros) [defspec]]
            [clojure.test.check.generators :as gen]
            [#?(:clj clj-time.core :cljs cljs-time.core) :as ct]
            [#?(:clj clj-time.coerce :cljs cljs-time.coerce) :as ctc]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.generators :as gen'
             #?@(:cljs [:include-macros true])]))

(def lists-and-counts
  (gen'/for [nums (gen/vector gen/nat)
             :let [cardinality (count nums)]]
    [nums cardinality]))

(defspec for-works-correctly 100
  (prop/for-all [[nums cardinality] lists-and-counts]
    (= (count nums) cardinality)))

(defspec for-accepts-empty-bindings 100
  (prop/for-all [x (gen'/for [] 42)]
    (= x 42)))

(def lists-with-two-of-their-elements
  (gen'/for [nums (gen/vector gen/nat)
             :let [cardinality (count nums)]
             :when (> cardinality 1)
             x (gen/elements nums)
             :let [[befores [_x & afters]] (split-with #(not= % x) nums)
                   nums-x (concat befores afters)]
             y (gen/elements nums-x)]
    [nums x y]))

(defspec complex-for-works-correctly 100
  (prop/for-all [[nums x y] lists-with-two-of-their-elements]
    (let [f (frequencies nums)]
      ;; check that both x and y are in the list
      (or (and (= x y) (> (f x) 1))
          (and (not= x y) (pos? (f x)) (pos? (f y)))))))

(def destructuring-usage
  (gen'/for [{:keys [foo]} (gen/hash-map :foo gen/nat)
             :let [unused-binding 42]
             vs (gen/vector gen/boolean foo)]
    [foo vs]))

(defspec destructuring-usage-spec 100
  (prop/for-all [[n vs] destructuring-usage]
    (= n (count vs))))

(def parallel-usage
  (gen'/for [:parallel [x gen/nat
                        y gen/boolean]]
    [x y]))

(defspec parallel-usage-spec 100
  (prop/for-all [[x y] parallel-usage]
    (and (>= x 0)
         (or (= true y) (= false y)))))

(def parallel-as-second-clause
  (gen'/for [n gen/nat
             :parallel [v1 (gen/vector gen/boolean n)
                        v2 (gen/vector gen/boolean n)]]
    [n (concat v1 v2)]))

(defspec parallel-as-second-clause-spec 100
  (prop/for-all [[n v] parallel-as-second-clause]
    (= (* 2 n) (count v))))

(defspec bounded-int-generates-bounded-ints 500
  (let [large-int (gen/choose -200000000 200000000)
        g (gen/bind (gen/tuple large-int large-int)
                    (fn [pair]
                      (let [[low high] (sort pair)]
                        (gen/tuple (gen/return low)
                                   (gen/return high)
                                   (gen'/bounded-int low high)))))]
    (prop/for-all [[low high n] g]
      (<= low n high))))

; TODO: improve the cljs tests for gen'/double
(defspec double-generates-doubles 100
  (prop/for-all [x gen'/double]
    #?(:clj  (instance? Double x)
       :cljs (= js/Number (type x)))))

(defspec subset-in-set 100
  (prop/for-all [s (gen'/subset (range 10))]
    (every? (set (range 10)) s)))

(defn subsequence?
  "Checks if xs is a subsequence of ys."
  [xs ys]
  (or (empty? xs)
      (and (seq ys)
           (= (first xs) (first ys))
           (subsequence? (rest xs) (rest ys)))
      (and (seq ys)
           (subsequence? xs (rest ys)))))

(def subsequence-gen
  (gen'/for [ys (gen/list gen/nat)
             xs (gen'/subsequence ys)]
    [xs ys]))

(defspec subsequence-spec 100
  (prop/for-all [[xs ys] subsequence-gen]
    (subsequence? xs ys)))

(def sub-map-gen
  (gen'/for [m (gen/map gen/string-alphanumeric gen/nat)
             sm (gen'/sub-map m)]
    [m sm]))

(defspec sub-map-spec 100
  (prop/for-all [[m sm] sub-map-gen]
    (every? #(= (find m (key %))
                %)
            sm)))

(defspec datetime-spec 100000
  (prop/for-all [dt (gen'/datetime {:offset-min 0
                                    :offset-max 100
                                    :offset-fns [ct/millis ct/seconds ct/minutes ct/hours ct/days ct/months]})]
                (ct/within? (ct/date-time 2000)
                            (ct/date-time 2009)
                            dt)))

(defn valid-bounded-rec-struct?
  [breadth height coll]
  (if (not-any? coll? coll)
    (and (<= (count coll) breadth)
         (or (zero? height) (pos? height)))
    (and (<= (count coll) breadth)
         (every? identity (map (partial valid-bounded-rec-struct?
                                        breadth
                                        (dec height))
                               coll)))))

(defspec bounded-recursive-gen-spec 100
  (prop/for-all
   [bounded-rec (gen'/bounded-recursive-gen gen/vector
                                            gen/int
                                            10
                                            5)]
   (valid-bounded-rec-struct? 10 5 bounded-rec)))

;; # Mutually-recursive generators

;; ping pong

(def ping-pong-mutual-gens-args
  {:ping (fn [gen-for]
           (gen/tuple (gen/return :ping)
                      (gen-for [:pong])))
   :pong (fn [gen-for]
           (gen/tuple (gen/return :pong)
                      (gen-for [:ping])))
   :nil (gen/return nil)})

(comment
  (gen/one-of
    [(gen/recursive-gen
       (fn [ping]
         (gen/tuple (gen/return :ping)
                    (gen/recursive-gen
                      (fn [pong]
                        (gen/tuple (gen/return :pong)
                                   ping))
                      (gen/return nil))))
       (gen/return nil))
     (gen/recursive-gen
       (fn [ping]
         (gen/tuple (gen/return :ping)
                    (gen/recursive-gen
                      (fn [pong]
                        (gen/tuple (gen/return :pong)
                                   ping))
                      (gen/return nil))))
       (gen/return nil))
     (gen/return nil)])
  (gen/one-of
    [(gen/recursive-gen
       (fn [ping]
         (gen/tuple (gen/return :ping)
                    (gen/recursive-gen
                      (fn [pong]
                        (gen/tuple (gen/return :pong)
                                   ping))
                      (gen/return nil))))
       (gen/return nil))
     (let [gs {}]
       (gen/recursive-gen
         (fn [pong]
           (let [gs (assoc gs :pong pong)]
             (gen/tuple (gen/return :pong)
                        (gen-for gs [:ping]))))
         (gen/return nil)))
     (gen/return nil)])
  )

(def ping-pong-gens
  "Generates nested alternating vectors like
  [:ping [:pong [:ping nil]]]
  Uses lower-level gen'/mutual-gens."
  (gen'/mutual-gens ping-pong-mutual-gens-args))

(defn valid-ping? [v]
  (loop [v v
         expected-first :ping]
    (or (nil? v)
        (and (vector? v)
             (= 2 (count v))
             (if (= (first v) expected-first)
               (case (first v)
                 :ping (recur (second v) :pong)
                 :pong (recur (second v) :ping))
               false)))))

(defn valid-pong? [v]
  (loop [v v
         expected-first :pong]
    (or (nil? v)
        (and (vector? v)
             (= 2 (count v))
             (if (= (first v) expected-first)
               (case (first v)
                 :ping (recur (second v) :pong)
                 :pong (recur (second v) :ping))
               false)))))

(def valid-ping-examples
  [nil
   [:ping nil]
   [:ping [:pong nil]]
   [:ping [:pong [:ping nil]]]])

(def valid-pong-examples
  [nil
   [:pong nil]
   [:pong [:ping nil]]
   [:pong [:ping [:pong nil]]]])

(def invalid-ping-examples
  [:ping
   [:ping]
   [:ping [:ping nil]]
   [:ping [:pong nil] :pong]
   [:ping [:pong [:pong nil]]]])

(def invalid-pong-examples
  [:pong
   [:pong]
   [:pong [:pong nil]]
   [:pong [:ping nil] :ping]
   [:pong [:ping [:ping nil]]]])

(deftest valid-ping-pong-test
  (doseq [v valid-ping-examples]
    (is (true? (valid-ping? v))
        (pr-str v)))
  (doseq [v (concat invalid-ping-examples
                    valid-pong-examples)
          ;; remove scalar values
          :when (not (nil? v))]
    (is (false? (valid-ping? v))
        (pr-str v)))
  (doseq [v valid-pong-examples]
    (is (true? (valid-pong? v))
        (pr-str v)))
  (doseq [v (concat invalid-pong-examples
                    valid-ping-examples)
          ;; remove scalar values
          :when (not (nil? v))]
    (is (false? (valid-pong? v))
        (pr-str v))))

(defspec mutual-gens-ping-spec 100
  (prop/for-all
    [ping (:ping ping-pong-gens)]
    (valid-ping? ping)))

(defspec mutual-gens-pong-spec 100
  (prop/for-all
    [ping (:pong ping-pong-gens)]
    (valid-pong? ping)))

(def juxtaposed-ping-pong-generator
  (let [{:keys [ping pong]} ping-pong-gens]
    (gen/tuple ping
               pong
               (gen'/combine-mutual-gens ping-pong-gens)
               (gen'/mutual-gen ping-pong-mutual-gens-args))))

(defspec mutual-gens-juxtaposed-ping-pong-generator-spec 100
  (prop/for-all
    [pp juxtaposed-ping-pong-generator]
    (and (vector? pp)
         (= 4 (count pp))
         (valid-ping? (nth pp 0))
         (valid-pong? (nth pp 1))
         ((some-fn valid-ping? valid-pong?) (nth pp 2))
         ((some-fn valid-ping? valid-pong?) (nth pp 3)))))

;; AST's

;; c := (has-result c e) | boolean
;; e := (if c e e) | integer | (+ e e)
(def ast-gen
  (gen'/mutual-gen
    {:ce {:c [{:has-result (fn [{{:keys [c e]} :ce}]
                             (gen/tuple (gen/return 'has-result)
                                        (gen'/combine-mutual-gens c)
                                        (gen'/combine-mutual-gens e)))}
              gen/boolean]
          :e [{:if (fn [{{:keys [c e]} :ce}]
                     (gen/tuple (gen/return 'has-result)
                                (gen'/combine-mutual-gens c)
                                (gen'/combine-mutual-gens e)))
               :plus (fn [{{:keys [c e]} :ce}]
                       (gen/tuple (gen/return '+)
                                  (gen'/combine-mutual-gens e)
                                  (gen'/combine-mutual-gens e)))}
              gen/large-integer]}}))

(comment
  (gen/recursive-gen
    (fn [])
    [])
  )

(def ast-gen-smaller
  (gen'/mutual-gen
    {:ce {:c {:has-result (fn [gen-for]
                            (gen/tuple (gen/return 'has-result)
                                       (gen-for [:ce :c])
                                       (gen-for [:ce :e])))
              :boolean gen/boolean}
          :e {:if (fn [gen-for]
                    (gen/tuple (gen/return 'if)
                               (gen-for [:ce :c])
                               (gen-for [:ce :e])))
              :integer gen/large-integer}}}))

#_
(def ast-gen-smaller
  (gen'/mutual-gen
    {:ce {:c {:has-result (fn [gen-for]
                            (gen/tuple (gen/return 'has-result)
                                       (gen-for [:ce :c])
                                       (gen-for [:ce :e])))
              :boolean gen/boolean}
          :e {:if (fn [gen-for]
                    (gen/tuple (gen/return 'if)
                               (gen-for [:ce :c])
                               (gen-for [:ce :e])))}}
     ;; inherited
     :integer gen/large-integer}))

(comment
  ((requiring-resolve 'clojure.repl/pst) 10000)
  ;; FIXME stackoverflow
  (gen/generate ast-gen 0)
  (gen/sample ast-gen-smaller)
  )
