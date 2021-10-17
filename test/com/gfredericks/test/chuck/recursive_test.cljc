(ns com.gfredericks.test.chuck.recursive-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test
             #?(:clj :refer :cljs :refer-macros) [defspec]]
            [clojure.test.check.generators :as gen]
            [#?(:clj clj-time.core :cljs cljs-time.core) :as ct]
            [#?(:clj clj-time.coerce :cljs cljs-time.coerce) :as ctc]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.generators :as gen'
             #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.recursive :as sut]))

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
  (declare Pong)

  (def Ping
    (tagged-recursive-gen
      #'Ping
      {}
      (fn [gen-for]
        (gen/tuple (gen/return :ping)
                   (gen-for Pong)))
      (gen/return nil)))

  

  (defmutualgen ping-pong-mutual-gen)
  (extend-mutual-gen
    ping-pong-mutual-gen
    assoc :nil (gen/return nil))
  (extend-mutual-gen
    ping-pong-mutual-gen
    assoc :pong (fn [gen-for]
                  (gen/tuple (gen/return :pong)
                             (gen-for [:ping]))))
  (extend-mutual-gen
    ping-pong-mutual-gen
    assoc :ping (fn [gen-for]
                  (gen/tuple (gen/return :ping)
                             (gen-for [:pong]))))

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
       (fn [pong]
         (gen/tuple (gen/return :pong)
                    (gen/recursive-gen
                      (fn [ping]
                        (gen/tuple (gen/return :ping)
                                   pong))
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
  Uses lower-level sut/mutual-gens."
  (sut/mutual-gens ping-pong-mutual-gens-args))

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

(defspec mutual-gens-ping-spec {:num-tests 100
                                ;#_
                                :seed 1634498746660}
  (prop/for-all
    [ping (:ping ping-pong-gens)]
    (valid-ping? ping)))

(defspec mutual-gens-pong-spec {:num-tests 100
                                ;#_
                                :seed 1634498746641}
  (prop/for-all
    [ping (:pong ping-pong-gens)]
    (valid-pong? ping)))

(def juxtaposed-ping-pong-generator
  (let [{:keys [ping pong]} ping-pong-gens]
    (gen/tuple ping
               pong
               (sut/combine-mutual-gens ping-pong-gens)
               (sut/mutual-gen ping-pong-mutual-gens-args))))

(defspec mutual-gens-juxtaposed-ping-pong-generator-spec {:num-tests 100
                                                          ;#_
                                                          :seed 1634498746649}
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
  (sut/mutual-gen
    {:ce {:c {:has-result (fn [gen-for]
                            (gen/tuple (gen/return 'has-result)
                                       (gen-for [:ce :c])
                                       (gen-for [:ce :e])))
              :boolean gen/boolean}
          :e {:if (fn [gen-for]
                    (gen/tuple (gen/return 'has-result)
                               (gen-for [:ce :c])
                               (gen-for [:ce :e])))
              :plus (fn [gen-for]
                      (gen/tuple (gen/return '+)
                                 (gen-for [:ce :e])
                                 (gen-for [:ce :e])))
              :integer gen/large-integer}}}))

(comment
  (gen/recursive-gen
    (fn [])
    [])
  )

(def ast-gen-smaller
  (sut/mutual-gen
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
  (sut/mutual-gen
    {:ce {:c {:has-result (fn [gen-for]
                            (gen/tuple (gen/return 'has-result)
                                       (gen-for [:ce :c])
                                       (gen-for [:ce :e])))
              :boolean gen/boolean}
          :e ^{::gen'/frequency 5
               ::gen'/shrink-order -1}
          {:if (fn [gen-for]
                 (gen/tuple (gen/return 'if)
                            (gen-for [:ce :c])
                            (gen-for [:ce :e])))
           :boolean gen/boolean}}}))

(comment
  ((requiring-resolve 'clojure.repl/pst) 10000)
  (gen/sample ast-gen)
  (gen/sample ast-gen-smaller)
  )

;; From https://github.com/frenchy64/mini-occ/blob/288625d9ea0204a1724fe404cc2c5c125f1af403/src/mini_occ/core.clj#L11-L14

;;  e  ::= x | (if e e e) | (lambda (x :- t) e) | (e e*) | #f | n? | add1
;;  t  ::= [x : t -> t] | (not t) | (or t t) | (and t t) | #f | N | Any
;;  p  ::= (is e t) | (not p) | (or p p) | (and p p) | (= e e)

(defn list-tuple [& args]
  (gen/fmap #(apply list %) (apply gen/tuple args)))

(defn list-tuple* [& args]
  (assert args)
  (gen/fmap (fn [[bl l]]
              (apply list (concat bl l)))
            (gen/tuple (apply gen/tuple (butlast args))
                       (last args))))

(def mini-occ-gens
  (sut/mutual-gens
    {:e {:x gen/symbol
         :if (fn [gen-for]
               (list-tuple (gen/return 'if)
                           (gen-for [:e])
                           (gen-for [:e])
                           (gen-for [:e])))
         :lambda (fn [gen-for]
                   (list-tuple (gen/return 'lambda)
                               (list-tuple (gen-for [:e :x])
                                           (gen/return :-)
                                           (gen-for [:t]))
                               (gen-for [:e])
                               (gen-for [:e])
                               (gen-for [:e])))
         :app (fn [gen-for]
                (list-tuple* (gen-for [:e])
                             (gen/list (gen-for [:e]))))
         :false (gen/return false)
         :number gen/large-integer
         :add1 (gen/return 'add1)}
     :t {:fn (fn [gen-for]
               (gen/tuple (gen-for [:e :x])
                          (gen/return :-)
                          (gen-for [:t])
                          (gen/return :->)
                          (gen-for [:t])))
         :not (fn [gen-for]
                (list-tuple (gen/return 'not)
                            (gen-for [:t])))
         :or (fn [gen-for]
               (list-tuple (gen/return 'or)
                           (gen-for [:t])
                           (gen-for [:t])))
         :and (fn [gen-for]
                (list-tuple (gen/return 'and)
                            (gen-for [:t])
                            (gen-for [:t])))
         :false (gen/return false)
         :N (gen/return 'N)
         :Any (gen/return 'Any)}
     :p {;; FIXME punt
         :fake-leaf-gen (gen/return :fake-leaf-gen)
         ;; hmm this is a leaf gen :)
         ;; idea: search for leaf gens by stubbing gen-for and collecting paths.
         ;; ah, but what about (gen/list (gen-for [:p]))? since it can generate the
         ;; empty list, it is a leaf gen.
         :is (fn [gen-for]
               (list-tuple (gen/return 'is)
                           (gen-for [:e])
                           (gen-for [:t])))
         ;; also a leaf gen
         :eq (fn [gen-for]
               (list-tuple (gen/return '=)
                           (gen-for [:e])
                           (gen-for [:e])))
         :not (fn [gen-for]
                (list-tuple (gen/return 'not)
                            (gen-for [:p])))
         :or (fn [gen-for]
               (list-tuple (gen/return 'or)
                           (gen-for [:p])
                           (gen-for [:p])))
         :and (fn [gen-for]
                (list-tuple (gen/return 'and)
                            (gen-for [:p])
                            (gen-for [:p])))}}))

(comment
  (gen/generate
    (sut/combine-mutual-gens
      (:e mini-occ-gens))
    100)
  (do mini-occ-gens)
  )

;; defrecursive-gen

(declare Pong)

(sut/defrecursive-gen Ping
  (fn [gen-for]
    (gen/tuple (gen/return :ping)
               (gen-for Pong)))
  (gen/return nil))

(sut/defrecursive-gen Pong
  (fn [gen-for]
    (gen/tuple (gen/return :pong)
               (gen-for Ping)))
  (gen/return nil))

;; defrecursive-cases

(sut/defrecursive-cases Foo)
(sut/defrecursive-case Foo
  ::case1
  (gen/return nil))
(sut/defrecursive-case Foo
  ::case2
  (fn [gen-for]
    (gen/vector (gen-for Foo))))

(comment
  ((requiring-resolve 'clojure.repl/pst) 100)
  (do Foo)
  (gen/sample Foo)
  )

;; mini-occ + defrecursive-cases

(sut/defrecursive-cases MO-E)
(sut/defrecursive-case MO-E
  :x gen/symbol)

(comment
  ((requiring-resolve 'clojure.repl/pst) 100)
  (do Foo)
  (gen/sample MO-E)
  )
