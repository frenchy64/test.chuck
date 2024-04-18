(ns com.gfredericks.test.chuck.fns
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]))

(defn seeded
  "Creates a generator that depends on the seed parameter.
  `sized-gen` is a function that takes an integer and returns
  a generator.

  Examples:

      ;; generates an :int with the same seed as the outer sample.
      (gen/sample (seeded (fn [seed]
                            (gen/tuple (gen/return seed)
                                       (generator :int {:seed seed})))))
      => ([-9189345824394269271 0]
          [2069340105756572361 -1]
          [-382523443817476848 -1]
          [-727106358269903677 0]
          [3041036363633372983 -1]
          [-3816606844531533988 1]
          [-5643022030666591503 -1]
          [7456223948749621027 -1]
          [5327329620473603684 34]
          [8284970028005224634 12])"
  [seeded-gen]
  (#'gen/make-gen ;;FIXME bb
   (fn [^clojure.test.check.random.JavaUtilSplittableRandom rnd size]
     (let [seeded-gen (seeded-gen (or (.-state rnd)
                                      (throw (ex-info "Failed to recover seed" {:rnd rnd}))))]
       (gen/call-gen seeded-gen rnd size)))))

(defn- gen-root [options gen rnd size]
  (rose/root (gen/call-gen gen rnd size)))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn sampling-eduction
  "An infinite eduction of generator samples.
  
  :seed - set seed
  :size - set size
  
  Second argument can be a transducer that is applied at the end of the eduction.
  For 2-arity, transducer must be fn?, otherwise is treated as options.

  (sampling-eduction gen/int (take 15))
  ;=> (-1 -1 1 -1 -2 -11 0 -7 -46 122 -1 0 -1 0 0)
  (sequence (take 15) (sampling-eduction gen/int {:seed 10}))
  ;=> (-1 0 -1 3 1 3 -2 -2 5 0 -1 -1 -2 3 -5)
  (sampling-eduction gen/int (take 15) {:seed 10})
  ;=> (-1 0 -1 3 1 3 -2 -2 5 0 -1 -1 -2 3 -5)."
  ([gen]
   (sampling-eduction gen identity nil))
  ([gen ?options-or-xform-fn]
   (let [xform? (fn? ?options-or-xform-fn)]
     (sampling-eduction gen
                        (if xform? ?options-or-xform-fn identity)
                        (when-not xform? ?options-or-xform-fn))))
  ([gen xform {:keys [seed size] :or {size 10} :as options}]
   (eduction
     (map-indexed (fn [iter rnd]
                    (let [size (mod iter size)]
                      (gen-root options gen rnd size))))
     (or xform identity)
     (gen/lazy-random-states (-random seed)))))

(defn- non-zero [n]
  (if (zero? n) Long/MIN_VALUE n))

(defn- summarize-string [x]
  (non-zero (reduce #(unchecked-add %1 (int %2)) 0 x)))

(defn summarize-value [x {:keys [seed size] :as options}]
  (let [n (letfn [(summarize-ident [x]
                    (non-zero (unchecked-add (unknown (namespace x))
                                             (unknown (name x)))))
                  (unknown [x]
                    (non-zero
                      (cond
                        (boolean? x) (if x -1 1)
                        (int? x) x
                        (string? x) (summarize-string x)
                        (ident? x) (summarize-ident x)
                        (coll? x) (reduce #(unchecked-add %1 (unknown %2)) 0
                                          (eduction
                                            (if (and (seq? x)
                                                     (sequential? x)
                                                     (not (counted? x)))
                                              ;; handle infinite seqs
                                              (take 32)
                                              identity)
                                            x))
                        (fn? x) 64
                        (ifn? x) -64
                        (instance? java.math.BigInteger x) (unknown (.toPlainString ^java.math.BigInteger x))
                        (instance? clojure.lang.BigInt x) (unknown (str x))
                        (instance? java.math.BigDecimal x) (unknown (.toPlainString ^java.math.BigDecimal x))
                        (instance? Float x) (Float/floatToIntBits x)
                        (instance? Double x) (Double/doubleToLongBits x)
                        (instance? java.util.concurrent.atomic.AtomicInteger x) (.longValue ^java.util.concurrent.atomic.AtomicInteger x)
                        (instance? java.util.concurrent.atomic.AtomicLong x) (.longValue ^java.util.concurrent.atomic.AtomicLong x)
                        (instance? clojure.lang.IAtom2 x) (unchecked-add (unknown @x) 1024)
                        :else 456456456)))]
            (unchecked-multiply
              (unknown x)
              (unchecked-inc size)))]
    (unchecked-add n seed)))

(defn fn-gen* [->fn-gen]
  (gen/sized
    (fn [size]
      (seeded
        (fn [seed]
          (let [options {:seed seed :size size}]
            (->fn-gen options)))))))

(defn fn-gen [fn-return]
  (fn-gen*
    (fn [options]
      (gen/return
        (fn [& args]
          (fn-return args options))))))

(defn pure-fn-gen [output]
  (fn-gen
    (fn [args {:keys [size seed] :as options}]
      (gen/generate output size (summarize-value args options)))))

(defn impure-fn-gen [output]
  (fn-gen*
    (fn [{:keys [size seed] :as options}]
      (gen/return
        (let [a (atom (gen/lazy-random-states (-random seed)))]
          (fn [& args]
            (gen-root options
                      (seeded
                        (fn [seed]
                          (let [options (assoc options :seed seed)
                                seed (summarize-value args options)]
                            (gen/generate output size seed))))
                      (ffirst (swap-vals! a rest))
                      size)))))))

(comment
  (= [#{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M} #{\M}]
     (mapv (gen/generate (pure-fn-gen gen/any-printable) 3 1)
           (repeat 10 45644666)))

  (= [#{} #uuid "709ca02d-8f95-4c0e-a3e0-279373fa23ef" #{} #{} [] (\{) #{} () #{} ()]
     (mapv ((gen/generate (impure-fn-gen gen/any-printable) 3 1) 1)
           (repeat 10 45644666)))
  ((gen/generate (impure-fn-gen gen/any) 4 1)
   45644666)
  )
