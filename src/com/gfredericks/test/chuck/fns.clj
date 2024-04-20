(ns com.gfredericks.test.chuck.fns
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]))

(defn ^:private randomized
  "Like sized, but passes an rng instead of a size."
  [func]
  (#'gen/make-gen (fn [rng size]
                    (let [[r1 r2] (random/split rng)]
                      (gen/call-gen
                        (func r1)
                        r2
                        size)))))

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

;; size == how much 2^64 span we generate in?
;; or
;; size == number of conditional returns other than the default one in the generated fn?
;; or both?
(defn summarize-value [x {:keys [rng] :as options}]
  (letfn [(summarize-ident [x]
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
    (unchecked-add (unknown x) (random/rand-long rng))))

(defn fn-gen* [->fn-gen]
  (gen/sized
    (fn [size]
      (randomized
        (fn [rng]
          (let [options {:rng rng :size size}]
            (->fn-gen options)))))))

(defn fn-gen [fn-return]
  (fn-gen*
    (fn [options]
      (gen/return
        (fn [& args]
          (fn-return args options))))))

(defn pure-fn-gen [output]
  (fn-gen
    (fn [args {:keys [size] :as options}]
      (gen/generate output size (summarize-value args options)))))

(defn impure-fn-gen [output]
  (fn-gen*
    (fn [{:keys [size rng] :as options}]
      (let [a (atom (gen/lazy-random-states rng))]
        (fn [& args]
          (gen-root options
                    (randomized
                      (fn [rng]
                        (let [options (assoc options :rng rng)
                              seed (summarize-value args options)]
                          (gen/return
                            (gen/generate output size seed)))))
                    (ffirst (swap-vals! a rest))
                    size))))))

(defn instrument [a path v]
  (if (fn? v)
    (fn [& args]
      (let [r (apply v args)]
        (swap! a update-in (conj path ))
        r))
    v))

(comment

  )
