(ns com.gfredericks.test.chuck.recursive
  (:refer-clojure :exclude [double for partition prn])
  #?(:cljs (:require-macros [com.gfredericks.test.chuck.generators :refer [for]]))
  (:require [clojure.test.check.generators :as gen]
            [#?(:clj clojure.core :cljs cljs.core) :as core]))

(let [out *out*]
  (defn- prn [& args]
    (binding [*out* out]
      (apply clojure.core/prn args))))

(defn combine-mutual-gens
  "Combine the (direct) result of mutual-gens into a single generator using a disjunction."
  [mgs-result]
  {:post [(gen/generator? %)]}
  (assert (map? mgs-result) (class mgs-result))
  (let [comb (fn comb [r]
               (cond
                 (gen/generator? r) r
                 (map? r) (gen/frequency
                            (->> (vals r)
                                 (sort-by
                                   (fn [v]
                                     {:post [(number? %)]}
                                     (or (-> v meta ::shrink-order) 0)))
                                 (into []
                                       (mapcat (fn _mapcat [v]
                                                 (let [freq (or (-> v meta ::frequency) 1)]
                                                   (assert (number? freq)
                                                           (str "Invalid ::frequency: " (pr-str freq)))
                                                   ;; TODO think about when we might want to remove scalar-gens here (eg., shrinking)
                                                   (when (pos? freq)
                                                     (let [g (comb v)]
                                                       (assert (gen/generator? g) (pr-str g))
                                                       [[freq g]]))))))))
                 :else (AssertionError. (str "unexpected input to combine-mutual-gens " (class r) " " (pr-str r)
                                             " " (class mgs-result) " " (pr-str mgs-result)))))]
    (comb mgs-result)))

(comment
  (gen/sample
    (mutual-gen
      {:ping (fn _ping [gen-rec]
               (gen/tuple (gen/return :ping)
                          (gen-rec [:pong])))
       :pong (fn _pong [gen-rec]
               (gen/tuple (gen/return :pong)
                          (gen-rec [:ping])))
       :nil (gen/return nil)}))

  (gen/sample
    (mutual-gen
      (gen/return nil)))
  (gen/sample
    (mutual-gen
      {:Ping {:ping (fn [gen-at]
                      (gen/tuple (gen/return :ping)
                                 (gen-at [:Ping])))
              :nil (gen/return nil)}}))
  (gen/sample
    (mutual-gen
      {:ping (fn [gen-for]
               (gen/tuple (gen/return :ping)
                          (gen-for [:ping])))
       :nil (gen/return nil)}))
  (gen/sample
    (mutual-gen
      {:ping (fn [gen-for]
               (gen/tuple (gen/return :ping)))
       :nil (gen/return nil)}))
  ((requiring-resolve 'clojure.repl/pst) 1000)
  (gen/sample
    (mutual-gen
      {:nil (gen/return nil)}))
  )

(defn mutual-gens
  "Create a map of mutually-recursive generators.

  scalar-gen is a generator for leaf values.

  container-gen-fns is a map from identifiers to functions. Each function
  takes a map from these identifiers to their generators and should return
  a generator.

  Returns a map of identifiers (from container-gen-fns) to their generators.

  Combine with gen/one-of to combine into a single generator:
  (gen/one-of (vec (vals (mutual-gens container-gen-fns scalar-gen))))"
  [root-desc]
  ;(prn "root-desc" root-desc)
  (let [rec (fn rec [path desc gs]
              {:pre [(vector? path)
                     (map? gs)
                     (every? gen/generator? (vals gs))]}
              ;(prn "rec desc" [path desc gs])
              (let [rdesc 
                    (cond
                      (gen/generator? desc) desc

                      (map? desc)
                      (let [_ (assert (seq desc) "map desc must be non-empty")
                            [scalar-gens non-scalar-gens] (mapv #(into {} (% (comp gen/generator? val)) desc)
                                                                [filter remove])
                            scalar-gen (delay (gen/one-of (vec (vals scalar-gens))))]
                        (cond-> scalar-gens 
                          (seq non-scalar-gens)
                          (into (map (fn [[inner-k desc]]
                                       (let [path (conj path inner-k)
                                             rdesc
                                             (cond
                                               (gen/generator? desc) desc
                                               (map? desc) (rec path desc gs)

                                               (ifn? desc)
                                               (let [container-gen-fn desc]
                                                 (gen/recursive-gen
                                                   (fn [rec-generator]
                                                     (let [gs (assoc gs path rec-generator)
                                                           gen-rec (fn [p]
                                                                     {:post [(gen/generator? %)]}
                                                                     (assert (seq p))
                                                                     (assert (vector? p))
                                                                     ;(prn "gen-rec p" [(vec (keys gs)) p])
                                                                     (or
                                                                       (get gs p)
                                                                       (let [root-desc
                                                                             (reduce
                                                                               (fn [desc [path g]]
                                                                                 ;; TODO at this point, container generators
                                                                                 ;; become Generator's (instead of ifn's).
                                                                                 ;; we should add tags
                                                                                 ;; to distinguish scalar/container generators
                                                                                 ;; so redundant scalar generators can be removed.
                                                                                 ;; eg., {:Ping {:ping
                                                                                 ;;              (fn [gen-at]
                                                                                 ;;                (gen/tuple (gen/return :ping)
                                                                                 ;;                           ;; TODO should not contain :nil gen directly
                                                                                 ;;                           (gen-at [:Ping])))
                                                                                 ;;              :nil (gen/return nil)}}
                                                                                 (assoc-in desc path g))
                                                                               root-desc
                                                                               gs)
                                                                             gr (get-in root-desc p)
                                                                             _ (assert gr
                                                                                       (str "No path " p " in desc " root-desc))
                                                                             ;_ (prn "gen-rec rdesc" p gr gs)
                                                                             rdesc (cond
                                                                                     (gen/generator? gr) gr
                                                                                     (map? gr) (rec p gr gs)
                                                                                     :else (get-in (rec [] root-desc gs) p))]
                                                                         (assert ((some-fn map? gen/generator?) rdesc) (pr-str rdesc))
                                                                         (combine-mutual-gens
                                                                           rdesc))))]
                                                       (container-gen-fn gen-rec)))
                                                   @scalar-gen))
                                               :else (throw (AssertionError. (str "bad inner desc " (pr-str (class desc))))))]
                                         [inner-k (with-meta rdesc (meta desc))])))
                                non-scalar-gens)))

                      :else (throw (AssertionError. (str "bad outer desc " (pr-str (class desc))))))]
                (with-meta rdesc (meta desc))))
        rdesc (rec [] root-desc {})]
    rdesc))


(defn mutual-gen
  ""
  [desc]
  (-> desc
      mutual-gens 
      combine-mutual-gens))

(defn tagged-recursive-gen [tag tag->gen container-gen-fn scalar-gen]
  (assert (some? tag))
  (or (tag->gen tag)
      (with-meta
        (gen/recursive-gen
          (fn [rec]
            (let [tag->gen (assoc tag->gen tag rec)]
              (container-gen-fn
                (fn [next-gen]
                  (assert (gen/generator? next-gen))
                  (assert (-> next-gen meta ::tagged-recursive-gen)
                          (meta next-gen))
                  (let [[scalar-gen container-gen-fn] ((::->scalar+container-gen-fn (meta next-gen)))]
                    (tagged-recursive-gen
                      (::tag (meta next-gen))
                      tag->gen
                      container-gen-fn
                      scalar-gen))))))
          scalar-gen)
        {::tagged-recursive-gen true
         ::tag tag
         ::->scalar+container-gen-fn (fn [] [scalar-gen container-gen-fn])})))

(defmacro defrecursive-gen [name container scalar]
  (let [kw (keyword (-> *ns* ns-name str) (str name))]
    `(def ~name
       (tagged-recursive-gen ~kw
                             {}
                             ~container
                             ~scalar))))

(defn recursive-cases [tag]
  (vary-meta
    (tagged-recursive-gen
      tag
      {}
      (fn [gen-for]
        (throw (Exception. "Empty recursive-cases")))
      (gen/bind (gen/return nil)
                (fn [_]
                  (throw (Exception. "Empty recursive-cases")))))
    assoc
    ::recursive-cases true
    ::cases {}))

(defn add-recursive-case [rc id container-fn-or-scalar]
  (assert (::recursive-cases (meta rc))
          (pr-str (meta rc)))
  (let [cases (assoc (::cases (meta rc)) id container-fn-or-scalar)
        [scalar-gens container-gen-fns] (mapv #(into {} (% (comp gen/generator? val)) cases)
                                              [filter remove])]
    (vary-meta
      (let [scalar-gen (delay (gen/one-of (vec (vals scalar-gens))))] 
        (tagged-recursive-gen
          (::tag (meta rc))
          {}
          (fn [gen-for]
            (gen/one-of (mapv #(% gen-for) (vals container-gen-fns))))
          (gen/bind (gen/return nil)
                    (fn [_] @scalar-gen))))
      assoc
      ::recursive-cases true
      ::cases cases)))

(defmacro defrecursive-cases [name]
  (let [id (keyword (-> *ns* ns-name str) (str name))]
    `(def #_defonce ~name
       (let [a# (atom (recursive-cases ~id)
                      :validator #(-> % meta ::recursive-cases))]
         (with-meta
           (gen/bind (gen/return nil)
                     (fn [_#] @a#))
           {::tagged-recursive-gen true
            ::tag ~id
            ::->scalar+container-gen-fn (fn []
                                          ((::->scalar+container-gen-fn @a#)))
            ::atom a#})))))

(defmacro defrecursive-case [rc id container-fn-or-scalar]
  `(let [rc# ~rc]
     (swap! (::atom (meta rc#)) add-recursive-case ~id ~container-fn-or-scalar)
     rc#))
