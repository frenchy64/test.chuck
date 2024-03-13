(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.clojure-test]
            [com.gfredericks.test.chuck.properties :as prop]
            [clojure.test :as ct :refer [is testing]])
  #?(:cljs
     (:require-macros [com.gfredericks.test.chuck.clojure-test])))

;; exists in clojure.test.check.clojure-test v0.9.0
(defn with-test-out* [f]
  #?(:clj  (ct/with-test-out (f))
     :cljs (f)))

(defn ^:private not-exception?
  [value]
  (not (instance? #?(:clj  Throwable
                     :cljs js/Error)
                  value)))

(defn shrunk-report [m]
  (merge (dissoc m :result) {:type ::shrunk}))

(defmethod ct/report #?(:clj ::shrunk :cljs [::ct/default ::shrunk]) [m]
  (newline)
  (println (str "Tests failed, smallest case: " (pr-str (-> m :shrunk :smallest))
                "\nSeed: " (:seed m))))

(defn report-exception-or-shrunk [result]
  (if (:result result)
    (is (not-exception? (:result result)) (pr-str result))
    (with-test-out*
      (fn [] (ct/report (shrunk-report result))))))

(defn pass? [reports]
  (every? #(= (:type %) :pass) reports))

(defn report-needed? [reports final-reports]
  (or (not (pass? reports)) (empty? final-reports)))

(defn save-to-final-reports [final-reports reports]
  (if (report-needed? reports final-reports)
    reports
    final-reports))

(def ^:dynamic *chuck-captured-reports*)

#?(:cljs
(defmethod ct/report [::chuck-capture :error]
  [m]
  (swap! *chuck-captured-reports* conj (assoc m ::testing-contexts (:testing-contexts (ct/get-current-env))))))

#?(:cljs
(defmethod ct/report [::chuck-capture :fail]
  [m]
  (swap! *chuck-captured-reports* conj (assoc m ::testing-contexts (:testing-contexts (ct/get-current-env))))))

#?(:cljs
(defmethod ct/report [::chuck-capture :pass]
  [m]
  (swap! *chuck-captured-reports* conj (assoc m ::testing-contexts (:testing-contexts (ct/get-current-env))))))

(defn capture-reports*
  [reports-atom f]
  #?(:clj
     (binding [ct/report #(swap! reports-atom conj (assoc % ::testing-contexts ct/*testing-contexts*))]
       (f))

     :cljs
     (binding [*chuck-captured-reports* reports-atom
               ct/*current-env* (assoc (ct/empty-env ::chuck-capture)
                                       :testing-contexts (:testing-contexts (ct/get-current-env)))]
       (f))))

(defmacro capture-reports
  [& body]
  `(let [reports# (atom [])]
     (capture-reports* reports# (fn [] (do ~@body)))
     @reports#))

(defn times [num-tests-or-options]
  (cond (map? num-tests-or-options)     (:num-tests num-tests-or-options tc.clojure-test/*default-test-count*)
        (integer? num-tests-or-options) num-tests-or-options))

(defn get-current-time-millis
  "Internal"
  []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.valueOf (js/Date.))))

(defn options [num-tests-or-options]
  (-> (cond (map? num-tests-or-options)     (dissoc num-tests-or-options :num-tests)
            (integer? num-tests-or-options) {})
      (update :seed #(or % (get-current-time-millis)))))

(defn counting-reporter-fn [f atm]
  (fn [{:keys [num-tests] :as r}]
    (when (integer? num-tests)
      (swap! atm max num-tests))
    (when f
      (f r))))

(defn -exception-thrown-report
  "Internal"
  [property e trial-number seed start-time reporter-fn]
  (let [failed-after-ms (- (get-current-time-millis) start-time)]
    {:type :failure
     :failed-after-ms failed-after-ms
     :num-tests trial-number
     :pass? false
     :property property
     :result e
     :result-data {:clojure.test.check.properties/error e}
     :seed seed}))

(defn -report-generator-exception
  "Internal"
  [{:keys [seed current-iteration]}]
  (with-test-out*
    #(println (str "\n\nError thrown by test during iteration " current-iteration
                   "\nSeed: " seed))))

(defmacro qc-and-report-exception
  [final-reports num-tests-or-options bindings & body]
  `(report-exception-or-shrunk
     (let [num-tests-or-options# ~num-tests-or-options
           final-reports# ~final-reports
           so-far-atm# (atom 0)
           options-map# (-> (options num-tests-or-options#)
                            (update :reporter-fn counting-reporter-fn so-far-atm#))
           with-captured-reports# (fn [f#]
                                    (let [reports# (capture-reports (f#))]
                                      (swap! final-reports# save-to-final-reports reports#)
                                      reports#))
           property# (prop/for-all ~bindings
                       (pass? (with-captured-reports# (fn [] (do ~@body)))))
           start-time# (get-current-time-millis)]
       (try (apply tc/quick-check
             (times num-tests-or-options#)
             property#
             (apply concat options-map#))
            (catch #?(:clj  Throwable
                      :cljs :default) e#
              (let [reporter-fn# (:reporter-fn options-map#)
                    r# (-exception-thrown-report
                         property# e# (inc @so-far-atm#) (:seed options-map#)
                         start-time# reporter-fn#)]
                (with-captured-reports#
                  #(reporter-fn# r#))
                r#))))))

(defn -testing
  [name func]
  (testing name (func)))

(defn -report
  [report]
  #?(:clj
     (binding [ct/*testing-contexts* (::testing-contexts report)]
       (ct/report report))
     :cljs
     (let [old-env (ct/get-current-env)]
       (ct/set-env! (assoc old-env :testing-contexts (::testing-contexts report)))
       (try (ct/report report)
            (finally
              (ct/set-env! (assoc (ct/get-current-env) :testing-contexts (:testing-contexts old-env))))))))

(defmacro checking
  "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make

    (testing \"doubling\"
      (is (= (* 2 2) (+ 2 2))))

  generative, you simply have to change it to

    (checking \"doubling\"
      [x gen/int]
      (is (= (* 2 x) (+ x x)))).

  Test failures will be reported for the smallest case only.

  Bindings and body are passed to com.gfredericks.test.chuck.properties/for-all.

  Options can be provided to `clojure.test.check/quick-check` as an optional
  second argument, and should either evaluate to an integer (number of tests) or a
  map (with :num-tests specifying the number of tests). e.g.:

    (let [options {:num-tests 100 :seed 123 :max-size 10}]
      (checking \"doubling\" options
        [x gen/int]
        (is (= (* 2 x) (+ x x)))))

  For background, see
  http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
  {:forms '[(checking name num-tests-or-options? [bindings*] body*)]}
  [name & opt+body]
  (let [[num-tests-or-options opt+body] (if (vector? (first opt+body))
                                          [{} opt+body]
                                          ((juxt first next) opt+body))
        [bindings & body] opt+body]
    `(let [final-reports# (atom [])]
       (-testing ~name
                 (fn []
                   (qc-and-report-exception final-reports# ~num-tests-or-options ~bindings ~@body)))
       (doseq [r# @final-reports#]
         (-report r#)))))

(defmacro for-all
  "An alternative to com.gfredericks.test.chuck.properties/for-all that uses
  clojure.test-style assertions (i.e., clojure.test/is) rather than
  the truthiness of the body expression.

  See `checking` to additionally report clojure.test assertion failures."
  [bindings & body]
  `(prop/for-all
    ~bindings
    (let [reports# (capture-reports ~@body)]
      (pass? reports#))))
