(defproject com.gfredericks/test.chuck "0.2.14-SNAPSHOT"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3" :scope "provided"]
                 [org.clojure/clojurescript "1.10.879" :scope "provided"]
                 [org.clojure/test.check "1.1.0"]
                 [clj-time "0.15.2"]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [instaparse "1.4.10"]
                 [io.github.frenchy64/fully-satisfies "1.7.1-SNAPSHOT"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:ci {:jvm-opts ["-Xmx1g" "-server"]}}
  :plugins [[lein-cljsbuild "1.1.8"]
            [lein-doo "0.1.11"]
            [com.gfredericks/lein-all-my-files-should-end-with-exactly-one-newline-character "0.1.0"]]

  :cljsbuild
  {:builds
   [{:id "node-test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/tests.js"
                :output-dir "target/node"
                :main com.gfredericks.test.chuck.runner
                :optimizations :none
                :hashbang false
                :target :nodejs}}
    {:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/tests.js"
                :main com.gfredericks.test.chuck.runner
                :optimizations :none}}]}

  :aliases {"test-all"
            ^{:doc "Runs tests on multiple JVMs; profiles java-8
                    and java-11 should be defined outside this project."}
            ["do"
             "clean,"
             "with-profile" "+java-8" "test,"
             "clean,"
             "with-profile" "+java-11" "test"]})
