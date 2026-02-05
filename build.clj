(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.github.tailrecursion/restructure)
(def version "1.0.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm {:url "https://github.com/tailrecursion/restructure"}
                :pom-data [[:licenses
                            [:license
                             [:name "MIT"]
                             [:url "https://opensource.org/licenses/MIT"]]]]
                :description "Rewrite nested Clojure data with a declared shape."})
  ;; deps-deploy 0.0.12 expects a root pom.xml
  (b/copy-file {:src (b/pom-path {:class-dir class-dir
                                  :lib lib
                                  :version version})
                :target "pom.xml"})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  {:jar jar-file})

(defn deploy [_]
  (let [{:keys [jar]} (jar nil)]
    (dd/deploy {:installer :clojars
                :artifact-map (dd/all-artifacts false version jar)
                :coordinates [lib version]})))
