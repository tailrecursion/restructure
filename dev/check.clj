(ns check
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]))

(defn- run-command!
  [cmd]
  (let [{:keys [exit out err]} (apply sh/sh cmd)
        output (str/trim (str out err))]
    (when (seq output)
      (println output))
    (when-not (zero? exit)
      (throw (ex-info "Command failed" {:cmd cmd, :exit exit})))
    exit))

(defn -main
  [& _]
  (run-command! ["clj" "-M:fmt"])
  (run-command! ["clj" "-M:test"])
  (run-command! ["clj" "-M:lint" "--lint" "src" "test"])
  (shutdown-agents))
