(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'gmrs)
(def version "0.0.1")
(def class-dir "target/classes")
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/compile-clj {:basis @basis
                  :ns-compile '[gmrs.command]
                  :class-dir class-dir
                  ; for Spark 3.*, otherwise can delete
                  :java-opts ["--add-exports=java.base/sun.nio.ch=ALL-UNNAMED"]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'gmrs.command.main}))
