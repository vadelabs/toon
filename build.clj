(ns build
  (:refer-clojure :exclude [test])
  (:require
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as dd]))


(def lib 'com.vadelabs/toon)


(defn- get-git-tag-version
  "Get version from current git tag if it exists"
  []
  (try
    (let [proc (.exec (Runtime/getRuntime) (into-array String ["git" "describe" "--exact-match" "--tags"]))
          exit-code (.waitFor proc)]
      (when (zero? exit-code)
        (let [output (clojure.string/trim (slurp (.getInputStream proc)))]
          (when (and (not (empty? output))
                     (clojure.string/starts-with? output "v"))
            (subs output 1))))) ; Strip leading 'v'
    (catch Exception _
      nil)))


(defn- date-commit-count-version
  []
  (let [date (.format (java.time.LocalDate/now)
                      (java.time.format.DateTimeFormatter/ofPattern "yyyy.MM.dd"))
        today-midnight (str date "T00:00:00")]
    (try
      (let [proc (.exec (Runtime/getRuntime) (into-array String ["git" "log" "--since" today-midnight "--oneline"]))
            _ (.waitFor proc)
            output (slurp (.getInputStream proc))
            commit-count (if (empty? output) 0 (count (clojure.string/split-lines output)))]
        (format "%s-%d" date commit-count))
      (catch Exception _
        ;; Fallback if git is not available
        (format "%s-0" date)))))


(def version (or (get-git-tag-version) (date-commit-count-version)))
(def class-dir "target/classes")


(defn- pom-template
  [version]
  [[:description "Clojure/ClojureScript implementation of TOON v1.3 (Token-Oriented Object Notation) - A compact data format optimized for LLMs, achieving 49% fewer tokens than formatted JSON while maintaining readability and structure"]
   [:url "https://github.com/vadelabs/toon"]
   [:licenses
    [:license
     [:name "MIT License"]
     [:url "https://opensource.org/licenses/MIT"]]]
   [:developers
    [:developer
     [:name "Pragyan"]
     [:email "pragyan@vadelabs.com"]]]
   [:scm
    [:url "https://github.com/vadelabs/toon"]
    [:connection "scm:git:https://github.com/vadelabs/toon.git"]
    [:developerConnection "scm:git:ssh:git@github.com:vadelabs/toon.git"]
    [:tag (str "v" version)]]])


(defn- jar-opts
  [opts]
  (assoc opts
         :lib lib   :version version
         :jar-file  (format "target/%s-%s.jar" lib version)
         :basis     (b/create-basis {})
         :class-dir class-dir
         :target    "target"
         :src-dirs  ["src"]
         :pom-data  (pom-template version)))


(defn jar
  "Build the JAR."
  [opts]
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println "\nBuilding JAR..." (:jar-file opts))
    (b/jar opts))
  opts)


(defn install
  "Install the JAR locally."
  [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)


(defn deploy
  "Deploy the JAR to Clojars."
  [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
