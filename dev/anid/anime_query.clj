#!/usr/bin/env bb

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def anime-file "anime-filtered.csv")
(def ratings-file "user-filtered.csv")
(def max-anime-per-user 10)

(defn csv-lazy-seq
  "Create a lazy sequence of CSV rows as maps, keeping file open"
  [filename]
  (let [reader (io/reader filename)
        csv-data (csv/read-csv reader)
        headers (map keyword (first csv-data))
        rows (rest csv-data)]
    (map #(zipmap headers %) rows)))

(defn find-anime-by-ids
  "Lazily find anime by IDs, stopping when all are found"
  [filename anime-ids]
  (let [id-set (set (map str anime-ids))
        found (atom {})]
    (with-open [reader (io/reader filename)]
      (let [csv-data (csv/read-csv reader)
            headers (map keyword (first csv-data))
            rows (rest csv-data)]
        (doseq [row rows
                :while (< (count @found) (count id-set))]
          (let [anime (zipmap headers row)
                id (:anime_id anime)]
            (when (id-set id)
              (swap! found assoc id anime))))
        @found))))

(defn find-user-ratings
  "Lazily find user ratings, stopping after max-count"
  [filename user-id max-count]
  (with-open [reader (io/reader filename)]
    (let [csv-data (csv/read-csv reader)
          headers (map keyword (first csv-data))
          rows (rest csv-data)
          user-id-str (str user-id)]
      (doall
        (take max-count
          (keep (fn [row]
                  (let [rating (zipmap headers row)]
                    (when (= (:user_id rating) user-id-str)
                      rating)))
                rows))))))

(defn truncate-text
  "Truncate text to max-length characters, adding ellipsis if needed"
  [text max-length]
  (if (> (count text) max-length)
    (str (subs text 0 max-length) "...")
    text))

(defn print-anime-info
  "Print information for a list of anime IDs"
  ([anime-map anime-ids] (print-anime-info anime-map anime-ids
                                           (repeat (count anime-ids) {})))
  ([anime-map anime-ids addtl-info-seq]
   (run!
     (fn [[id add-info]]
       (if-let [anime (get anime-map (str id))]
         (do
           (println "    " (:Name anime) "[" id "]")
           (println "Genres:" (:Genres anime))
           (println "Synopsis:" (truncate-text (:sypnopsis anime) 200))
           (when (seq add-info) (println "Info:" add-info)))
         (println  "Anime ID " id " not found in database")))
     (map vector
          anime-ids
          addtl-info-seq))))

(defn print-user-ratings
  "Print ratings for a specific user"
  [ratings user-id]
  (if (empty? ratings)
    (println  "No ratings found for user" user-id)
    (do
      (println "=== Anime for User" user-id "- showing up to"
                    max-anime-per-user "===")
      (doseq [rating ratings]
        (println "Anime ID:" (:anime_id rating)
                     "| Rating:" (:rating rating)))
      (println "Showing" (count ratings) "interaction(s)"))))

(defn print-user-anime-info
  "Print anime descriptions and genres from the user ratings"
  [ratings user-id]
  (if (empty? ratings)
    (println "No ratings found for user" user-id)
    (do
      (println "=== Anime for User" user-id "- showing up to"
                    max-anime-per-user "===")
      (let [anime-ids (map :anime_id ratings)]
        (print-anime-info (find-anime-by-ids anime-file anime-ids)
                          anime-ids (map #(select-keys % [:rating]) ratings)))
      (println "Showing" (count ratings) "interaction(s)"))))

(defn -main [& args]
  (cond
    ;; Query anime by IDs
    (= (first args) "anime")
    (let [anime-ids (map #(Integer/parseInt %) (rest args))
          anime-map (find-anime-by-ids anime-file anime-ids)]
      (println "=== Requested anime IDs" anime-ids "===")
      (print-anime-info anime-map anime-ids))

    ;; Query anime by IDs from a file
    (= (first args) "anime-file")
    (let [anime-ids (str/split (slurp (second args)) #"\s+"),
          anime-map (find-anime-by-ids anime-file anime-ids)]
      (println "=== Requested anime IDs" anime-ids "===")
      (print-anime-info anime-map anime-ids))

    ;; Query user ratings - show descriptions
    (= (first args) "user")
    (let [user-id (second args)
          ratings (find-user-ratings ratings-file user-id max-anime-per-user)]
      (print-user-anime-info ratings user-id))

    ;; Help message
    :else
    (do
      (println "Anime Database Query Tool")
      (println "Usage:")
      (println "  bb anime_query.clj anime <anime_id1> <anime_id2> ...")
      (println "    - Show information for specified anime IDs")
      (println "  bb anime_query.clj anime-file <anime_txt> ...")
      (println "    - Show information for specified anime IDs, read from the file")
      (println "      containing IDs separated by whitespace")
      (println "  bb anime_query.clj user <user_id>")
      (println "    - Show up to" max-anime-per-user "ratings for specified user")
      (println "Examples:")
      (println "  bb anime_query.clj anime 1 67 242")
      (println "  bb anime_query.clj user-anime-descs 0"))))

(apply -main *command-line-args*)
