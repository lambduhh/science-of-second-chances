(ns sosc.csv-data
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn str->int [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(def data
  (with-open [reader (io/reader "/sosc/resources/data.csv")]
    (doall
     (csv/read-csv reader))))

(defn csv-data->maps [data]
  (map zipmap
       (->> (first data)
            (map keyword)
            repeat)
       (map (fn [m] (mapv str->int m)) (rest data))))

(def all-hired-candidates
  (csv-data->maps data))


(defn valid-candidates [all-hired-candidates]
  (letfn [(fields-present? [row] (some nil? (vals row))) ;; Check for any missing fields
          (job-position [{:keys [position]}] (or (= position 1)
                                                 (= position 2)))]
    (->> all-hired-candidates
         (remove fields-present?)  ;; Remove rows with missing data
         (filter job-position))))  ;; Only count sales and customer service positions


(def valid-hired-candidates (valid-candidates all-hired-candidates))  ; all valid hired candidates


(comment
  (count all-hired-candidates)
  (count valid-hired-candidates)

  (defn columnizer [column-index]
    (let [column
          (mapv #(nth % column-index) data)

          attribute
          (keyword (first column))

          values
          (mapv str->int (rest column))]
      (assoc {} attribute values)))

  (def xf (map columnizer))
  (def column-by-attribute
    (transduce xf conj (range 10))) ; this is a list of maps, each map has a key that corresponds to an attribute and a value that is a list of the values
)


(defn criminal-history? [valid-hired-candidates]
  (letfn [(criminal-history? [{:keys [criminal-history]}]
            (= criminal-history 1))]
    {:second-chance-candidates
     (->> valid-hired-candidates
          (filter criminal-history?))

     :no-criminal-history
     (->> valid-hired-candidates
          (remove criminal-history?))}))

(comment
  (def criminal-histories (criminal-history? valid-hired-candidates))
  => {:second-chance-candidates
      ({:ID 1, :longest-job 2, :school 1, :fewest-short-jobs -2, :voluntary-t 0, :LOE 1236, :terminated 0, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 0} {:ID 4, :longest-job 3, :school 0, :fewest-short-jobs -5, :voluntary-t 0, :LOE 1000, :terminated 1, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 1} {:ID 14, :longest-job 1, :school 1, :fewest-short-jobs -3, :voluntary-t 0, :LOE 1036, :terminated 0, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0} {:ID 19, :longest-job 2, :school 1, :fewest-short-jobs -4, :voluntary-t 0, :LOE 1236, :terminated 0, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0}
       {:ID 24365, :longest-job 2, :school 1, :fewest-short-jobs 1, :voluntary-t 1, :LOE 653, :terminated 1, :misconduct 1, :position 1, :criminal-history 1, :involuntary-t 0}
       {:ID 26242, :longest-job 1, :school 1, :fewest-short-jobs -5, :voluntary-t 1, :LOE 735, :terminated 1, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0})

      :no-criminal-history
      ({:ID 13, :longest-job 1, :school 0, :fewest-short-jobs -2, :voluntary-t 1, :LOE 89, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 0}
       {:ID 16, :longest-job 3, :school 1, :fewest-short-jobs -4, :voluntary-t 0, :LOE 1936, :terminated 0, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 0}
       {:ID 4325, :longest-job 4, :school 1, :fewest-short-jobs -2, :voluntary-t 0, :LOE 234, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 1} {:ID 6345, :longest-job 4, :school 1, :fewest-short-jobs -3, :voluntary-t 0, :LOE 543, :terminated 1, :misconduct 1, :position 1, :criminal-history 0, :involuntary-t 1} {:ID 526, :longest-job 6, :school 1, :fewest-short-jobs -1, :voluntary-t 0, :LOE 253, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 1})})

(defn average
  [numbers]
  (if (empty? numbers) 0
    (float (/ (reduce + numbers) (count numbers)))))

(defn length-of-employment [candidate-pool]
  (map (fn [{:keys [LOE]}] LOE) candidate-pool))

(defn compare-LOE [valid-hired-candidates]
  (let [criminal-histories
        (criminal-history? valid-hired-candidates)

       no-criminal-history-LOE
       [:all (average (length-of-employment (:no-criminal-history criminal-histories)))]

       second-chance-candidate-LOE
       [:second-chance (average (length-of-employment (:second-chance-candidates criminal-histories)))]]
    (max-key second no-criminal-history-LOE second-chance-candidate-LOE)))

(comment
   (compare-LOE valid-hired-candidates))

(defn quit-vs-fired [candidate-pool]
  (letfn [(terminated? [{:keys [terminated]}]
            (= terminated 1))

          (fired? [{:keys [involuntary-t]}]
            (= involuntary-t 1))

          (quit? [{:keys [voluntary-t]}]
            (= voluntary-t 1))

          (misconduct? [{:keys [misconduct]}]
            (= misconduct 1))]
    (let [terminated-employees
          (filter terminated? candidate-pool)]

      {:total-employees-terminated
       (count terminated-employees)

       :total-employees-who-quit
       (->> terminated-employees
            (filter quit?)
            count)

       :total-fired-employees
       (->> terminated-employees
            (filter fired?)
            count)

       :fired-for-misconduct
       (->> terminated-employees
            (filter misconduct?)
            count)})))


(comment
  (quit-vs-fired hired-candidates-with-CH)
  (quit-vs-fired valid-hired-candidates))
