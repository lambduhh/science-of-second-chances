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
  (letfn [(fields-present? [row] (some nil? (vals row)))
          (job-position [{:keys [position]}] (or (= position 1)
                                                 (= position 2)))]
    (->> all-hired-candidates
         (remove fields-present?)
         (filter job-position))))

(defn criminal-history? [valid-hired-candidates]
  (letfn [(criminal-history? [{:keys [criminal-history]}]
            (= criminal-history 1))]
    {:second-chance-candidates
     (->> valid-hired-candidates
          (filter criminal-history?))

     :no-criminal-history
     (->> valid-hired-candidates
          (remove criminal-history?))}))

(defn average
  [numbers]
  (if (empty? numbers) 0
      (float (/ (reduce + numbers) (count numbers)))))

(defn length-of-employment [candidate-pool]
  (map :LOE candidate-pool))

(defn compare-LOE [valid-hired-candidates]
  (let [average-LOE
        (->> (length-of-employment valid-hired-candidates)
             (average))

        criminal-histories
        (criminal-history? valid-hired-candidates)

        no-criminal-history-LOE
        (->> (:no-criminal-history criminal-histories)
             (length-of-employment)
             (average))

        second-chance-LOE
        (->> (:second-chance-candidates criminal-histories)
             (length-of-employment)
             (average))]
    {:average-LOE               average-LOE
     :LOE-second-chance         second-chance-LOE
     :LOE-no-criminal-history   no-criminal-history-LOE
     :longest-tenure-on-average (if (< second-chance-LOE no-criminal-history-LOE)
                                  "no criminal history"
                                  "second-chance-candidates")
     :difference-in-tenure      (if (< second-chance-LOE no-criminal-history-LOE)
                                  (str (- no-criminal-history-LOE second-chance-LOE) " days")
                                  (str (- second-chance-LOE no-criminal-history-LOE) " days"))}))


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

  (defn columnizer [column-index] ; this was just some fun I was having with sorting the data by columns
    (let [column
          (mapv #(nth % column-index) data)

          attribute
          (keyword (first column))

          values
          (mapv str->int (rest column))]
      (assoc {} attribute values)))

  (def xf (map columnizer))
  (def column-by-attribute
    (transduce xf conj (range 10))))
