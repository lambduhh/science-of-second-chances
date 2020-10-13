(ns sosc.csv-data
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def data
  (with-open [reader (io/reader "/Users/ljmiller/Desktop/Projects/personal/science-of-second-chances/sosc/resources/data.csv")]
    (doall
     (csv/read-csv reader))))

(defn str->int [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

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






(defn csv-data->maps [data]
  (map zipmap
       (->> (first data)  ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
       (map (fn [m] (mapv str->int m)) (rest data))))

(def all-hired-candidates (csv-data->maps data)) ;this is a list of maps, each map is one hired person
(count all-hired-candidates)

(defn valid-candidates [all-hired-candidates] ;28 persons get passed in
  (letfn [(fields-present? [row] (some nil? (vals row))) ;check for any missing fields
          (job-position [{:keys [position]}] (or (= position 1)
                                                 (= position 2)))] ;only counts sales and customer service positions
    (->> all-hired-candidates
         (remove fields-present?)
         (filter job-position))))

(def valid-hired-candidates (valid-candidates all-hired-candidates))  ; all valid hired candidates
(count valid-hired-candidates) ;16

(defn criminal-history [all-hired-candidates-clean]
  (letfn [(criminal-history? [{:keys [criminal-history]}]
            (= criminal-history 1))]
    (->> all-hired-candidates-clean
         (filter criminal-history?))))

(def hired-candidates-with-CH (criminal-history valid-hired-candidates))
(count hired-candidates-with-CH) ; 2

(defn average
  [numbers]
  (if (empty? numbers)
    0
    (float (/ (reduce + numbers) (count numbers)))))

(defn length-of-employment [candidate-pool]
  (map (fn [{:keys [LOE]}] LOE) candidate-pool))

(def average-LOE-of-hired-candidates [:all (average (length-of-employment valid-hired-candidates))])
(def average-LOE-of-CH-candidates [:CH-only (average (length-of-employment hired-candidates-with-CH))])

(max-key second average-LOE-of-hired-candidates average-LOE-of-CH-candidates)

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

(quit-vs-fired hired-candidates-with-CH)
(quit-vs-fired valid-hired-candidates)
