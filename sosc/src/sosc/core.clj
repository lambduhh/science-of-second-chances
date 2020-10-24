(ns sosc.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn str->int [s]
  (try (Integer/parseInt s)
       (catch Exception _ nil)))

(def data
  (with-open [reader (io/reader "resources/data.csv")]
    (doall
     (csv/read-csv reader))))

(defn csv-data->maps [data]
  (map zipmap
       (->> (first data)
            (map keyword)
            repeat)
       (map (fn [m] (mapv str->int m)) (rest data))))

(def all-hired-candidates (csv-data->maps data))

(defn valid-candidates [all-hired-candidates]
  (letfn [(fields-present? [row] (some nil? (vals row)))
          (job-position [{:keys [position]}] (or (= position 1)
                                                 (= position 2)))]
    (->> all-hired-candidates
         (remove fields-present?)
         (filter job-position)))) ; data scrubber

(def valid-hired-candidates (valid-candidates all-hired-candidates))

;;;;;;;;;;;;;;
;; BAD CODE ;; -> why?
;;;;;;;;;;;;;;

(defn criminal-history? [valid-hired-candidates]
  (letfn [(criminal-history? [{:keys [criminal-history]}]
            (= criminal-history 1))]
    {:second-chance-candidates
     (->> valid-hired-candidates
          (filter criminal-history?))

     :no-criminal-history
     (->> valid-hired-candidates
          (remove criminal-history?))}))

;;;;;;;;;;;;;;
;; GOOD CODE ;;
;;;;;;;;;;;;;;

(defn separate-gen-pop [valid-hired-candidates]
  (reduce (fn [acc {:keys [criminal-history] :as person}]
            (if (= criminal-history 1)
              (update acc :second-chance-candidates conj person)
              (update acc :no-criminal-history conj person)))
          {:second-chance-candidates []
           :no-criminal-history []}
          valid-hired-candidates))


;;;;;;;;;;;;;;
;; helpers ;;
;;;;;;;;;;;;;;


(defn average
  [numbers]
  (if (empty? numbers) 0
      (float (/ (reduce + numbers) (count numbers)))))

(defn length-of-employment [candidate-pool]
  (map :LOE candidate-pool))


;;;;;;;;;;;;;;
;; MEDIO-CODE ;; -> why?
;;;;;;;;;;;;;;


(defn compare-LOE-alpha [valid-hired-candidates]
  (let [average-LOE
        (->> (length-of-employment valid-hired-candidates)
             (average))

        criminal-histories
        (separate-gen-pop valid-hired-candidates)

        no-criminal-history-LOE
        (->> (:no-criminal-history criminal-histories)
             (length-of-employment)
             (average))

        second-chance-LOE
        (->> (:second-chance-candidates criminal-histories)
             (length-of-employment)
             (average))]
    {:average-loe               average-LOE
     :second-chance-loe         second-chance-LOE
     :no-criminal-history-loe   no-criminal-history-LOE
     :longest-tenure-on-average (if (< second-chance-LOE no-criminal-history-LOE)
                                  "no criminal history"
                                  "second-chance-candidates")

     :difference-in-tenure      (if (< second-chance-LOE no-criminal-history-LOE)
                                  (str (- no-criminal-history-LOE second-chance-LOE) " days")
                                  (str (- second-chance-LOE no-criminal-history-LOE) " days"))}))

(defn compare-loe-second [valid-hired-candidates]
  (letfn [(length-of-employment [candidate-pool]
            (map :LOE candidate-pool))]
    (let [gen-pop-average-loe
          (->> (length-of-employment valid-hired-candidates)
               (average))

          separated-gen-pop
          (separate-gen-pop valid-hired-candidates)

          ave-loe
          (comp average length-of-employment)

          no-criminal-history-loe
          (ave-loe (:no-criminal-history separated-gen-pop))

          second-chance-loe
          (ave-loe (:second-chance-candidates separated-gen-pop))]
      {:average-loe               gen-pop-average-loe
       :second-chance-loe         second-chance-loe
       :no-criminal-history-loe   no-criminal-history-loe
       :longest-tenure-on-average (if (< second-chance-loe no-criminal-history-loe)
                                    "no criminal history"
                                    "second-chance-candidates")

       :difference-in-tenure      (if (< second-chance-loe no-criminal-history-loe)
                                    (str (- no-criminal-history-loe second-chance-loe) " days")
                                    (str (- second-chance-loe no-criminal-history-loe) " days"))})))


; This time passing in the value of (separate-gen-pop valid-hired-candidates)
;  instead of valid hired candidates


(defn compare-loe-third [{:keys [second-chance-candidates
                                 no-criminal-history]}]
  (letfn [(length-of-employment [candidate-pool]
            (map :LOE candidate-pool))]
    (let [ave-loe
          (comp average length-of-employment)

          gen-pop-average-loe
          (->> (concat second-chance-candidates no-criminal-history)
               ave-loe)

          no-criminal-history-loe
          (ave-loe (:no-criminal-history separated-gen-pop))

          second-chance-loe
          (ave-loe (:second-chance-candidates separated-gen-pop))]
      {:average-loe               gen-pop-average-loe
       :second-chance-loe         second-chance-loe
       :no-criminal-history-loe   no-criminal-history-loe
       :longest-tenure-on-average (if (< second-chance-loe no-criminal-history-loe)
                                    "no criminal history"
                                    "second-chance-candidates")

       :difference-in-tenure (if (< second-chance-loe no-criminal-history-loe)
                               (str (- no-criminal-history-loe second-chance-loe) " days")
                               (str (- second-chance-loe no-criminal-history-loe) " days"))})))

;;;;;;;;;;;;;;
;; GOOD CODE ;;
;;;;;;;;;;;;;;


(defn compare-loe-fourth [{:keys [second-chance-candidates
                                  no-criminal-history]}]
  (letfn [(length-of-employment [candidate-pool]
            (map :LOE candidate-pool))]
    (let [ave-loe
          (comp average length-of-employment)

          gen-pop-average-loe
          (->> (concat second-chance-candidates no-criminal-history)
               ave-loe)

          no-criminal-history-loe
          (ave-loe no-criminal-history)

          second-chance-loe
          (ave-loe second-chance-candidates)]
      {:average-loe               gen-pop-average-loe
       :second-chance-loe         second-chance-loe
       :no-criminal-history-loe   no-criminal-history-loe})))

(defn render-compare-loe [{:keys [average-loe
                                  second-chance-loe
                                  no-criminal-history-loe] :as data}]
  (let [longest-tenure-on-average
        (if (< second-chance-loe no-criminal-history-loe)
          "had no criminal history"
          "were second-chance-candidates")

        diff-in-tenure
        (if (< second-chance-loe no-criminal-history-loe)
          (str (- no-criminal-history-loe second-chance-loe) " days")
          (str (- second-chance-loe no-criminal-history-loe) " days"))]

    (str "Those that " longest-tenure-on-average " had the longest tenure on average."
         " The difference is " diff-in-tenure)))

;TO-DO refactor
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

(defn columnizer [column-index] ; this was just some fun I was having building transducers and sorting the data by columns
  (let [column
        (mapv #(nth % column-index) data)

        attribute
        (keyword (first column))

        values
        (mapv str->int (rest column))]
    (assoc {} attribute values)))

(def xf (map columnizer))
(def column-by-attribute
  (transduce xf conj [] (range 10)))
