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
          (ave-loe no-criminal-history)

          second-chance-loe
          (ave-loe second-chance-candidates)]
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
                                  no-criminal-history-loe]}]
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
; helper to transform data from int->bool augmenting data
; filters terminated employees
; aggregate
(defn quit-vs-fired [candidate-pool]
  (letfn [(terminated? [{:keys [terminated] :as candidate}] ;step fns (comp (map add-terminated) (map add-fired) (..)
            (assoc candidate :terminated? (= terminated 1)))   ;               (filter :terminated?)

          (fired? [{:keys [involuntary-t]}]
            (= involuntary-t 1))

          (quit? [{:keys [voluntary-t]}]
            (= voluntary-t 1))

          (misconduct? [{:keys [misconduct]}]
            (= misconduct 1))]
    (let [terminated-employees
          (filter terminated? candidate-pool)]

      {:total-employees-terminated ;monadic fn returns this hashmap
       (count terminated-employees) ;diatic fn is conj

       :total-employees-who-quit
       (->> terminated-employees
            (filter quit?) ; (filter :quit?)
            count)

       :total-fired-employees
       (->> terminated-employees
            (filter fired?)
            count)

       :fired-for-misconduct
       (->> terminated-employees
            (filter misconduct?)
            count)})))

;;; start of the refactor


(defn add-terminated [{:keys [terminated] :as candidate}]
  (assoc candidate :terminated? (= terminated 1)))

(defn add-fired [{:keys [involuntary-t] :as candidate}]
  (assoc candidate :fired? (= involuntary-t 1)))

(defn add-quit [{:keys [voluntary-t] :as candidate}]
  (assoc candidate :quit? (= voluntary-t 1)))

(defn add-misconduct [{:keys [misconduct] :as candidate}]
  (assoc candidate :misconduct? (= misconduct 1)))

;; could do something like ^^ but this is more slick
;; all this is doing is converting all the 1 -> true bc booleans are easier to work with

(defn int->bool [{:keys [terminated
                         involuntary-t
                         voluntary-t
                         misconduct :as candidate]}]
  (cond-> candidate
    (= terminated 1) (assoc :terminated? true)
    (= involuntary-t 1) (assoc :fired? true)
    (= voluntary-t 1) (assoc :quit? true)
    (= misconduct 1) (assoc :misconduct? true)))

(def first-step (map int->bool valid-hired-candidates))
(def second-step (filter :terminated? first-step))

;first two step functions done, we compose these with comp, creating a function combinator= xff

(def xff (comp
          (map int->bool)
          (filter terminated?)))

; now to write the reducing fn for the transduce. i call it "the finisher" (yes mortal combat)

(def fin (finisher {} second-step))

(defn finisher [acc second-step] ;should be diatic fn
  (reduce (fn [acc {:keys [fired?
                          quit?
                          misconduct?] :as candidate}]
            (cond
              quit? (update acc :total-quit conj candidate)
              fired? (update acc :total-fired conj candidate)
              misconduct? (update acc :fired-for-misconduct conj candidate)))
          {:total-terminated (count second-step)
           :total-quit []
           :total-fired []
           :fired-for-misconduct []}
          second-step))

(transduce xff finisher valid-hired-candidates) ; WHY is this not working? Grrr...


; side-note... did you know conj can be a reducing function for transduce?

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
