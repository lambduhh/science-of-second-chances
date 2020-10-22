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

(def all-hired-candidates
  (csv-data->maps data))

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
  (transduce xf              conj             []  (range 10))
  ; transduce transforming-fns reducing  []  coll
  )

(defn valid-candidates [all-hired-candidates]
  (letfn [(fields-present? [row] (some nil? (vals row)))
          (job-position [{:keys [position]}] (or (= position 1)
                                                 (= position 2)))]
    (->> all-hired-candidates
         (remove fields-present?)
         (filter job-position))))

(defn separate-gen-pop [valid-hired-candidates]
  (reduce (fn [acc {:keys [criminal-history] :as person}]
            (if (= criminal-history 1)
              (update acc :second-chance-candidates conj person)
              (update acc :no-criminal-history conj person)))
          {:second-chance-candidates []
           :no-criminal-history []}
          valid-hired-candidates))
(comment
  (def separated-by-ch (separate-gen-pop valid-hired-candidates))

  => {:second-chance-candidates
      ({:ID 1, :longest-job 2, :school 1, :fewest-short-jobs -2, :voluntary-t 0, :LOE 1236, :terminated 0, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 0}
       {:ID 4, :longest-job 3, :school 0, :fewest-short-jobs -5, :voluntary-t 0, :LOE 1000, :terminated 1, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 1} {:ID 14, :longest-job 1, :school 1, :fewest-short-jobs -3, :voluntary-t 0, :LOE 1036, :terminated 0, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0} {:ID 19, :longest-job 2, :school 1, :fewest-short-jobs -4, :voluntary-t 0, :LOE 1236, :terminated 0, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0} {:ID 24365, :longest-job 2, :school 1, :fewest-short-jobs 1, :voluntary-t 1, :LOE 653, :terminated 1, :misconduct 1, :position 1, :criminal-history 1, :involuntary-t 0} {:ID 26242, :longest-job 1, :school 1, :fewest-short-jobs -5, :voluntary-t 1, :LOE 735, :terminated 1, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0} {:ID 6536, :longest-job 2, :school 1, :fewest-short-jobs -3, :voluntary-t 1, :LOE 632, :terminated 1, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 0}
       {:ID 2535, :longest-job 2, :school 1, :fewest-short-jobs -3, :voluntary-t 1, :LOE 723, :terminated 1, :misconduct 0, :position 2, :criminal-history 1, :involuntary-t 0}
       {:ID 6436, :longest-job 1, :school 1, :fewest-short-jobs -1, :voluntary-t 1, :LOE 733, :terminated 1, :misconduct 0, :position 1, :criminal-history 1, :involuntary-t 0})
      , :no-criminal-history
      ({:ID 13, :longest-job 1, :school 0, :fewest-short-jobs -2, :voluntary-t 1, :LOE 89, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 0}
       {:ID 16, :longest-job 3, :school 1, :fewest-short-jobs -4, :voluntary-t 0, :LOE 1936, :terminated 0, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 0}
       {:ID 18, :longest-job 4, :school 1, :fewest-short-jobs -3, :voluntary-t 0, :LOE 1036, :terminated 0, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 0}
       {:ID 24, :longest-job 4, :school 1, :fewest-short-jobs -5, :voluntary-t 0, :LOE 1234, :terminated 1, :misconduct 0, :position 1, :criminal-history 0, :involuntary-t 1}
       {:ID 4325, :longest-job 4, :school 1, :fewest-short-jobs -2, :voluntary-t 0, :LOE 234, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 1}
       {:ID 6345, :longest-job 4, :school 1, :fewest-short-jobs -3, :voluntary-t 0, :LOE 543, :terminated 1, :misconduct 1, :position 1, :criminal-history 0, :involuntary-t 1}
       {:ID 526, :longest-job 6, :school 1, :fewest-short-jobs -1, :voluntary-t 0, :LOE 253, :terminated 1, :misconduct 0, :position 2, :criminal-history 0, :involuntary-t 1})})

(defn average
  [numbers]
  (if (empty? numbers) 0
      (float (/ (reduce + numbers) (count numbers)))))

(defn length-of-employment [candidate-pool]
  (map :LOE candidate-pool))

; so we have these two helpers abstracted out.. could be beneficial to use in other places
; this function takes "valid-hired-candidates" data instead of (separate-gen-pop valid-hired-candidates) by design..
;  -> all the fns in the app are meant to take the same input and do the data transforamtion internally
;   -> if each fn does its own transformation, less fn dependent logic means that it's likely a bug in one function can detrimentally effect other parts/functions
; easy to follow, very declarive/expressive?


(defn compare-loe-alpha [valid-hired-candidates]
  (let [average-LOE
        (->> (length-of-employment valid-hired-candidates)
             (average))

        criminal-histories
        (criminal-history valid-hired-candidates)

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
(comment
  (compare-loe-alpha valid-hired-candidates))
; change those caps to lowercase (somebody once told me that "dont use uppercase, everyone will hate you)
; move the length-of-employment function into a letfn (we only use it here)
;  -> average is a very abstract helper that may have use in other places.. I would typically move to a 'utils' file
; use comp instead of thread-last

; (how many lines did I actually save?)
; (what are the trade-off of using comp?)
; ->  similar to partial (oh its fancy, make you feel powerful :) )
;   -> so a traditionalist functional POV may encourage? but vs idiomatic clojure discourages
; (did it make it more OR less readable?)
; (benefit of starting to us comp is training for transducers)


(defn compare-loe-second [valid-hired-candidates]
  (letfn [(length-of-employment [candidate-pool]
            (map :LOE candidate-pool))]
    (let [gen-pop-average-loe
          (->> (length-of-employment valid-hired-candidates)
               (average))

          separated-gen-pop
          (criminal-history valid-hired-candidates)

          ave-loe
          (comp average length-of-employment)

          no-criminal-history-loe
          (ave-loe (:no-criminal-history separated-gen-pop))

          second-chance-loe
          (ave-loe (:second-chance-candidates separated-gen-pop))]
      {:average-loe               gen-pop-average-loe
       :second-chance-loe         second-chance-LOE
       :no-criminal-history-loe   no-criminal-history-loe
       :longest-tenure-on-average (if (< second-chance-LOE no-criminal-history-loe)
                                    "no criminal history"
                                    "second-chance-candidates")

       :difference-in-tenure      (if (< second-chance-loe no-criminal-history-loe)
                                    (str (- no-criminal-history-loe second-chance-loe) " days")
                                    (str (- second-chance-loe no-criminal-history-loe) " days"))})))

; still some things wrong with this...
; what if we chose to pass in the value of (separate-gen-pop valid-hired-candidates)?
; -> still need to get 'all-candidates' to return average-loe
; another concern is that since we are passing in the map that is the result of a former fn
; -> opens up to concerns about "passing down bugs"
; heading in the right direction !

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
       :longest-tenure-on-average (if (< second-chance-LOE no-criminal-history-loe)
                                    "no criminal history"
                                    "second-chance-candidates")

       :difference-in-tenure      (if (< second-chance-loe no-criminal-history-loe)
                                    (str (- no-criminal-history-loe second-chance-loe) " days")
                                    (str (- second-chance-loe no-criminal-history-loe) " days"))})))

; dont want to mix in rendering vs data.. separate view logic out

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

(comment
   (def separated-by-ch (separate-gen-pop valid-hired-candidates))
  (compare-loe-fourth separated-by-ch))

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

(comment
  (render-compare-loe (compare-loe-fourth separated-by-ch))

  {:average-loe                  average-loe
   :loe-second-chance            second-chance-loe
   :loe-no-criminal-history      no-criminal-history-loe
   :second-chance-longer-tenure? (if (< second-chance-loe no-criminal-history-loe)
                                   "no criminal history"
                                   "second-chance-candidates")

   :difference-in-tenure         (Math/abs (< second-chance-loe no-criminal-history-loe))}
  (def old-compare-loe (compare-loe valid-hired-candidates)))

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
