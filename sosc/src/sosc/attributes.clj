(ns sosc.attributes)

(def fewer-short-jobs-legend
  {:question "In the last 5 years, how many full-time jobs have you held for < 6
              months (not including jobs while in school"
   :responses {:none     -1
               :one      -2
               :two      -3
               :three    -4
               :four     -5
               :five     -6}})

(def longest-job-legend
  {:question  "What the the longest amount of time you have worked at a single
               company?"
   :responses {:not-applicable       1
               :less-than-3-months   1
               :two-years            2
               :three-years          3
               :four-years           4
               :five-years           5
               :more-than-five-years 6}})

(def total-applicant-count [1144575])

(def criminal-record?   {:max 1
                         :min 0
                         :exists? 264091 })

(def school?             {:max 1
                         :min 0
                         :exists? 285065 })

(def job-stability? {:exists?           753259
                     :fewer-short-jobs? {:min -5
                                         :max -1}
                     :longest-job?      {:min 1
                                         :max 6}})



(filter (:exists? criminal-record?) total-applicant-count)


(defn make-data-usable [data])



(comment






  )
