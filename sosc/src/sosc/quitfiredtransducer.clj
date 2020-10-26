(ns sosc.quitfiredtransducer)

(defn add-terminated [{:keys [terminated] :as candidate}]
  (assoc candidate :terminated? (= terminated 1)))

(defn add-fired [{:keys [involuntary-t] :as candidate}]
  (assoc candidate :fired? (= involuntary-t 1)))

(defn add-quit [{:keys [voluntary-t] :as candidate}]
  (assoc candidate :quit? (= voluntary-t 1)))

(defn add-misconduct [{:keys [misconduct] :as candidate}]
  (assoc candidate :misconduct? (= misconduct 1)))

(defn foo [{:keys [terminated
                   involuntary-t
                   voluntary-t
                   misconduct] :as candidate}]
  (cond-> {}
    (= terminated 1) (assoc :terminated? candidate)
    (= involuntary-t 1) (assoc :fired? candidate)
    (= voluntary-t 1) (assoc :quit? candidate)
    (= misconduct)))

(map foo valid-hired-candidates)

(def xf (comp (map foo)))
