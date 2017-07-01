(ns reactor.handlers.common)


;; =============================================================================
;; Dependencies
;; =============================================================================


(defn ->db [deps] (:db deps))


(defn ->public-hostname [deps] (:public-hostname deps))


(defn ->mailer [deps] (:mailer deps))


(defn ->cf [deps] (:community-safety deps))


(defn ->slack [deps] (:slack deps))


(defn ->weebly [deps] (:weebly deps))


(defn ->stripe [deps] (:stripe deps))
