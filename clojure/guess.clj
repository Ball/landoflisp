;; Atoms give me something referenceable that I can change
(def *small* (atom 1))
(def *big* (atom 100))

;; a guess is the center of the endpoint ranges
(defn guess-my-number []
  (int (/ (+ @*small* @*big*) 2)))

;; if a number is bigger than the guess,
;; the lower bound is at least one bigger than the number
(defn bigger []
  (reset! *small* (inc (guess-my-number))))

;; vice-versa for a number smaller than the guess
(defn smaler []
  (reset! *big* (dec (guess-my-number))))

;; when starting over, we reset the upper and lower bounds
(defn start-over []
  (reset! *small* 1)
  (reset! *big* 100)
  (guess-my-number))
