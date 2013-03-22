(defparameter *small* 1)
(defparameter *big* 100)

;; This is an overly clever way of
;; (/ (+ small big) 2)
;; Or the midpoint of big and small
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

;; Guessing is done the following way,
;; if you tell me the number is smaller,
;; my last guess minus one becomes my new upper bound
;; if you tell me the number is larger,
;; my last guess plus one becomes the lower bound 
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))
(defun bigger ()
  (setf *smaller* (1+ (guess-my-number)))
  (guess-my-number))

;; Since the game modifies the small and big globals,
;; starting over means resetting them and making a new guess
(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))
