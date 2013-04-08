;; Game data
;; *nodes* -> the rooms in the wizards house
;;            this is an associated list
(defparameter *nodes* '((living-room (you are in the living-room.  a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.  There is a well in front of you.))
                        (attic (you are in the attic.  there is a giant welding torch in the corner.))))
;; *edges* -> the exits from one room to another
;;            this is an associated list
;;            the values are stored as (destination direction portal-description)
(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))
;; *objects* -> a list of objects in the game
(defparameter *objects* '(whiskey bucket frog chain))
;; *objects* -> a list of objects and their location
;;              this is an associated list
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))
;; The current location of the player
(defparameter *location* 'living-room)
;; A list of commands allowed to be converted to function calls
(defparameter *allowed-commands* '(look walk pickup inventory))

;; The description functions
;; describe-location -> returns the value from the locations in *nodes*
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
;; describes a single pathway
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;; collects the descriptions of all paths in a location
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
;; collects the descriptions of all objects at a location
(defun describe-objects (loc objs obj-loc)
                  (labels ((describe-obj (obj) `(you see a ,obj on the floor.)))
                    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; collects all the objects in a location
(defun objects-at (loc objs obj-loc)
  (labels ((is-at (obj) (eq (cadr (assoc obj obj-loc)) loc)))
    (remove-if-not #'is-at objs)))

;; collects the descriptions for locations, paths, and objects
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
;; looks up the destination on a path and moves the player
(defun walk (direction)
  (labels ((correct-way (edge) (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.)))))
;; moves an object from a location to the location 'body
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))
;; collects the objects at location 'body
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; the main game loop
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; turns the input command in to an sexpression
(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; runs sexpressions if allowed
(defun game-eval (sexp)
  ; what needs to be white-listed is the function in the sexpression
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

;; turns the retuned sexpressions in to text
(defun game-print (lst)
  ; print the caracters of
     ; the tweaked text after coercing it into a string
        ; the string tweaked is a coerced list after making the simbles happen
           ; oh, and we eliminate empty lists
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))

;; fine tunes text for output
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
            ;; if the item is a space, add it to the list
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ;; if the item is punctuation, add it to the list adn capitalize the next item
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ;; if the item is a quote, skip it, but toggle literal
            ((eql item #\") (tweak-text rest caps (not lit)))
            ;; if we are literal, add this item to the list.
            (lit (cons item (tweak-text rest nil lit)))
            ;; if we are cap, upcase the text and add it to the list
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            ;; else, downcase and add to the list with no caps and no literals
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


