(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                        there is a well in front of you.))
                               (attic (you are in the attic.
                                       there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)  
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

; makes a string from the node name
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

; makes a name for a graph label and limits the length
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

; makes a dot file description of a node
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

; makes a dot file description of an edge
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

; builds the dot file from the edges collection and the nodes collection
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

; writes out the undirected version of the edges
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))
  
  ; creates an undirected graph
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

; turns a dot file in to a png file
(defun dot->png (fname thunk)
  (with-open-file (*standard-output* (concatenate 'string fname ".dot") :direction :output :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot")))

; makes a png from a directed graph
(defun dgraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (dgraph->dot nodes edges))))

; creates a png from an undirected graph 
(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

;(defun run ()
;  (ugraph->png "wizard" *nodes* *edges*))