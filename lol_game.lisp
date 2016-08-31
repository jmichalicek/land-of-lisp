(defparameter *nodes* '((living-room (You are in the living-room.
                                          A wizard is snoring loudly on the couch.))
                        (garden (You are in a beautiful garden.
                                     There is a well in front of you.))
                        (attic (You are in the attic.
                                    There is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attric (living-room downstairs ladder))))

(defparameter *objects* '(wiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))


(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  ;; backtick, not single quote, to enable quasiquoting
  ;; commas flop the data mode off so that caddr and cadr may be used
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  ;; Takes a location symbol and alist of edges with locations list
  ;; and returns the joined descriptions of each path (or edge) from that
  ;; location.  The #' before function names is Lisp shorthand for
  ;; specifying that it is a function call - longhand is
  ;; (mapcar (function describe-path) (cdr (assoc location edges)))
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  ;; use of labels function to create a nested inner function which we then call
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
          (remove-if-not #'at-loc-p objs)))
