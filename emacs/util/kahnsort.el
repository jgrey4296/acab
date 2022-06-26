;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;Module to topologically sort a graph
;;Uses a buffer for the process queue
;;input graph is a hashmap of alists: `((:name . X) (:inputs . Y) (:outputs . Z))
(provide 'kahnsort)

(defun kahn/is-terminal-p (terms str)
  (-contains? terms str))
(defun kahn/pop ()
  (goto-char (point-min))
  (let ((str (string-trim (buffer-substring-no-properties
                           (point-min)
                           (line-end-position)))))
    (delete-region (point-min) (line-end-position))
    (join-line 1)
    str))
(defun kahn/push (x)
  (goto-char (point-max))
  (insert (format "%s\n" x))
  )
(defun kahnsort (nodes initial terminals)
  " Run a Topological sort on the input NODES graph,
starting with the INITIAL list.
Returns (SORTED STILL-ACTIVE UNDISCOVERED) "
  (with-temp-buffer
    (mapcar (lambda (x) (kahn/push x)) initial)
    (let* ((graph nodes)
           (exhausted '())
           (maxLayerMap (make-hash-table :test 'equal))
           )
      (while (/= (point-min) (point-max))
        (let* ((current_name (kahn/pop))
               (current (gethash current_name graph))
               (outputs (cdr (assoc :outputs current)))
               ;; Terminals can be layer 1, everything else is >2. one less so always +1 on puthash
               (maxLayer (gethash current_name maxLayerMap (if (kahn/is-terminal-p terminals current_name) 0 1))))
          (message "Kahn Frontier Current : %s" current_name)
          (if (not (-contains? exhausted current_name))
              (progn (push current_name exhausted)
                     (puthash current_name (+ 1 maxLayer) maxLayerMap)
                     (loop for x in outputs do
                           (puthash x (max (+ 1 maxLayer) (gethash x maxLayerMap 1)) maxLayerMap)
                           (if (not (-difference (cdr (assoc :inputs (gethash x graph))) exhausted))
                               (progn (message "Adding %s to frontier" x)
                                      (kahn/push x))))))))

      (let ((still-active (-difference (hash-table-keys maxLayerMap) exhausted))
            (undiscovered (-difference (hash-table-keys graph) (hash-table-keys maxLayerMap)))
            (sorted (mapcar (lambda (x) `(,x . ,(gethash x maxLayerMap))) exhausted))
            )
        `(,sorted ,still-active ,undiscovered)))))
