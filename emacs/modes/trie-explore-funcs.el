;;; trie-explore-funcs.el --- Description -*- lexical-binding: t; -*-
;; Overlays
(defun trie-explore/make-overlay (beg end layer)
  " Create or reuse an overlay to use for a particular layer in the tree "
  (let ((overlay (or (gethash layer (trie-explore/tree-data-overlays trie-explore/current-data))
                     (puthash layer (make-overlay 1 2) (trie-explore/tree-data-overlays trie-explore/current-data))))
        )
    (overlay-put overlay 'face 'acab-face-0)
    ;; `((foreground-color . ,trie-explore/selection-color)))
    (move-overlay overlay beg (- end 3))
    )
  )
(defun trie-explore/clear-overlays ()
  " Clear all overlays "
  (cl-loop for x in (hash-table-values (trie-explore/tree-data-overlays trie-explore/current-data)) do
        (delete-overlay x)
        )
  (clrhash (trie-explore/tree-data-overlays trie-explore/current-data))
  )
;;--------------------------------------------------
;; Utils

(defmacro trie-explore/pop-list-to-length (lst n)
  " Pop values off a list IN PLACE until it is a given length,
then return its new head "
  `(progn (while (and ,lst (> (length ,lst) ,n))
            (pop ,lst)
            )
          (car ,lst))
  )
(defmacro trie-explore/reduce-list-length (lst n)
  " Pop values off a DUPLICATED list until it is a given length,
then return the modified list "
  `(let* ((the_list (seq-copy ,lst)))
     (while (and the_list (> (length the_list) ,n))
       (pop the_list)
       )
     the_list)
  )

(defun trie-explore/cols-to-bounds (seq col &optional maxlen)
  " Given a sequence of indents and a column value,
calculate the bounds that column falls within "
  (if (not maxlen) (setq maxlen 20))
  (let* ((indents (copy-seq seq))
         (upper 0)
         (lower 0)
         )
    (if (>= col (car indents))
        (setq lower (car indents)
              upper (+ lower maxlen))
      (while (and indents (< col (car indents)))
        (setq upper (pop indents)
              lower (or (car indents) 0))
        )
      )
    ;; (message "Upper: %s Lower: %s" upper lower)
    (list upper lower)
    )
  )
(defun trie-explore/cols-to-pos (bounds)
  " Convert at least a pair of bounding columns to actual buffer positions "
  (let* ((lower (cadr bounds))
         (upper (car bounds))
         )
    (list (trie-explore/col-to-pos upper) (trie-explore/col-to-pos lower))
    )
  )
(defun trie-explore/col-to-pos (col)
  " Convert a column value to an actual buffer position "
  (save-excursion
    (move-to-column col t)
    (point))
  )
(defun trie-explore/col-to-layer (col)
  " Convert a column value to the corresponding layer of the tree "
  (let* ((indents (reverse (copy-seq (trie-explore/tree-data-indents trie-explore/current-data))))
         (cal-layer 0)
         )
    (while (and indents (>= col (car indents)))
      (pop indents)
      (cl-incf cal-layer)
      )
    ;; (message "Col to Layer: %s -> %s" col cal-layer)
    cal-layer
    )
  )

(defun trie-explore/print-state ()
  (let* ((data (save-excursion
                 (outline-previous-heading)
                 (get-text-property (point) :tree-data)))
         (indents (trie-explore/tree-data-indents data))
         (path (trie-explore/tree-data-curr-path data))
         (maxlines (trie-explore/tree-data-max-lines data))
         (start-pos (marker-position (trie-explore/tree-data-start-pos data)))
         (path-pos (marker-position (trie-explore/tree-data-path-pos data)))
         )
    (message "\nState:\nIndents: %s\nPath: %s\nMax Lines: %s\nStart Pos: %s\nPath Pos: %s\n\n"
             indents path maxlines start-pos path-pos)
    )
  )
;;--------------------------------------------------
;; Drawing
(defun trie-explore/draw-children ()
  (let* ((rev-path (reverse (trie-explore/tree-data-curr-path trie-explore/current-data)))
         (node (trie-data/tree-get (trie-explore/tree-data-root trie-explore/current-data) rev-path))
         (layer (+ 1 (length rev-path)))
         )
    (trie-explore/draw-children-safe node layer))
  )
(defun trie-explore/draw-children-safe (node layer)
  (let* ((children (trie-data/node-children node))
         (num_children (length (hash-table-values children)))
         (maxlen (if (hash-table-empty-p children) 20
                   (apply 'max (mapcar (lambda (x) (length x))
                                       (hash-table-keys children)))))
         (indent_lower (car (trie-explore/tree-data-indents trie-explore/current-data)))
         (indent_upper (+ indent_lower maxlen))
         )
    (if (hash-table-empty-p children) (message "HASH empty"))
    ;;move to the indent head position
    (goto-char (marker-position (trie-explore/tree-data-start-pos trie-explore/current-data)))
    (set-marker trie-explore/next-heading (save-excursion (outline-next-visible-heading 1) (point)))
    (seq-each (lambda (x)
                (move-to-column indent_lower t)
                (delete-region (point) (line-end-position))
                ;;TODO: possibly limit size of x
                (insert x)
                (put-text-property (trie-explore/col-to-pos indent_lower)
                                   (trie-explore/col-to-pos indent_upper)
                                   :layer layer)
                (move-to-column (+ indent_lower maxlen) t)
                (insert " : ")
                ;; Move forward a line if there is one already, otherwise add one
                (forward-line 1)
                (if (<= (marker-position trie-explore/next-heading) (point))
                    (progn (forward-line -1) (goto-char (line-end-position)) (newline)))
                ) (sort (hash-table-keys children) (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    ;; Clear any remaining lines
    (if (> (trie-explore/tree-data-max-lines trie-explore/current-data) num_children)
        (let ((clear-lines (make-list (- (trie-explore/tree-data-max-lines trie-explore/current-data) num_children) nil)))
          (message "Clearing %s lines" (length clear-lines))
          (while (< (point) (marker-position trie-explore/next-heading))
            (move-to-column indent_lower t)
            (delete-region (point) (line-end-position))
            (forward-line 1)
            )
          )
      )
    )
  nil
  )
(defun trie-explore/draw-path ()
  " Draw the path through the tree currently being used "
  (save-excursion
    (goto-char (marker-position (trie-explore/tree-data-path-pos trie-explore/current-data)))
    (delete-region (point) (line-end-position))
    (seq-each (lambda (pair)
                (move-to-column (car pair) t)
                (insert (cdr pair)))
              (-zip-pair (reverse (trie-explore/tree-data-indents trie-explore/current-data))
                         (reverse (trie-explore/tree-data-curr-path trie-explore/current-data))))
    (add-text-properties (marker-position (trie-explore/tree-data-path-pos trie-explore/current-data)) (line-end-position)
                         '(font-lock-ignore t))
    (add-face-text-property (marker-position (trie-explore/tree-data-path-pos trie-explore/current-data)) (line-end-position)
                            'acab-face-1)
                            ;;`(:foreground ,trie-explore/path-color))
    )
  )

;;--------------------------------------------------
;; Interaction

(defun trie-explore/update-tree-data()
  (interactive)
  ;;Use correct tree data:
  (save-excursion
    (outline-previous-heading)
    (setq trie-explore/current-data (get-text-property (point) :tree-data))
    )
  ;;Calculate and update:
  (let* ((current-indent (current-column))

         (layer (trie-explore/col-to-layer current-indent))

         (short-path (trie-explore/reduce-list-length (trie-explore/tree-data-curr-path trie-explore/current-data) (- layer 1)))

         (parent (trie-data/tree-get (trie-explore/tree-data-root trie-explore/current-data) (reverse short-path)))
         (pchildren (trie-data/node-children parent))
         (pmaxlen (if (hash-table-empty-p pchildren) 20
                    (apply 'max (mapcar (lambda (x) (length x)) (hash-table-keys pchildren)))))

         (bounds (trie-explore/cols-to-bounds (trie-explore/tree-data-indents trie-explore/current-data) current-indent (+ pmaxlen 3)))
         (positions (trie-explore/cols-to-pos bounds))

         (substr (if (< 0 layer) (string-trim (buffer-substring (cadr positions) (- (car positions) 3))) nil))

         (new-path (if (< 0 layer) (cons substr short-path) '()))

         ;;node inspection
         (node (trie-data/tree-get (trie-explore/tree-data-root trie-explore/current-data) (reverse new-path)))
         (children (trie-data/node-children node))
         (num_children (length (hash-table-values children)))
         (max-children (max (trie-explore/tree-data-max-lines trie-explore/current-data) num_children))
         (indent_upper (car bounds))
         )

    (if (or (not (string-empty-p substr)) (eq 0 layer))
        (progn
          ;;update tree data
          (setf (trie-explore/tree-data-curr-path trie-explore/current-data) new-path
                (trie-explore/tree-data-max-lines trie-explore/current-data) max-children)
          (trie-explore/pop-list-to-length (trie-explore/tree-data-indents trie-explore/current-data) layer)
          (push indent_upper (trie-explore/tree-data-indents trie-explore/current-data))
          ;; (trie-explore/print-state)
          (set-marker (car trie-explore/current-markers) (cadr positions))
          (set-marker (cadr trie-explore/current-markers) (car positions))
          (setq trie-explore/current-layer layer)
          )
      (progn
        (set-marker (car trie-explore/current-markers) (point-min))
        (set-marker (cadr trie-explore/current-markers) (point-min))
        (setq trie-explore/current-layer 0)
        )
      )
    (message "\n-----\nDATA:\nPath: %s\nIndents: %s\nLayer: %s\n"
             (trie-explore/tree-data-curr-path trie-explore/current-data)
             (trie-explore/tree-data-indents trie-explore/current-data)
             trie-explore/current-layer)
    )
  )
(defun trie-explore/expand-entry ()
  " Expand the node the user selects "
  (interactive)
  (trie-explore/update-tree-data)
  (save-excursion
    (trie-explore/draw-children)
    (trie-explore/draw-path)
    ;;TODO: make overlay
    )
  (if (> trie-explore/current-layer 0)
      (trie-explore/make-overlay (marker-position (car trie-explore/current-markers))
                                 (marker-position (cadr trie-explore/current-markers))
                                 trie-explore/current-layer)
    (trie-explore/clear-overlays)
    )
  )

(defun trie-explore/insert-entry ()
  " insert a new node into the tree from insert state"
  (interactive)
  (trie-explore/update-tree-data)
  (let* ((curr-data trie-explore/current-data)
         (parent-path (trie-explore/tree-data-curr-path curr-data))
         (node (trie-data/tree-get (trie-explore/tree-data-root curr-data) (reverse parent-path)))
         (bounds (trie-explore/cols-to-pos (trie-explore/cols-to-bounds (trie-explore/tree-data-indents curr-data) (current-column))))
         (substr (string-trim (buffer-substring-no-properties (cadr bounds) (point))))
         )
    (if (not (eq trie-explore/current-layer (length parent-path)))
        (message "Path / Layer Mismatch")
      (progn
        (trie-data/node-add-child node substr)
        (save-excursion
          (move-to-column (- (current-column) (length substr) 3))
          (trie-explore/expand-entry)
          )
        )
      )
    )
  )
(defun trie-explore/insert-at-leaf ()
  " Insert from minibuffer "
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq trie-explore/current-data (get-text-property (point) :tree-data))
    )
  (let* ((value (read-string "Value: "))
         (parent-path (trie-explore/tree-data-curr-path trie-explore/current-data))
         (node (trie-data/tree-get (trie-explore/tree-data-root trie-explore/current-data) (reverse parent-path)))
         )
    (trie-data/node-add-child node value)
    )
  ;;TODO: move to the parent node before re-display
  (trie-explore/expand-entry)
  )

(defun trie-explore/delete-entry ()
  (interactive)
  (trie-explore/update-tree-data)
  ;; get the substring
  (let ((to-delete (car (trie-explore/tree-data-curr-path trie-explore/current-data)))
        (path (cdr (trie-explore/tree-data-curr-path trie-explore/current-data))))
    (trie-data/tree-remove (trie-explore/tree-data-root trie-explore/current-data)
                           (reverse path) to-delete)
    (save-excursion
      (trie-explore/layer-decrease)
      (trie-explore/expand-entry)
      )
    )
  )

(defun trie-explore/layer-decrease ()
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq trie-explore/current-data (get-text-property (point) :tree-data))
    )
  (let ((indents (seq-copy (trie-explore/tree-data-indents trie-explore/current-data)))
        (curr-point (current-column))
        (start-pos (trie-explore/tree-data-start-pos trie-explore/current-data))
        last
        )
    (while (and indents (<= curr-point (car indents)))
      (setq last (pop indents))
      )
    (if (car indents)
        (progn
          (move-to-column (car indents))
          (while (and (not (looking-at trie-explore/movement-regexp-guard))
                      (> (point) start-pos))
            (forward-line -1)
            (move-to-column (car indents))
            )
          )
      (move-to-column 0))
    )
  )
(defun trie-explore/layer-increase ()
  (interactive)
  (save-excursion
    (outline-previous-heading)
    (setq trie-explore/current-data (get-text-property (point) :tree-data))
    )
  (let ((indents (reverse (trie-explore/tree-data-indents trie-explore/current-data)))
        (curr-point (current-column))
        (start-pos (marker-position (trie-explore/tree-data-start-pos trie-explore/current-data)))
        last
        )
    (while (and indents (>= curr-point (car indents)))
      (setq last (pop indents))
      )
    (move-to-column (or (car indents) last))
    (progn
      (while (and (not (looking-at trie-explore/movement-regexp-guard))
                  (> (point) start-pos))
        (forward-line -1)
        (move-to-column (or (car indents) last))
        )
      (if (looking-at "-")
          (progn (forward-line)
                 (move-to-column (or (car indents) last)))
        )
      )
    )
  )
;;--------------------------------------------------
;; Setup
(defun trie-explore/explore-current-buffer ()
  (interactive)
  ;;insert current buffer into temp
  (let* ((current (current-buffer))
         (name (s-replace "*" "" (buffer-name current)))
         (tree (make-trie-data/node :name "__root"))
        )
    (with-temp-buffer
      (insert-buffer-substring-no-properties current)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (trie-data/tree-add tree
                            (s-split-words (buffer-substring (line-beginning-position) (line-end-position)))
                            (forward-line)
                            )
        )
      )
  ;;create new buffer, set it to explore mode,
    (with-current-buffer-window (format "*%s-Explore*" name)
        'display-buffer-use-some-window
        'trie-explore/quit-action-make-writable
      (trie-explore-mode)
      (trie-explore/initial-setup tree name)
      )
    )
  )

(defun trie-explore/quit-action-make-writable (a b)
  (with-current-buffer (window-buffer a)
    (read-only-mode 0)))

(defun trie-explore/initial-setup (&optional tree tree-name)
  " An initial setup for a tree "
  (interactive)
  (cond
   ((not tree) (progn (setq tree (make-trie-data/node :name "__root"))
                      (trie-data/generate-tree tree 5 5)))
   ((equal tree t) (setq tree (make-trie-data/node :name "__root")))
   )

  (let* ((tree-data (make-trie-explore/tree-data
                     :name (or tree-name (read-string "Tree Name: "))
                     :root tree))
         indent-amount
         )
    (setq trie-explore/current-data tree-data)
    (goto-char (point-max))
    ;; Put the basic data into the heading
    (insert (propertize (format "\n** %s: " (trie-explore/tree-data-name trie-explore/current-data))
                        :tree-data trie-explore/current-data))
    ;;store the indent
    (setq indent-amount (current-column))
    (set-marker (trie-explore/tree-data-path-pos trie-explore/current-data) (point))
    (newline)
    ;; draw a divider line
    (move-to-column indent-amount t)
    (insert (make-string (- 80 indent-amount) ?-))
    (newline)
    ;;draw the root node
    (move-to-column indent-amount t)
    (insert (propertize "Root: " :layer 0))
    (push (current-column) (trie-explore/tree-data-indents trie-explore/current-data))
    (set-marker (trie-explore/tree-data-start-pos trie-explore/current-data) (point))
    (insert "\n")
    ;;Draw all children, indented, with " : " after
    (trie-explore/draw-children)
    (goto-char (point-max))
    (newline 2)
    )
  )

(provide 'trie-explore-funcs)
;;; trie-explore-funcs.el ends here
