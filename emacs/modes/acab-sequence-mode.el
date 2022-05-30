;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; A Custom mode adapting org-table
(require 'cl-lib)
(require 'dash)
(require 'org-table)
(require 'kahnsort)

(provide 'acab-sequence-mode)
;; add transient / hydra
;; add helm

;;--------------------
;; Mode Variables
;;--------------------

(defgroup acab-sequence '() "Customization group for acab-sequence"
  :group 'acab-ide)

(defconst acab-sequence/left-tab "Acab-SequenceMLeft")
(defconst acab-sequence/right-tab "Acab-SequenceMRight")
(defcustom acab-sequence/info-tab "*Seq Info*" "Name of Sequence info buffer" :type '(string))

(defcustom acab-sequence/current-colour "green" "Overlay colour for current selection" :type '(color))
(defcustom acab-sequence/input-colour "blue" "Overlay colour for input selection" :type '(color))
(defcustom acab-sequence/output-colour "orange" "Overlay colour for output selection" :type '(color))
(defcustom acab-sequence/rule-divider "->" "String signifying rule rhs"  :type '(string))
(defcustom acab-sequence/overlay-max 20 "Maximum number of overlays for acab-sequence" :type '(integer))

(defvar acab-sequence/overlays '())
(defvar acab-sequence/free-overlays '())
(defvar acab-sequence/inspector-overlay nil)
;;--------------------
;; Overlays
;;--------------------
(defun acab-sequence/make-overlay (beg end type)
  (let ((overlay (if acab-sequence/free-overlays (pop acab-sequence/free-overlays) (make-overlay 1 2)))
        (color "green"))
    (if (not (-contains? acab-sequence/overlays overlay))
        (push overlay acab-sequence/overlays))
    (setq color (cond ((equal type :point) acab-sequence/current-colour)
                      ((equal type :input) acab-sequence/input-colour)
                      ((equal type :output) acab-sequence/output-colour)))
    (overlay-put overlay 'face `((foreground-color . ,color)))
    (overlay-put overlay 'font-lock-ignore t)
    (move-overlay overlay beg end)

    ))
(defun acab-sequence/clear-overlays ()
  (loop for x in acab-sequence/overlays do
        (push x acab-sequence/free-overlays)
        (delete-overlay x)
        )
  (setq-local acab-sequence/overlays '())
  )
(defun acab-sequence/set-overlays-for-current ()
  " Set overlays for the currently selected cell "
  (let* ((row (org-table-current-line))
         (col (org-table-current-column))
         (value (substring-no-properties (org-table-get row col)))
         (value-hash (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
         (value-obj (gethash value value-hash))
         ;; Get the inputs and outputs
         (inputs (acab-sequence/get value-obj :inputs))
         (outputs (acab-sequence/get value-obj :outputs))
         (wind (get-buffer-window acab-sequence/info-tab))
         )

    ;;Dealing with side window:
    (if wind
        (with-current-buffer acab-sequence/info-tab
          (goto-char (point-min))
          (search-forward-regexp (format "^*** %s" value) nil t)
          (if (not (overlayp acab-sequence/inspector-overlay))
              (progn (setq acab-sequence/inspector-overlay (make-overlay 1 2))
                     (overlay-put acab-sequence/inspector-overlay 'face '((foreground-color . "green")))))
          (move-overlay acab-sequence/inspector-overlay
                        (line-beginning-position)
                        (point)
                        (get-buffer acab-sequence/info-tab))))
    ;;Main window:
    (save-excursion
      (goto-char (org-table-begin))
      (search-forward value)
      (acab-sequence/make-overlay
       (progn (skip-chars-backward "^|") (point))
       (progn (skip-chars-forward "^|") (point))
       :point)

      ;;Overlay on inputs
      (loop for x in inputs do
            (goto-char (org-table-begin))
            (if (acab-sequence/is-terminal-p x)
                (progn (search-forward x)
                       (acab-sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :input)))
            (search-forward x)
            (acab-sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :input))

      ;;Overlay on outputs
      (loop for x in outputs do
            (goto-char (org-table-begin))
            (if (acab-sequence/is-terminal-p x)
                (progn (search-forward x)
                       (acab-sequence/make-overlay
                        (progn (skip-chars-backward "^|") (point))
                        (progn (skip-chars-forward "^|") (point))
                        :output)))
            (search-forward x)
            (acab-sequence/make-overlay
             (progn (skip-chars-backward "^|") (point))
             (progn (skip-chars-forward "^|") (point))
             :output))

      ;;TODO: overlay unconnected cells
      )
    )
  )

;;--------------------
;;Motion
;;--------------------
(defun acab-sequence/user-inc-column (count)
  " Increment the user point by a column,
    Updating the highlights as well "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (let* ((curr-table-column (org-table-current-column))
            (max-column org-table-current-ncol)
            (curr-col (current-column))
            (new-col (+ curr-table-column (if count (prefix-numeric-value count) 1)))
            )
        (org-table-goto-column (if (<= new-col max-column) new-col curr-table-column))
        (setq curr-col (current-column))
        ;;if cell is empty, go up until it isnt
        (while (and (org-at-table-p) (not (looking-at "[[:alnum:]-]")))
          (forward-line -1)
          (move-to-column curr-col)
          )
        (if (looking-at "-")
            (progn (forward-line)
                   (move-to-column curr-col))
          )
        ;;Deal with overlays
        (acab-sequence/clear-overlays)
        (acab-sequence/set-overlays-for-current)
        )
    ;;No
    (progn (acab-sequence/clear-overlays)
           (evil-forward-char (prefix-numeric-value count))))
  )
(defun acab-sequence/user-dec-column (count)
  "Decrement the user point by a column "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (let* ((curr-table-column (org-table-current-column))
            curr-col
            (new-col (- curr-table-column (if count (prefix-numeric-value count) 1)))
            )
        (org-table-goto-column (if (>= new-col 1) new-col curr-table-column))
        ;;Move to a non empty cell
        (setq curr-col (current-column))
        (while (not (looking-at "[[:alnum:]-]"))
          (forward-line -1)
          (move-to-column curr-col)
          )
        (if (looking-at "-")
            (progn (forward-line)
                   (move-to-column curr-col))
          )
        ;;Deal with overlays
        (acab-sequence/clear-overlays)
        (acab-sequence/set-overlays-for-current)
        )
    ;;No
    (progn (acab-sequence/clear-overlays)
           (evil-backward-char (prefix-numeric-value count))))
  )

(defun acab-sequence/user-dec-line (count)
  "decrement the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (- row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (acab-sequence/clear-overlays)
             (acab-sequence/set-overlays-for-current)
             )
    ;;No
    (progn (acab-sequence/clear-overlays)
           (evil-previous-line (prefix-numeric-value count))))
  )
(defun acab-sequence/user-inc-line (count)
  "increment the user point by a line "
  (interactive "P")
  (if (org-at-table-p)
      ;;Yes
      (progn (let ((row (org-table-current-line))
                   (col (org-table-current-column)))
               (org-table-goto-line (+ row (if count (prefix-numeric-value count) 1)))
               (org-table-goto-column col))
             (acab-sequence/clear-overlays)
             (acab-sequence/set-overlays-for-current)
             )
    ;;No
    (progn (acab-sequence/clear-overlays)
           (evil-next-line (prefix-numeric-value count))))
  )

(defun acab-sequence/centre-column ()
  " Centre the current column in the window "
  (interactive)
  (acab-sequence/horizontal-recenter)
  )
(defun acab-sequence/horizontal-recenter ()
  " Force horizontal recenter, based on:
https://stackoverflow.com/questions/1249497 "
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun acab-sequence/goto-position (row col)
  " In a table, go to the Column, Row specified "
  (interactive "N\nN")
  ;; Row first, then column, as goto-line resets column otherwise
  (org-table-goto-line row)
  (org-table-goto-column col))

(defun acab-sequence/scroll ()
  " Scroll Left or right "
  ;; get direction

  ;; get current indent column

  ;; move in direction

  ;; update indent column

  )

;;--------------------
;; Utilities
;;--------------------
(defun acab-sequence/get-table-prop (sym)
  " Get a property from the current table "
  (get-text-property (org-table-begin) sym))
(defun acab-sequence/push-table-prop (sym val)
  "Push a value to a list property of the current table"
  (let ((lst (get-text-property (org-table-begin) sym)))
    (push val lst)
    (put-text-property (org-table-begin) (org-table-end) sym lst)))

(defun acab-sequence/is-terminal-p (str)
  (-contains? (acab-sequence/get-table-prop :acab-sequence/terminals) str))

(defun acab-sequence/inputless ()
  (let* ((value-map (acab-sequence/get-table-prop :acab-sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (acab-sequence/get x :inputs)
                                      (acab-sequence/get x :name)))
                      (hash-table-values value-map)))))
(defun acab-sequence/outputless ()
  (let* ((value-map (acab-sequence/get-table-prop :acab-sequence/value-hashmap)))
    (-non-nil (mapcar (lambda (x) (if (acab-sequence/get x :outputs)
                                      (acab-sequence/get x :name)))
                      (hash-table-values value-map)))))

(defun acab-sequence/analyze-table ()
  " Update analytics on the table "
  (message "Analyzing table")
  (org-table-analyze)
  ;; create column sets
  (let* ((depth-hash (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
         (dh_max (if (hash-table-empty-p depth-hash) 0 (-max (hash-table-values depth-hash))))
         (true-max (max 3 (+ 1 dh_max))))
    (put-text-property (org-table-begin) (org-table-end) :acab-sequence/max-depth true-max))
  (let ((depth-map (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
        (depth-sets (make-vector (acab-sequence/get-table-prop :acab-sequence/max-depth) '())))
    (maphash (lambda (k v) (aset depth-sets (- v 1) (cons k (aref depth-sets (- v 1))))) depth-map)
    (put-text-property (org-table-begin) (org-table-end) :acab-sequence/depth-sets depth-sets)
    ))

(defun acab-sequence/insert-string (str inputs outputs &optional terminal)
  " Insert a possibly new exclusion string into the value-hashmap, updating IOs"
  (message "Inserting string")
  (let* ((value-hashmap (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
         (depth-hashmap (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
         (current (gethash str value-hashmap `((:name . ,str)
                                               (:inputs . nil )
                                               (:outputs . nil))))
         (updated (acab-sequence/update-IO current inputs outputs)))
    (puthash str updated value-hashmap)
    ;;Handle graph ordering:
    (cond ((acab-sequence/is-terminal-p str) nil) ;;already a terminal
          ;;New terminal
          (terminal (progn (acab-sequence/push-table-prop :acab-sequence/terminals str)
                           (puthash str 1 depth-hashmap)))
          ;; not a terminal, so calc depth
          (t (let ((min-max (acab-sequence/get-depth-range str)))
               (message "Min-Max: %s" min-max)
               ;; if no conflict, set depth
               (if (>= (- (cdr min-max) (car min-max)) 2)
                   (puthash str (+ 1 (car min-max)) depth-hashmap)
                 ;; if conflicts, need to shuffle a side,
                 ;; and insert a new column
                 (acab-sequence/kahnsort)
                 )
               )))))
(defun acab-sequence/remove-string (str)
  " Force a string to be completely removed "
  (let* ((value-hash (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
         (depth-hash (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
         (terminals (acab-sequence/get-table-prop :acab-sequence/terminals))
         (updated-terminals (remove str terminals))
         )
    (message "Removing %s from %s" str terminals)
    (message "Terminals: %s" updated-terminals)
    (remhash str value-hash)
    (remhash str depth-hash)
    (put-text-property (org-table-begin) (org-table-end) :acab-sequence/terminals updated-terminals)
    ))

(defun acab-sequence/get (obj sym)
  "Get the inputs from an object"
  (cdr (assoc sym obj)))
(defun acab-sequence/del (obj syms)
  "Delete a value from an alist"
  (cond ((not syms) obj)
        ((equal 'symbol (type-of syms))
         (delq (assoc syms obj) obj))
        ((equal 'cons (type-of syms))
         (acab-sequence/del (delq (assoc (car syms) obj) obj) (cdr syms)))))

(defun acab-sequence/update-IO (obj &optional i o)
  " Given an object from the value-hashmap, increment its count "
  (let* ((inputs (acab-sequence/get obj :inputs))
         (outputs (acab-sequence/get obj :outputs))
         (rest (acab-sequence/del obj '(:inputs :outputs))))
    (push `(:inputs . ,(union i inputs)) rest)
    (push `(:outputs . ,(union o outputs)) rest)
    rest))

(defun acab-sequence/get-depth-range (str)
  "Get the (non-inclusive) range of depths available for a value"
  (message "Get Depth Range")
  (if (acab-sequence/is-terminal-p str)
      '(1 . 1)
    (let* ((depth-hash (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
           (value-hash (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
           (obj (gethash str value-hash))
           (lookup-f (lambda (xs default)
                       (if xs
                           (mapcar (lambda (x) (if (acab-sequence/is-terminal-p x) default (gethash x depth-hash default))) xs)
                         `(,default)))))
      (if obj
          ;; exists, so get range
          (let ((max-input (-max (apply lookup-f `(,(acab-sequence/get obj :inputs) 1))))
                (min-output (-min (apply lookup-f `(,(acab-sequence/get obj :outputs) 100)))))
            `(,max-input . ,min-output))
        ;; doesn't exist, so default range:
        `(1 . 100)))))
(defun acab-sequence/kahnsort ()
  " Run Kahnsort on the graph to determine depths "
  (let* ((value-hash (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
         (terminals (acab-sequence/get-table-prop :acab-sequence/terminals))
         (inputless (acab-sequence/inputless))
         (result (kahnsort value-hash (union terminals inputless) terminals))
         ;; having sorted, get components of result:
         (sorted (car result))
         (active (cadr result))
         (undiscovered (caddr result))
         (max_depth (max (acab-sequence/get-table-prop :acab-sequence/max-depth) (-max (mapcar (lambda (x) (cdr x)) sorted)))))
    ;; add / remove columns as necessary
    ;; update columns
    ;; (debug)
    ))

;;--------------------
;;Drawing
;;--------------------
(defun acab-sequence/update-column (col vals)
  " Given a column and a list of values, set that column "
  ;;goto start column, row 1
  (save-excursion
    (goto-char (org-table-begin))
    (while (and (org-at-table-p) (not (org-at-table-hline-p)))
      (forward-line))
    (forward-line)
    ;; down below the first hline
    ;;loop over values, inserting them
    (loop for x in vals do
          ;; extend the table if necessary
          (if (not (org-at-table-p))
              (progn (forward-line -1)
                     (org-table-insert-row 1)))
          ;; insert the value
          (org-table-put (org-table-current-dline) col x)
          (forward-line)
          ;; skip over hlines
          (if (org-at-table-hline-p)
              (forward-line)))
    ;; clear obsolete values
    (while (org-at-table-p)
      (if (org-at-table-hline-p)
          (forward-line)
        (progn (org-table-goto-column col)
               (org-table-blank-field)
               (forward-line)
               )))
    (org-table-align)))
(defun acab-sequence/redraw-entire-table ()
  " Redraw the entire table"
  (message "Redrawing")
  (let ((cols org-table-current-ncol)
        (needed (acab-sequence/get-table-prop :acab-sequence/max-depth))
        (depth-sets (acab-sequence/get-table-prop :acab-sequence/depth-sets))
        (count 0))
    ;; Add more columns
    (while (< cols needed)
      (org-table-insert-column)
      (cl-incf cols)
      )

    (while (< count cols)
      (message "Updating column: %s" count)
      (if (and (< 1 count) (< count cols))
          (org-table-put 1 count (format "Depth: %s" count)))
      (acab-sequence/update-column (+ count 1) (aref depth-sets count))
      (cl-incf count))
    (acab-sequence/update-column cols (aref depth-sets 0))
    ))
(defun acab-sequence/redraw-inspector ()
  (let ((action '(display-buffer-in-side-window (side . left)))
        (value-hash (acab-sequence/get-table-prop :acab-sequence/value-hashmap))
        (depth-hash (acab-sequence/get-table-prop :acab-sequence/depth-hashmap))
        (terminals (seq-copy (acab-sequence/get-table-prop :acab-sequence/terminals)))
        )
    (with-temp-buffer-window acab-sequence/info-tab action nil
                             (princ "* Sequence Information\n")
                             (if (hash-table-p value-hash)
                                 (progn
                                   (princ "** Values: \n")
                                   (mapc (lambda (k) (princ (format "*** %s :\n\tInputs: %s\n\tOutputs: %s\n\n" k
                                                                    (acab-sequence/get (gethash k value-hash) :inputs)
                                                                    (acab-sequence/get (gethash k value-hash) :outputs))))
                                         (sort (hash-table-keys value-hash) (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             (if (hash-table-p depth-hash)
                                 (progn
                                   (princ "** Depths: \n")
                                   (mapc (lambda (k) (princ (format "   %s : %s\n" k (gethash k depth-hash))))
                                         (sort (hash-table-keys depth-hash) (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             (if terminals
                                 (progn
                                   (princ "\n** Terminals: \n")
                                   (mapc (lambda (k) (princ (format "   %s\n" k))) (sort terminals (lambda (x y) (string-lessp (downcase x) (downcase y)))))))
                             )
    (with-current-buffer acab-sequence/info-tab
      (org-mode)
      (org-show-all)
      )
    ))
(defun acab-sequence/inspect-table ()
  " Open the right tab buffer, displaying details of the selected field "
  ;; create a tab that updates with details
  ;; on current field
  (interactive)
  (let ((wind (get-buffer-window acab-sequence/info-tab)))
    (if wind
        ;; if exists, lose it
        (delete-window wind)
      ;;otherwise create it
      (acab-sequence/redraw-inspector))))

(defun acab-sequence/highlight ()
  ;; choose colour for inputs / outputs

  ;; get inputs

  ;; get outputs

  ;; find them

  ;; overlay them

  )
;;--------------------
;;Table Operations
;;--------------------
(defun acab-sequence/new-table ()
  " Create a new table, after having moved to empty space "
  (interactive)
  (if (org-at-table-p)
      (progn (goto-char (org-table-end))
             (insert "\n")))
  ;; add default table
  (let ((input (propertize " Input Terminals " :acab-sequence/terminal t))
        (output (propertize " Output Terminals " :acab-sequence/terminal t)))
    (insert "\n")
    (beginning-of-line)
    (insert (format "    |%s| Non-Terminals |%s|\n" input output))
    (insert "    |-----------------+---------------+------------------|\n")
    (insert "    |                 |               |                  |\n")
    )
  ;; move point to start of table
  (forward-line -1)
  (goto-char (org-table-begin))
  (acab-sequence/goto-position 2 2)

  (put-text-property (org-table-begin) (org-table-end) :acab-sequence/terminals '())
  (put-text-property (org-table-begin) (org-table-end) :acab-sequence/depth-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :acab-sequence/max-depth 3)
  (put-text-property (org-table-begin) (org-table-end) :acab-sequence/value-hashmap (make-hash-table :test 'equal))
  (put-text-property (org-table-begin) (org-table-end) :acab-sequence/depth-sets '())
  )
(defun acab-sequence/insert-rule (input)
  " Insert an LHS, RHS and optional rule name into the graph "
  (interactive "M")
  (message "Inserting rule")
  (let ((row (org-table-current-line))
        (col (org-table-current-column))
        (parts (split-string input " " 't))
        (on_lhs 't)
        (lhs '())
        (rhs '()))
    (while parts
      (let ((curr (pop parts)))
        (cond ((equal curr acab-sequence/rule-divider) (setq on_lhs nil))
              (on_lhs (push curr lhs))
              ('t (push curr rhs)))))

    ;; have got lhs and rhs, add them
    (loop for x in lhs do
          (acab-sequence/insert-string x nil rhs))
    (loop for x in rhs do
          (acab-sequence/insert-string x lhs nil))
    (acab-sequence/analyze-table)
    ;; update table
    (acab-sequence/redraw-entire-table)
    ;; (goto-char (org-table-begin))
    (acab-sequence/goto-position row col)
    (if (get-buffer-window acab-sequence/info-tab)
        (acab-sequence/redraw-inspector))
    ))
(defun acab-sequence/insert-terminal (term)
  " Insert a terminal value into the graph "
  ;; get value
  (interactive "M")
  (let ((row (org-table-current-line))
        (col (org-table-current-column)))
    ;; insert into data
    (acab-sequence/insert-string term '() '() t)
    ;; insert into table
    (acab-sequence/analyze-table)
    (acab-sequence/update-column 1 (acab-sequence/get-table-prop :acab-sequence/terminals))
    (acab-sequence/update-column org-table-current-ncol (acab-sequence/get-table-prop :acab-sequence/terminals))
    ;; (goto-char (org-table-begin))
    (acab-sequence/goto-position row col)
    (if (get-buffer-window acab-sequence/info-tab)
        (acab-sequence/redraw-inspector))
    )
  )

(defun acab-sequence/delete-value (&optional deleting-column)
  " Delete a field from the graph and table "
  (interactive)
  (let* ((curr-line (org-table-current-line))
         (curr-col (org-table-current-column))
         (curr-value (org-table-get curr-line curr-col))
         )
    ;; remove it from the graph
    ;;TODO: remove from inputs and outputs
    (acab-sequence/remove-string curr-value)

    ;; update table
    (org-table-put curr-line curr-col "" t)
    (acab-sequence/goto-position curr-line curr-col)
    (if (and (not deleting-column) (get-buffer-window acab-sequence/info-tab))
        (acab-sequence/redraw-inspector)
        )
    )
  )
(defun acab-sequence/delete-column ()
  " Delete the current column "
  (interactive)
  (let ((curr-column (org-table-current-column)))
    (org-table-goto-line 2)
    (org-table-goto-column curr-column)
    (while (org-at-table-p)
      (acab-sequence/delete-value t)
      (forward-line)
      (org-table-goto-column curr-column))
    (forward-line -1)
    (acab-sequence/goto-position 2 curr-column)
    (acab-sequence/redraw-inspector)
    )
  )

(defun acab-sequence/rename-column ()
  " Rename the current column. Persistently across columm additions and removals "
  (interactive)
  ;; get column data row 1
  (if (org-at-table-p)
      (let* ((curr-table-row (org-table-current-line))
             (curr-table-col (org-table-current-column))
             (curr-name (org-table-get 1 curr-table-col))
             (is-terminal (get-text-property 0 :acab-sequence/terminal curr-name))
             new-name
             )
        (if (not (or (string-empty-p curr-name) is-terminal))
            (progn (acab-sequence/goto-position 1 curr-table-col)
                   (setq new-name (read-string "New Column Name: "))
                   (org-table-put 1 curr-table-col new-name t)
                   (acab-sequence/goto-position curr-table-row curr-table-col)
                   )
          )
        )
    )
  )
(defun acab-sequence/merge-column ()
  " Merge the left and right connections of a column, removing the middle "
  ;;get current column

  ;;get values

  ;; connect inputs to outputs

  ;;remove values

  ;; update table

  )
(defun acab-sequence/sort-table ()
  " Sort each column of the table alphabetically or by usage count "
  (interactive)
  (let ((max-column org-table-current-ncol)
        (curr-table-column (org-table-current-column))
        (curr-table-line (org-table-current-line))
        (curr-col 1)
        (column-values (acab-sequence/get-table-prop :acab-sequence/depth-sets))
        (curr-count 0)
        curr-values
        )
    (acab-sequence/goto-position 2 1)
    (while (< (org-table-current-column) max-column)
      ;;Get the values
      (setq curr-values (sort (seq-copy (aref column-values curr-count))
                              (lambda (x y) (string-lessp (downcase x) (downcase y)))))
      ;;set the columns values
      (acab-sequence/update-column curr-col curr-values)
      (incf curr-col)
      (incf curr-count)
      (acab-sequence/goto-position 2 curr-col)
      )
    (setq curr-values (sort (seq-copy (aref column-values 0))
                            (lambda (x y) (string-lessp (downcase x) (downcase y)))))
    (acab-sequence/update-column (- curr-col 1) curr-values)
    (acab-sequence/goto-position curr-table-line curr-table-column)
    )
  )
;;--------------------
;;Tabs
;;--------------------
(defun acab-sequence/set-tab (col data)
  " Set the left or right table to data "
  ;; default to left
  ;; retrieve data
  ;; add it to a temporary buffer

  ;; open window on side

  )

(defvar-local acab-sequence-mode-map

  (let ((map (make-keymap)))
    (define-key map "TAB" 'org-table-next-field)
    (define-key map "RET" 'org-table-next-row)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  " The basic keymap of the acab-sequence mode. Separate from more complex spacemacs bindings "
  )

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode acab-sequence-mode fundamental-mode "Acab-Sequence Mode"
  "Major Mode for creating a acab-sequence of rules "
  (interactive)
  (kill-all-local-variables)
  ;; Set the Org Table minor mode
  (orgtbl-mode)
  ;; set table coordinates to show
  (use-local-map acab-sequence-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults) '(acab-sequence-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'acab-sequence-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-sequence-mode-syntax-table)
  (setq major-mode 'acab-sequence-mode
        mode-name "ACAB-SEQUENCE")
  ;;(run-mode-hooks 'acab-sequence-mode-hook)
  )

;; TODO: add to hs-special-modes-alist
;; TODO: mod after-change-functions
;; useful functions:
;; org-table-goto-field
