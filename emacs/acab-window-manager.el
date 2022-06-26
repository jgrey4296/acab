;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(provide 'acab-window-manager)
;; TODO control the acab ide default views
(defvar acab-ide/window-configuration nil)
(setq-default acab-ide/inputs-buffer-name         "*Rule Inputs*"
              acab-ide/outputs-buffer-name        "*Rule Outputs*"
              acab-ide/working-group-buffer-name  "*Rule Working Group*"
              acab-ide/logging-buffer-name        "*Rule Logs*"
              acab-ide/python-process-buffer-name "*Rule IDE*"

              acab-ide/working-group-buffer-headings '("Defeaters"
                                                       "Interferers"
                                                       "Alternatives"
                                                       "Equal Depth"
                                                       "Relevant Types"
                                                       "Meta"
                                                       "Layer Stats"
                                                       "Tests")
)

(defun acab-wm/init ()
  (window--display-buffer (find-file (f-join location acab-ide/ide-pipeline-spec-buffer)) (plist-get windows :miscL) 'window)
  (window--display-buffer (generate-new-buffer "rule_stub")  (plist-get windows :rule) 'window)
  (window--display-buffer (get-buffer-create acab-ide/inputs-buffer-name)  (plist-get windows :prior)'window)
  (window--display-buffer (get-buffer-create acab-ide/outputs-buffer-name)  (plist-get windows :post) 'window)
  (window--display-buffer (get-buffer-create acab-ide/logging-buffer-name)  (plist-get windows :miscR) 'window)
  (window--display-buffer (get-buffer-create acab-ide/working-group-buffer-name)  (plist-get windows :miscC) 'window)
  )
(defun acab-wm/reset ()

  )
(defun acab-wm/popup ()

  )

;;Window setup
(defun acab-ide/reset-windows ()
  (interactive)
  (if (and (acab-ide/trie-ide-running-p) (window-configuration-p acab-ide/window-configuration))
      (progn (setq acab-ide/window-configuration (acab-ide/build-ide-window-layout))
             (acab-ide/init-ide-buffers-contents location inst-name acab-ide/window-configuration)
             )
    )
  )
(cl-defun acab-ide/build-ide-window-layout ()
  " Setup rule editing windows "
  ;; (terminals - ) priors - rule - posts (terminals)
  ;;                       defeaters
  ;;       upstream stats  - alts - downstream stats
  (interactive)
  (let (prior post rule miscL miscC miscR)
    (delete-other-windows)
    ;; split in half
    (setq prior (selected-window))
    (setq miscL (split-window-below))
    ;;Top half:
    ;; Split into three: priors, rule, posts
    (setq rule (split-window-right))
    (select-window rule)
    (setq post (split-window-right))
    ;;Bottom Half
    ;; Split into three: upstream, alts, downstream
    (select-window miscL)
    (setq miscC (select-window (split-window-right)))
    (setq miscR (split-window-right))

    (list :prior prior :post post :rule rule :miscL miscL :miscC miscC :miscR miscR)
    )
  )
(defun acab-ide/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )

(defun acab-ide/redisplay-io-window (side content)
  ;;delete all content in buffer
  ;;insert content
  ;;TODO redisplay io window
  )

(defun acab-ide/show-side-buffer ()
  ;;TODO show side buffer
  )
(defun acab-ide/write-io-info-buffer (data target)
  "Insert new data into IO buffers"
  (with-current-buffer (cond ((eq :input target)
                              acab-ide/inputs-buffer-name)
                             ((eq :output target)
                              acab-ide/outputs-buffer-name)
                             (t nil))
    ;;Clear
    (jg_layer/clear-buffer)
    ;;Insert header + layer
    (insert (format "* Available %s:\n" (cond ((eq :input target)
                                               "Inputs")
                                              ((eq :output target)
                                               "Outputs"))))

    ;;Insert data strings
    (loop for x in (plist-get data :list) do
          (insert (format "  %s\n" x)) ;;Maybe propertize
          )
    )
  )

(defun acab-ide/init-ide-buffers-contents (location inst-name windows)
  ;;create inst stub
  (if (not (f-exists? (f-join location acab-ide/ide-pipeline-spec-buffer)))
      (progn
        (with-current-buffer (get-buffer-create acab-ide/ide-pipeline-spec-buffer)
          ;; insert default institution contents
          (acab-rule-mode)
          (org-mode)
          (yas-expand-snippet (yas-lookup-snippet "pipeline" 'trie-mode))
          (write-file (f-join location acab-ide/ide-pipeline-spec-buffer))
          )
        )
    )


  (acab-ide/build-working-group-buffer)
  (with-current-buffer "rule_stub"
    (acab-rule-mode)
    (yas-expand-snippet (yas-lookup-snippet "rule" 'trie-mode) (point-min))
    )

  (with-current-buffer acab-ide/inputs-buffer-name
    (insert "AVAILABLE INPUTS Layer 0:\n--------------------\n\n")
    )
  (with-current-buffer acab-ide/outputs-buffer-name
    (insert "AVAILABLE OUTPUTS Layer 1:\n--------------------\n\n")
    )
  (with-current-buffer acab-ide/logging-buffer-name
    (insert "LOGGING:\n--------------------\n\n")
    )
  )
(defun acab-ide/build-working-group-buffer ()
  (with-current-buffer acab-ide/working-group-buffer-name
    (org-mode)
    (insert "* Working Group\n")
    (mapc (lambda (x) (insert "** " x ":\n")) acab-ide/working-group-buffer-headings)
    )
  )

;;UPDATE
(defun acab-ide/update-buffer-contents ()
  (interactive)
  ;;TODO update buffer contents
  )
(defun acab-ide/sort-conditions-and-actions ()
  ;;TODO sort conditions and actions
  )

(defun acab-ide/show-analysis-results ()
  ;;TODO show analysis results
  )
