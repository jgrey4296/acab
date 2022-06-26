;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; [[file:/Volumes/documents/github/emacs_files/lisp/comint-test.el][comint]]
;; adapted from https://masteringemacs.org/article/comint-writing-command-interpreter


(provide 'acab-comint)

;; For working with acab-py, through a comint

(setq acab-comint/acab-py-loc "/Volumes/documents/github/acab/")
(setq acab-comint/python-cmd "/Volumes/documents/github/acab/acab/modules/repl/repl_main.py")
(setq acab-comint/python-args '("--config" "/Volumes/documents/github/acab/acab/__configs/default"))
(defvar acab-comint/process nil)
(defvar acab-comint/cwd nil)
(defvar acab-comint/buffer-name "*Acab Comint*")
(defvar acab-comint/prompt-regexp "^\\(ACAB REPL: \\)")

(defun acab-comint/init ()
  " Startup the Acab Comint "
  (interactive)
  (let ((buffer (get-buffer-create acab-comint/buffer-name)))
    (unless (comint-check-proc buffer)
      (apply 'make-comint-in-buffer acab-comint/buffer-name buffer
             acab-comint/python-cmd nil acab-comint/python-args))
    (with-current-buffer buffer
      (acab-comint-mode))
    (setq acab-comint/process (get-buffer-process buffer))
    ;; (switch-to-buffer-other-window buffer)
    )
  )

;; --------- Python subprocess
(defun acab-comint/run-python-server (loc)
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test"
  (message "Initializing Python Server")
  ;;start python process
  (setq trie/python-process (make-process :name "Rule IDE Process"
                                          :buffer acab-comint/python-process-buffer-name
                                          :command (list acab-comint/process-python-command
                                                         acab-comint/process-python-args)
                                          :connection-type 'pipe
                                          :filter 'acab-comint/python-filter
                                          :sentinel 'acab-comint/python-sentinel
                                          )
        )
  ;;initialize
  (process-send-string trie/python-process (format "load {}\n" loc))
  ;;populate emacs side data with loaded+parsed info

  )
(defun acab-comint/python-filter (proc x)
  ;; TODO Filter to parse and handle python responses


  )
(defun acab-comint/python-sentinel (proc x)
  ;; TODO Sentinel to handle python state changes

  )

;; ---- Low Level
(defun acab-comint/send-input (proc x)
  ;; (message "Sending: %s" x)
  (comint-simple-send proc x)
  )
(defun acab-comint/get-output (x)
  ;; (message "Received: %s" x)
  x
  )

;; -- High Level Commands
(defun acab-comint/server-load (f)
  (interactive "f")
  (acab-comint/send-input acab-comint/process
                          (format "load %s" f))
  )
(defun acab-comint/server-save (f)
  (interactive "F")
  (acab-comint/send-input acab-comint/process
                          (format "save %s" f))
  )
(defun acab-comint/server-query ()
  (interactive)
  (acab-comint/send-input acab-comint/process "print wm")
  )
(defun acab-comint/server-inspect ()
  (acab-comint/send-input acab-comint/process "stat")
  )
(defun acab-comint/server-test ()
  ;; TODO python server test
  )
(defun acab-comint/trigger-tests ()
  " TODO Trigger a Bank of tests "
  (interactive)
  ;;with buffer rule logs
  ;;clear?
  ;;get tests for working group
  ;;run tests
  ;;print results


  )
(defun acab-comint/server-quit ()
  (interactive)
  (acab-comint/send-input acab-comint/process "exit")
  )

(define-derived-mode acab-comint-mode comint-mode "Acab-Comint"
  "Major Mode for Comint Interaction with Acab-Py"
  nil "Acab-Comint"
  ;;Setup:
  (setq comint-prompt-regexp acab-comint/prompt-regexp)
  (setq comint-prompt-read-only t)
  ;; (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) acab-comint/prompt-regexp)

  ;; (set (make-local-variable 'font-lock-defaults) '(my-comint-font-lock-keywords t))

  ;; Set up transforms:
  ;; (setq-local comint-input-filter-functions '(nil))
  (setq-local comint-input-sender 'acab-comint/send-input)
  (add-hook 'comint-preoutput-filter-functions 'acab-comint/get-output nil t)
  )

;; this has to be done in a hook. grumble grumble.
(defun acab-comint/init-hook ()
  "Helper function to initialize My-Comint"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  )
(add-hook 'acab-comint-mode-hook 'acab-comint/init-hook)
