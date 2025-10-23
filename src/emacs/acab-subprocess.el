;;; acab-subprocess.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 13, 2022
;; Modified: July 13, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


;; Subprocess
(defvar acab-comint/python-proc-cmd "python3")
(defvar acab-comint/python-proc-args '("-m" "acab" "--config" "/Volumes/documents/github/acab/acab/__configs/default"))

(defvar trie/python-process nil)

(defvar acab-comint/python-process-buffer-name "*AcabPythonProc*")

;; --------- Python subprocess
(defun acab-comint/run-python-server (loc)
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test"
  (message "Initializing Python Server")
  ;;start python process
  (python-shell-with-environment
    (setq trie/python-process (make-process :name "Rule IDE Process"
                                            :buffer acab-comint/python-process-buffer-name
                                            :command (-concat (list acab-comint/python-proc-cmd)
                                                              acab-comint/python-proc-args)
                                            :connection-type 'pipe
                                            :filter 'acab-comint/python-filter
                                            :sentinel 'acab-comint/python-sentinel
                                            )
          )
    )
  ;;initialize
  ;; (process-send-string trie/python-process (format "load {}\n" loc))
  (process-send-string trie/python-process "a.b.c.d\n")
  ;;populate emacs side data with loaded+parsed info
  )

(defun acab-comint/python-filter (proc x)
  ;; TODO Filter to parse and handle python responses
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert x)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun acab-comint/python-sentinel (proc x)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert x)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(provide 'acab-subprocess)
;;; acab-subprocess.el ends here
