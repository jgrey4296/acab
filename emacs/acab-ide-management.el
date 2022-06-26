;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
(provide 'acab-ide-management)

(defvar acab-ide/is-running nil)
;;Registered databases for lookup and tracking
(defvar acab-ide/dialects '()
  "All defined sub-dsls")
(defvar acab-ide/current-priors '()
  "Available Priors for the current rule")
(defvar acab-ide/current-post '()
  "Available Posts for the current rule")

(defvar acab-ide/ide-data-loc nil
  "The working directory of acab")

(defvar acab-ide/rules nil
  "Defined rules")
(defvar acab-ide/types nil
  "Loaded and Defined types")
(defvar acab-ide/crosscuts nil
  "Loaded and defined crosscuts")
(defvar acab-ide/patterns nil
  "Loaded and defined patterns")
(defvar acab-ide/tests nil
  "Defined tests")

(defvar acab-ide/sentences nil)
(defvar acab-ide/tags nil
  "Tag references to other objects")
(defvar acab-ide/channels nil
  "Specified channels between layers")

(defconst acab-ide/data-loc-subdirs '("rules"
                                      "types"
                                      "crosscuts"
                                      "patterns"
                                      "tests"))


(defun acab-ide/init ()
  " Start the trie ide, setting up windows, etc "
  (interactive)
  (setq acab-ide/rules     (make-hash-table :test 'equal)
        acab-ide/types     (make-hash-table :test 'equal)
        acab-ide/crosscuts (make-hash-table :test 'equal)
        acab-ide/patterns  (make-hash-table :test 'equal)
        acab-ide/tests     (make-hash-table :test 'equal)
        acab-ide/tags      (make-hash-table :test 'equal)
        acab-ide/channels  (make-hash-table :test 'equal)

        acab-ide/current-priors '()
        acab-ide/current-post '()
        )

  ;; TODO add snippets to snippet loc

  ;; Get the directory to work with
  (let ((location (read-file-name "Institution Location:"))
        (windows (acab-ide/build-ide-window-layout))
        inst-name)

    ;;if a dir chosen, get a name for the
    ;;inst, create a stub, create a data dir
    (assert (f-exists? location))
    (cond ((f-dir? location)
             ;;get name for inst
             (setq inst-name (read-string "Institution Name: ")
                   acab-ide/ide-data-loc (f-join location (format "%s-data" inst-name)))
             )
          ((equal (f-ext location) "org")
           (setq inst-name (f-base location)
                 location (f-parent location)
                 acab-ide/ide-data-loc (f-join location (format "%s-data" inst-name))
                 )
           ))

    (setq acab-ide/ide-pipeline-spec-buffer (format "%s.org" inst-name))
    (acab-ide/maybe-build-data-loc)
    (acab-ide/init-ide-buffers-contents location inst-name windows)
    ;;Save the window configuration
    (setq acab-ide/window-configuration windows)

    ;; TODO possibly add file-notify watchers
    ;;start python server
    ;; (acab-ide/run-python-server location)
    ;; (acab-ide/load-directory-and-pipeline acab-ide/ide-data-loc)
    )
  (setq acab-ide/is-running t)
  )
(defun acab-ide/cleanup ()
  (interactive)
  ;;Clear windows, unload data
  (message "Shutting down Trie IDE")
  (acab-ide/dump-to-files)

  (if (and acab-ide/python-process
           (processp acab-ide/python-process)
           (process-live-p acab-ide/python-process))
      (progn
        (message "Closing Python Server")
        (quit-process acab-ide/python-process)
        (kill-buffer acab-ide/python-process-buffer-name)
        (setq acab-ide/python-process nil)
        )
    )
  (setq acab-ide/is-running nil)
  (assert (not acab-ide/is-running))
  )

;;Directory and buffer initialisation
(defun acab-ide/maybe-build-data-loc ( )
  (if (not (f-exists? acab-ide/ide-data-loc))
      (progn (mkdir acab-ide/ide-data-loc)
             (mapc (lambda (x) (mkdir (f-join acab-ide/ide-data-loc x)))
                   acab-ide/data-loc-subdirs)
             )
    )
  )
;;Loading and saving files
(defun acab-ide/load-directory-and-pipeline (loc)
  " Given a location, load into ide "
  ;;Initialise data
  ;; TODO load directory and setup pipeline
  ;;command python


  )
(defun acab-ide/dump-to-files ()
  (interactive)
  ;;Get all trie-* mode buffers, and the pipeline spec
  ;;and write to subdirs of acab-ide/ide-data-loc
  (let ((buffers (buffer-list))
        (curr-buff (current-buffer))
        (special-buffers (list acab-ide/inputs-buffer-name
                               acab-ide/outputs-buffer-name
                               acab-ide/working-group-buffer-name
                               acab-ide/logging-buffer-name))
        )
    (mapc (lambda (x)
            (cond ((and (buffer-file-name x) (f-ancestor-of? acab-ide/ide-data-loc (buffer-file-name x)))
                   (progn (save-buffer)
                          (if (not (equal curr-buff x))
                              (kill-buffer x))))
                  ((-contains? special-buffers (buffer-name x))
                   (kill-buffer x))))
          buffers)
    )
  )

(defun acab-ide/retrieve-data (type key)

  )
(defun acab-ide/insert-data (type data)

  )
(defun acab-ide/delete-data (type data)

  )

(defun acab-ide/analyse-data ()
  ;;TODO analyse data
  )
