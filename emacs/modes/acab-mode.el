;;based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'acab-faces)
(require 'acab-hydras)
(require 'inst-mode)
(require 'trie-explore-mode)
(require 'trie-log-mode)
(require 'trie-management)
(require 'trie-management)
(require 'trie-minor-mode)
(require 'trie-mode)
(require 'trie-passive-mode)
(require 'trie-sequence-mode)
(require 'trie-tree)

(provide 'acab-mode)

(defgroup acab-mode '() "Acab Mode Customizations"
  :group 'acab-ide)
;;--------------------
;; Mode Variables
;;--------------------
(defcustom acab-mode-hook nil "Basic Hook For Acab Mode" :type '(hook))
(defcustom acab-mode-indent-end-regexp "^[ \t]*end$" "Regexp for Acab mode indenting based on block endings" :type '(regexp))
(defcustom acab-mode-indent-begin-regexp "^.+:\\( (::.+?)\\)?$" "Regexp for Acab mode block start" :type '(regexp))

;;--------------------
;;Utilities
;;--------------------

;;--------------------
;;definitions
;;--------------------
(defconst acab-keywords '("assert" "retract" "end"))
;;generate regexp for keywords
(defconst acab-keywords-regexp (regexp-opt acab-keywords))

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar acab-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  "Keymap for Acab mode major mode")

;;--------------------
;;Keyword based syntax highlighting
;;Specify as a list of (matcher . facename )
;;Potential keywords: operators +-*/!.()""$~ < > != == -> @
;;--------------------
(defconst acab-font-lock-keywords
  (list
   ;;Rule name
   `("^\\([[:word:].!]+\\):$" (0 "acab-rulename"))
   ;;punctuation
   `("\\." (0 'acab-face-1))
   `("!" (0 'acab-face-2))
   `("::" (0 'acab-face-3 t))
   `("->\\|\\?" (0 'acab-face-0 t))
   ;;Variables and tags
   `("#[[:word:]]+" (0 'acab-face-1))
   `("\\$[[:word:]]+" (0 'acab-face-2))
   ;;functions
   `("\\([-<>=%^*@+&~][[:word:]]*\\)" (1 'acab-face-3))
   ;;Words
   `("[[:word:]]" (0 'acab-face-0))
   ;;Closures
   `("[][()]" (0 'acab-face-1))
   `("[([]\.+[])]" (0 'acab-face-2))
   )
  "Minimal highlighting expressions for Acab mode")

;;--------------------
;;Indentation
;; Potential indent points:
;; newline ending with an EXOP, comma,
;; reset indent if prior line is empty
;;
;;--------------------
(defun acab-indent-line()
  "Indent current-line as Acab code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ;;if at beginning of buffer
      ;;then:
      (indent-line-to 0)
    ;;else:
    (let ((not-indented t) cur-indent)
      (if (looking-at acab-mode-indent-end-regexp) ;;if line contains an END_
          (progn
            (save-excursion ;;save where we are
              (forward-line -1) ;;go back a line
              ;;then deindent:
              (setq cur-indent (- (current-indentation) tab-width))
              ;;guard:
              (if (< cur-indent 0)
                  (setq cur-indent 0))))
        ;;else:
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at acab-mode-indent-end-regexp)
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ;;otherwise
              (if (looking-at acab-mode-indent-begin-regexp)
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;;--------------------
;;Syntax Table
;;--------------------
;;Define the syntax table, for word definitions etc
;;modify-syntax-entry: character, class/flag, syntable)
;;classes/syntax flags: include 'w': word, '.':punctuation,
;; see: C-h ? r elisp manual syntax-tables
(defvar acab-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the acab-mode")

;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.rule\\'" . acab-mode))


(defun acab-syntactic-face-function (parse-state)
  'font-lock-comment-face
  )

;; --------------------
;;Entry Function
;;--------------------
(define-derived-mode acab-mode fundamental-mode "Acab Mode"
  "Major Mode for creating rules using Acab"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-mode-map)
  (let ((base-locks (reverse acab-font-lock-keywords))
        (keywords (list acab-keywords-regexp 0 "acab-ruleend" t)))
    (push keywords base-locks)
    (set (make-local-variable 'font-lock-defaults) (list (reverse base-locks) nil))
    )
  (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-syntactic-face-function)
  (set (make-local-variable 'indent-line-function) 'acab-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table acab-mode-syntax-table)
  (setq major-mode 'acab-mode)
  (setq mode-name "ACAB")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )
