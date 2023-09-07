(require 'cl-lib)
(require 'project)
(require 'subr-x)

(defvar simple-jump-debug nil)
(defvar simple-jump-rg-command "rg")
(defvar simple-jump-family-alist nil)
(cl-defstruct simple-jump-family (base-regex "\\bJJJ\\b") regexes rg-types)

(defsubst simple-jump--populate-regex (regex look-for)
  (setq regex (string-replace "JJJ" (regexp-quote look-for) regex))
  (setq regex (string-replace "\\L" "[^\\S\\n\\r]" regex))
  (setq regex (string-replace "\\j" "[\\b-]"       regex)))

(defsubst simple-jump--populate-regexes (family look-for)
  (simple-jump--populate-regex (cl-loop for regex in (simple-jump-family-regexes family)
                                        collect regex  into regex-list
                                        finally return (string-join regex-list "|"))
                               look-for))

(defun simple-jump--sort-results (a b) (not (string> a b)))
(defun simple-jump--parse-raw (raw-results curr-file curr-line)
  (cl-loop for line in (sort (string-lines raw-results t nil) #'simple-jump--sort-results)
           for match = (string-match (rx bol (group (+ nonl)) ":" (group (+ num)) ":" (group (* nonl))) line)
           for filepath = (match-string 1 line)
           for line-num = (string-to-number (or (match-string 2 line) ""))
           for context  = (match-string 3 line)
           with prev = nil
           if (and match (not (or (and (string= filepath (car prev)) (= line-num (1+ (cdr prev))))
                                  (and (string= filepath curr-file)  (= line-num curr-line)))))
           collect (xref-make context (xref-make-file-location filepath line-num 0))
           do (setq prev (cons filepath line-num))))

(defun simple-jump--find (symbol &optional all-refs)
  (when-let* ((curr-file (expand-file-name (buffer-file-name)))
              (curr-line (line-number-at-pos))
              (search-from (project-root (or (project-current) (cons 'transient (expand-file-name "./")))))
              (family (alist-get major-mode simple-jump-family-alist))
              (base-regex (simple-jump--populate-regex (simple-jump-family-base-regex family) prompt))
              (base-args " -0l ")
              (search-regex (simple-jump--populate-regexes family prompt))
              (search-args " --color never --no-heading -PHUn ")
              (cmd (concat (concat simple-jump-rg-command (if all-refs search-args base-args)
                                   (shell-quote-argument base-regex) " " (shell-quote-argument search-from))
                           " | xargs -0 "
                           (unless all-refs (concat simple-jump-rg-command search-args (shell-quote-argument search-regex)))))
              (raw-results (shell-command-to-string cmd)))
    (let ((results (simple-jump--parse-raw raw-results curr-file curr-line)))
      (when simple-jump-debug
        (message "Command :: %s" cmd)
        (message "Raw out ::\n%s" raw-results)
        (message "Results :: %S" results))
      results)))

(cl-defmethod xref-backend-references  ((_backend (eql simple-jump)) prompt) (simple-jump--find prompt t))
(cl-defmethod xref-backend-definitions ((_backend (eql simple-jump)) prompt) (simple-jump--find prompt nil))
(cl-defmethod xref-backend-apropos     ((_backend (eql simple-jump)) prompt) (xref-backend-definitions 'simple-jump prompt))
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql simple-jump))) (thing-at-point 'symbol))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql simple-jump))))

(defun simple-jump-xref-backend () 'simple-jump)

(provide 'simple-jump)
