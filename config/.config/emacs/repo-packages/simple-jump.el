(require 'cl-lib)
(require 'project)
(require 'subr-x)

(defvar simple-jump-debug nil)
(defvar simple-jump-rg-command "rg")
(defvar simple-jump-family-alist nil)
(cl-defstruct simple-jump-family (base-regex "\\bJJJ\\b") regexes rg-types)

(defsubst simple-jump--populate-regex (regex look-for)
  (setq regex (string-replace "JJJ" (regexp-quote look-for) regex))
  (setq regex (string-replace "\\L" "[\\t ]" regex))
  (setq regex (string-replace "\\j" "[\\b-]" regex)))

(defsubst simple-jump--populate-regexes (family look-for)
  (simple-jump--populate-regex (cl-loop for regex in (simple-jump-family-regexes family)
                                        collect regex  into regex-list
                                        finally return (string-join regex-list "|"))
                               look-for))

(defun simple-jump--sort-results (a b)
  (let ((afl (xref-item-location a))
        (bfl (xref-item-location b)))
    (or (string< (xref-file-location-file afl) (xref-file-location-file bfl))
        (and (string= (xref-file-location-file afl) (xref-file-location-file bfl))
             (<       (xref-file-location-line afl) (xref-file-location-line bfl))))))

(defun simple-jump--parse-raw (raw-results curr-file curr-line)
  (cl-loop with matcher = (rx bol (group (+ nonl)) ":" (group (+ num)) ":" (group (* nonl)))
           for line in (string-lines raw-results t nil)
           for match = (string-match matcher line)
           for result = (xref-make (match-string 3 line) (xref-make-file-location
                                                          (match-string 1 line)
                                                          (string-to-number (or (match-string 2 line) "")) 0))
           if match collect result into results
           finally return (sort results #'simple-jump--sort-results)))

(defun simple-jump--find (symbol &optional all-refs)
  (when-let* ((curr-file (expand-file-name (or (buffer-file-name) "./")))
              (curr-line (line-number-at-pos))
              (search-from (project-root (or (project-current) (cons 'transient (expand-file-name "./")))))
              (family (alist-get major-mode simple-jump-family-alist))
              (base-regex (simple-jump--populate-regex (simple-jump-family-base-regex family) prompt))
              (base-args " -0l ")
              (search-regex (simple-jump--populate-regexes family prompt))
              (search-args " --color never --no-heading -PHUn ")
              (allref-args " --color never --no-heading -Hn ")
              (cmd (concat (concat simple-jump-rg-command (if all-refs allref-args base-args)
                                   (shell-quote-argument base-regex) " " (shell-quote-argument search-from))
                           (unless all-refs (concat " | xargs -0 " simple-jump-rg-command
                                                    search-args (shell-quote-argument search-regex)))))
              (raw-results (shell-command-to-string cmd)))
    (let ((results (simple-jump--parse-raw raw-results curr-file curr-line)))
      (setq results (cl-loop for result in results
                             for  rfl = (xref-item-location result)
                             with pfl = (xref-make-file-location nil 0 0)
                             unless (or (and (string= (xref-file-location-file rfl)     (xref-file-location-file pfl))
                                             (=       (xref-file-location-line rfl) (1+ (xref-file-location-line pfl))))
                                        (and (string= (xref-file-location-file rfl) curr-file)
                                             (=       (xref-file-location-line rfl) curr-line)))
                             collect result
                             do (setq pfl rfl)))
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
