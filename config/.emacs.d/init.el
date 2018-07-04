;; init.el -*- lexical-binding: t -*-
;; Author: weirdNox (Gonçalo Santos)

(eval-when-compile (and (version< emacs-version "25.1")
                        (error "This config requires at least GNU Emacs 25.1, but you're running version %s."
                               emacs-version)))

(unless (boundp 'early-init-file) (load (concat (file-name-directory load-file-name) "early-init") nil t))

(let* ((org (expand-file-name "config.org" user-emacs-directory))
       (el  (expand-file-name "config.el" user-emacs-directory))
       (elc (concat el "c")))
  (when (and (file-exists-p elc)
             (file-newer-than-file-p org elc))
    (message "Byte compiled init is old - deleting...")
    (dolist (file (directory-files user-emacs-directory nil ".*\.elc"))
      (delete-file (expand-file-name file user-emacs-directory))))

  (cond ((file-exists-p elc) (load elc nil t))
        (t
         (message "Loading config.el...")
         (when (file-newer-than-file-p org el)
           (message "Tangling the literate config...")
           (unless (call-process "emacs" nil nil nil
                                 "-q" "--batch" "-l" "ob-tangle" "--eval"
                                 (format "(org-babel-tangle-file \"%s\" \"%s\" \"emacs-lisp\")" org el))
             (warn "There was a problem tangling the literate config")))
         (load el nil t))))
