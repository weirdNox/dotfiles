;; init.el -*- lexical-binding: t -*-
;; Author: weirdNox (Gonçalo Santos)

(let* ((org (expand-file-name "config.org" user-emacs-directory))
       (el  (expand-file-name "config.el" user-emacs-directory)))
  (when (file-newer-than-file-p org el)
    (message "Tangling the literate config...")
    (unless (call-process "emacs" nil nil nil "--batch" "-l" "ob-tangle" "--eval"
                          (format "(org-babel-tangle-file \"%s\" \"%s\" \"emacs-lisp\")" org el))
      (warn "There was a problem tangling the literate config")))

  (load el nil t))
