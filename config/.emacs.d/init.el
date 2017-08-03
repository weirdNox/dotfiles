;; Author: weirdNox (Gonçalo Santos)

;; ------------------------------
;; Core
(when (version< emacs-version "25.1")
  (error "This config requires at least GNU Emacs 25.1, but you're running version %s."
         emacs-version))

(setq gc-cons-threshold 100000000)
(run-with-idle-timer 5 nil (lambda () (setq gc-cons-threshold 1000000)))

(setq-default default-directory "~/"
              temp-dir (locate-user-emacs-file "temp")
              custom-file (locate-user-emacs-file "custom.el")
              trash-directory (locate-user-emacs-file "trash"))

(require 'package)
(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq-default use-package-always-defer t)


;; ------------------------------
;; Appearance
(use-package color-theme-sanityinc-tomorrow :ensure t
  :init (load-theme 'sanityinc-tomorrow-night t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-hook 'after-init-hook (lambda ()
                             (when (> (display-pixel-width) 1500)
                               (split-window-horizontally))))

(cond
 ((member "DejaVu Sans Mono" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11")))
 ((member "Source Code Pro" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))))

(setq-default inhibit-startup-screen t
              initial-scratch-message "")

(setq-default truncate-partial-width-windows 70
              word-wrap t)
(add-hook 'prog-mode-hook (lambda () (setq-default truncate-lines t)))

(global-hl-line-mode 1)
(blink-cursor-mode 0)

(setq-default ring-bell-function 'ignore)

;; Mode line
(line-number-mode t)
(column-number-mode t)
(display-time)
(setq-default display-time-24hr-format t)


;; ------------------------------
;; Behaviour
(setq-default
 indent-tabs-mode nil
 tab-width 4

 fill-column 90

 require-final-newline t
 sentence-end-double-space nil

 scroll-margin 1
 scroll-conservatively 101
 scroll-preserve-screen-position t
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 mouse-wheel-progressive-speed nil

 kill-ring-max 5000
 undo-limit (* 20 1024 1024)
 undo-strong-limit (* 40 1024 1024)
 undo-outer-limit (* 100 1024 1024)
 mark-ring-max 5000
 global-mark-ring-max 5000
 large-file-warning-threshold (* 100 1024 1024)

 enable-recursive-minibuffers t

 save-interprogram-paste-before-kill t

 backup-directory-alist `((".*" . ,temp-dir))
 auto-save-file-name-transforms `((".*" ,temp-dir t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t

 load-prefer-newer t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(global-auto-revert-mode)

(defun nox/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun nox/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun nox/previous-blank-line ()
  "Move point to the previous blank line"
  (interactive)
  (move-end-of-line nil)
  (if (search-backward-regexp "^[\t ]*\n[\t ]*[^\t\n ]+" nil "NOERROR") nil
    (goto-char (point-min))))

(defun nox/next-blank-line ()
  "Move point to the next blank line"
  (interactive)
  (move-beginning-of-line nil)
  (if (not (search-forward-regexp "[^\t\n ]\n[\t ]*$" nil "NOERROR"))
      (goto-char (point-max))))

(defun nox/open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line 0)
  (newline-and-indent))

(defun nox/open-line-below ()
  "Insert an empty line below the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; ------------------------------
;; Keybindings
(use-package hydra :ensure t)
(use-package key-chord :ensure t
  :init
  (key-chord-mode 1)
  (setq-default key-chord-two-keys-delay 0.035
                key-chord-one-key-delay 0)
  (use-package use-package-chords :ensure t :demand))

(bind-keys
 ("<C-return>" . nox/open-line-below)
 ("<C-M-return>" . nox/open-line-above)
 ("<backtab>" . indent-for-tab-command)
 ("<C-tab>" . indent-region)
 ("M-o" . other-window))

(bind-chords
 (" f" . find-file)
 (" o" . ff-find-other-file)
 (" s" . save-buffer)
 (" b" . switch-to-buffer)
 (" k" . kill-this-buffer))

(defhydra hydra-files (:exit t)
  "Files"
  ("f" find-file "Open")
  ("s" save-buffer "Save")
  ("r" nox/rename-file-and-buffer "Rename current")
  ("k" nox/delete-file-and-buffer "Delete current")
  ("o" ff-find-other-file "Switch header/source")
  ("b" hexl-find-file "Open binary")
  ("l" find-file-literally "Open literally")
  ("q" nil "Quit"))
(key-chord-define-global "qf" 'hydra-files/body)

(defhydra hydra-error ()
  "Errors"
  ("f" first-error "First")
  ("n" next-error "Next")
  ("p" previous-error "Previous")
  ("q" nil "Quit"))
(key-chord-define-global "cn" 'hydra-error/next-error)


;; ------------------------------
;; Packages
(use-package anzu :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode))

(use-package avy :ensure t
  :chords (" j" . avy-goto-char))

(use-package company :ensure t
  :diminish company-mode
  :bind
  (:map company-mode-map
        ("<tab>" . company-complete-common))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle))
  :bind
  (:map company-template-nav-map
        ("<tab>" . company-complete-common)
        ("<C-return>" . company-template-forward-field))

  :init
  (global-company-mode)
  (setq-default company-require-match nil
                company-idle-delay nil))

(use-package compile
  :chords (" c" . nox/make)
  :config
  (defvar-local nox/build-script-names nil
    "Name of the build file to run when compile is called.")

  (defvar nox/compile-should-close nil
    "Set to t if it should close window when there are no errors.")

  (defun nox/make ()
    "Run build script."
    (interactive)
    (let ((done nil))
      (dolist (build-script-name nox/build-script-names)
        (if done nil
          (let ((script-dir (locate-dominating-file default-directory build-script-name))
                (source-path (concat "\"" (buffer-file-name) "\"")))
            (when script-dir
              (if (eq (count-windows) 1)
                  (setq nox/compile-should-close t)
                (if (not (get-buffer-window (get-buffer "*compilation*")))
                    (setq nox/compile-should-close nil)))

              (if (get-buffer "*compilation*")
                  (kill-buffer "*compilation*"))

              (switch-to-buffer-other-window "*compilation*")
              (cd script-dir)
              (let ((compilation-command (concat "\"" (expand-file-name default-directory)
                                                 build-script-name "\" " source-path)))
                (compile compilation-command))
              (other-window 1)
              (setq done t)))))))

  (defun nox/compilation-went-ok ()
    (and (save-excursion (not (ignore-errors (compilation-next-error 1 nil 1))))
         (with-current-buffer "*compilation*"
           (goto-char (point-min))
           (search-forward "finished" nil t))))

  (setq-default compilation-ask-about-save nil
                compilation-always-kill t
                compilation-context-lines 0
                compilation-environment '("TERM=xterm"))

  (add-to-list 'compilation-error-regexp-alist 'nox/devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(nox/devenv
                                                       "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
                                                       2 3 nil (4)))

  (if (eq system-type 'windows-nt)
      (setq-default nox/build-script-names '("build-nox.bat" "build.bat"))
    (setq-default nox/build-script-names '("build-nox.sh" "build.sh")))

  (defun nox/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings"
    (if (nox/compilation-went-ok)
        (progn
          (bury-buffer buffer)
          (if nox/compile-should-close
              (delete-window (get-buffer-window buffer))
            (switch-to-prev-buffer (get-buffer-window buffer) t)))))
  (add-hook 'compilation-finish-functions 'nox/bury-compile-buffer-if-successful)

  (require 'ansi-color)
  (defun nox/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'nox/colorize-compilation-buffer))

(use-package counsel :ensure t
  :diminish ivy-mode
  :diminish counsel-mode
  :bind (:map ivy-minibuffer-map
              ("<return>" . ivy-alt-done)
              ("C-j" . ivy-done))
  :bind (:map read-expression-map ("C-r" . counsel-expression-history))

  :init
  (use-package flx :ensure t)

  (ivy-mode 1)
  (counsel-mode 1)
  (setq-default ivy-use-virtual-buffers t
                ivy-height 10
                ivy-count-format "(%d/%d) "
                ivy-extra-directories nil
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  (defun counsel-find-file-as-root (x)
    "Find file X with root privileges."
    (counsel-require-program counsel-root-command)
    (let* ((method (file-remote-p x 'method))
           (user (file-remote-p x 'user))
           (host (file-remote-p x 'host))
           (file-name (concat (if (string= method "ssh")
                                  (format "/ssh:%s%s|"
                                          (if user (format "%s@" user) "")
                                          host)
                                "/")
                              (format "%s:%s:%s"
                                      counsel-root-command
                                      (or host "")
                                      (expand-file-name
                                       (if host
                                           (file-remote-p x 'localname)
                                         x))))))
      (if (eq (current-buffer) (get-file-buffer x))
          (find-alternate-file file-name)
        (find-file file-name)))))

(use-package ivy-bibtex :ensure t
  :config
  (setq-default bibtex-completion-bibliography (car org-ref-default-bibliography)
                bibtex-completion-notes-path org-ref-bibliography-notes
                bibtex-completion-library-path org-ref-pdf-directory))

(use-package dired
  :config
  (setq-default dired-listing-switches "-alh"
                dired-recursive-deletes 'always
                dired-recursive-copies 'always
                delete-by-moving-to-trash t))

(use-package find-file
  :config (setq-default ff-always-try-to-create t))

(use-package font-lock
  :config
  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-important-face)
  (make-face 'font-lock-note-face)

  (modify-face 'font-lock-todo-face "Red" nil nil t nil t nil nil)
  (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-note-face "Olive Drab" nil nil t nil t nil nil)

  (add-hook
   'prog-mode-hook
   (lambda ()
     (font-lock-add-keywords
      nil
      '(("\\<\\(TODO\\|FIXME\\)" 1 'font-lock-todo-face t)
        ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
        ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))))

(use-package gdb-mi
  :config
  (setq-default gdb-many-windows t
                gdb-show-main t))

(use-package imenu
  :config
  (set 'imenu-auto-rescan-maxout 500000)
  (set 'imenu-auto-rescan t))

(use-package imenu-anywhere :ensure t
  :chords (" i" . nox/ivy-imenu-center)
  :config
  (defun nox/ivy-imenu-center ()
    (interactive)
    (call-interactively 'ivy-imenu-anywhere)
    (recenter-top-bottom)))

(use-package interleave :ensure t)

(use-package magit :ensure t
  :if (executable-find "git")
  :chords (" g" . magit-status))

(use-package org :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode)
                             (company-mode 0))))

(use-package org-ref :ensure t
  :init
  (setq-default org-ref-completion-library 'org-ref-ivy-cite
                reftex-default-bibliography '("~/Documents/Bibliography/References.bib")
                org-ref-default-bibliography reftex-default-bibliography
                org-ref-bibliography-notes "~/Documents/Bibliography/Notes.org"
                org-ref-pdf-directory "~/Documents/Bibliography/PDFs/"))

(use-package pdf-tools :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config (pdf-tools-install))

(use-package paren
  :init
  (show-paren-mode)
  (setq-default show-paren-delay 0))

(use-package recentf
  :init
  (recentf-mode 1)
  (setq-default recentf-auto-cleanup 'never
                recentf-max-saved-items 150
                recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG")))

(use-package tramp
  :config
  (setq-default tramp-default-method "ssh")
  (add-to-list 'tramp-default-proxies-alist
               '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
               '((regexp-quote (system-name)) nil nil)))

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-after-kill-buffer-p t))


;; ------------------------------
;; MODES
(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))

  :config
  (defconst nox/c-style
    '((c-tab-always-indent . t)
      (c-comment-only-line-offset . 0)

      (c-hanging-braces-alist . ((class-open)
                                 (class-close)
                                 (defun-open)
                                 (defun-close)
                                 (inline-open)
                                 (inline-close)
                                 (brace-list-open)
                                 (brace-list-close)
                                 (brace-list-intro)
                                 (brace-list-entry)
                                 (block-open)
                                 (block-close)
                                 (substatement-open)
                                 (statement-case-open)
                                 (class-open)))

      (c-hanging-colons-alist . ((inher-intro)
                                 (case-label)
                                 (label)
                                 (access-label)
                                 (access-key)
                                 (member-init-intro)))
      (c-cleanup-list . (scope-operator
                         list-close-comma
                         defun-close-semi))

      (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                          (label . -4)
                          (access-label . -4)
                          (substatement-open . 0)
                          (statement-case-intro . 4)
                          (case-label . 4)
                          (block-open . 0)
                          (inline-open . 0)
                          (topmost-intro-cont . 0)
                          (knr-argdecl-intro . -4)
                          (brace-list-open . 0)
                          (brace-list-intro . 4)
                          (member-init-intro . ++)))

      (c-echo-syntactic-information-p . t)))

  (defun nox/header-format ()
    (interactive)
    (let ((definition (concat
                       (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                       "_H")))
      (insert (format "#if !defined(%s)\n#define %s\n\n\n\n#endif // %s" definition definition definition))
      (previous-line 2)))

  (defun nox/c-hook ()
    (c-add-style "NoxStyle" nox/c-style t)
    (c-toggle-auto-hungry-state -1)
    (if buffer-file-name
        (cond ((file-exists-p buffer-file-name) t)
              ((string-match "[.]h" buffer-file-name) (nox/header-format)))))

  (add-hook 'c-mode-common-hook 'nox/c-hook)
  (setq-default c-hanging-semi&comma-criteria '((lambda () 'stop))))
