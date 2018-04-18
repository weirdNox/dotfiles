;; Author: weirdNox (Gonçalo Santos)               -*- lexical-binding: t -*-

;; ------------------------------
;; Core
(when (version< emacs-version "25.1")
  (error "This config requires at least GNU Emacs 25.1, but you're running version %s."
         emacs-version))

(defvar nox/file-name-handler-alist file-name-handler-alist)
(setq-default file-name-handler-alist nil
              gc-cons-threshold 402653184
              gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook (lambda () (setq-default file-name-handler-alist nox/file-name-handler-alist
                                                       gc-cons-threshold 16777216
                                                       gc-cons-percentage 0.1)))

(setq-default default-directory "~/"
              private-settings-file (locate-user-emacs-file "private.el")
              custom-file (locate-user-emacs-file "custom.el")
              temp-dir (locate-user-emacs-file "temp/"))

(require 'package)
(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ("org" . "https://orgmode.org/elpa/"))
              package-enable-at-startup nil
              load-prefer-newer t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq-default use-package-always-defer t)

(push (locate-user-emacs-file "lisp/") load-path)

;; ------------------------------
;; Appearance
(defvar nox/fonts '(("PragmataPro" . 12) ("Hack" . 11) ("DejaVu Sans Mono" . 11) ("Inconsolata" . 13)
                    ("Source Code Pro" . 11))
  "List of fonts and sizes. The first one available will be used.")

(defun nox/change-font ()
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font nox/fonts (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))

    (if (not available-fonts)
        (error "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
                                       available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))

      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting))

      (cond ((string= font-name "PragmataPro")
             (custom-theme-set-faces
              'user `(org-table ((t (:family ,(format "PragmataPro Mono-%d" font-size)))))))))))

(defvar nox/customize-theme-hook nil "Hook for theme customization, called with the theme name.")
(advice-add 'enable-theme :after
            (lambda (theme)
              (run-hook-with-args-until-success
               'nox/customize-theme-hook (or theme (car custom-enabled-themes)))))

(defmacro nox/add-customize-theme-hook (target-theme &rest body)
  "TARGET-THEME may be a list or a symbol."
  (declare (indent defun))
  `(add-hook 'nox/customize-theme-hook
             (lambda (theme)
               ,(if (symbolp (eval target-theme)) `(when (eq theme ,target-theme) ,@body)
                  `(when (memq theme ,target-theme) ,@body)))))

(defface font-lock-todo-face      '((t (:foreground "#dc322f" :weight bold :underline t))) "Face for TODO keywords.")
(defface font-lock-important-face '((t (:foreground "#b58900" :weight bold :underline t))) "Face for IMPORTANT keywords.")
(defface font-lock-note-face      '((t (:foreground "#228b22" :weight bold :underline t))) "Face for NOTE keywords.")
(add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords
                                      nil '(("\\<\\(TODO\\|FIXME\\)" 1 'font-lock-todo-face t)
                                            ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
                                            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)))))

(use-package color-theme-sanityinc-tomorrow :ensure
  :config
  (nox/add-customize-theme-hook '(sanityinc-tomorrow-blue sanityinc-tomorrow-eighties sanityinc-tomorrow-bright
                                                          sanityinc-tomorrow-night sanityinc-tomorrow-day)
    (custom-theme-set-faces
     theme
     `(org-special-keyword ((t (:inherit shadow)))))))

(use-package solarized :ensure solarized-theme
  :config
  (setq-default solarized-use-variable-pitch nil
                solarized-use-more-italic t
                solarized-high-contrast-mode-line nil
                solarized-scale-org-headlines nil)

  (nox/add-customize-theme-hook 'solarized-dark
    (solarized-with-color-variables 'dark
      (custom-theme-set-faces
       theme
       `(org-block
         ((t (:foreground ,(color-lighten-name base0 5) :background ,(color-lighten-name base03 5))))))))

  (nox/add-customize-theme-hook 'solarized-light
    (solarized-with-color-variables 'light
      (custom-theme-set-faces
       theme
       `(org-block
         ((t (:foreground ,(color-darken-name base0 7) :background ,(color-darken-name base03 7)))))))))

(use-package gruvbox-theme :ensure)

(use-package smart-mode-line :ensure
  :config
  (defvar nox/require-smart-mode-line-theme
    '((light . (sanityinc-tomorrow-day))
      (dark  . (sanityinc-tomorrow-blue sanityinc-tomorrow-eighties sanityinc-tomorrow-bright
                                        sanityinc-tomorrow-night))))

  (defun nox/update-smart-mode-line-theme (theme)
    (unless (memq theme '(smart-mode-line-light smart-mode-line-dark user))
      (let (chosen)
        (if (or (and (setq chosen 'smart-mode-line-light)
                     (memq theme (alist-get 'light nox/require-smart-mode-line-theme)))
                (and (setq chosen 'smart-mode-line-dark)
                     (memq theme (alist-get 'dark nox/require-smart-mode-line-theme))))
            (load-theme chosen)
          (disable-theme 'smart-mode-line-light)
          (disable-theme 'smart-mode-line-dark)))))
  (advice-add 'enable-theme :after 'nox/update-smart-mode-line-theme)

  (setq-default sml/position-percentage-format nil
                sml/pos-id-separator nil
                sml/use-projectile-p 'before-prefixes))

(setq-default initial-frame-alist '((fullscreen . fullboth)
                                    (fullscreen-restore . maximized))
              inhibit-startup-screen t
              initial-scratch-message ""

              ring-bell-function 'ignore

              x-underline-at-descent-line t
              custom-safe-themes t

              truncate-partial-width-windows 70
              word-wrap t)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(defun nox/setup-appearance (frame)
  (with-selected-frame frame
    (remove-hook 'after-make-frame-functions 'nox/setup-appearance)

    (when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
    (when (functionp 'tool-bar-mode) (tool-bar-mode -1))
    (when (functionp 'menu-bar-mode) (menu-bar-mode -1))

    (load-theme 'sanityinc-tomorrow-eighties t)
    (nox/change-font)

    (sml/setup)
    (line-number-mode -1)
    (display-time-mode)
    (display-battery-mode)

    (global-hl-line-mode)
    (blink-cursor-mode -1)

    (when (> (window-width) 100)
      (split-window-right))

    ;; NOTE(nox): This needs to be here, else it doesn't work
    (setq-default system-time-locale "C")))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'nox/setup-appearance)
  (nox/setup-appearance (car (frame-list))))


;; ------------------------------
;; Behavior
(setq-default
 indent-tabs-mode nil
 tab-width 4

 fill-column 90

 require-final-newline t
 mode-require-final-newline t
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
 bidi-display-reordering nil

 auto-window-vscroll nil ;; https://emacs.stackexchange.com/a/28746

 delete-by-moving-to-trash t
 backup-directory-alist `(("^" . ,temp-dir))
 auto-save-file-name-transforms `((".*" ,temp-dir t))
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 0
 version-control t)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default default-input-method "TeX")

(fset 'yes-or-no-p 'y-or-n-p)
(minibuffer-depth-indicate-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun nox/rename-file-and-buffer ()
  "Rename current buffer and the file it is visiting, if any."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t))
        (set-visited-file-name new-name t t)))))

(defun nox/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting, if any."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
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

(defun nox/pos-at-line (line &optional column)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (move-to-column (or column 0))
    (point)))

(defun nox/get-line-from-file (file line &optional trim)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (forward-line (- line 1))
      (let ((string (thing-at-point 'line)))
        (if trim
            (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" string)
          string)))))

(defun nox/get-entire-buffer (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun nox/exit-emacs (arg)
  "Exit Emacs, possibly killing the daemon and/or saving buffer.
When ARG is:
- nil or negative, it will kill the current terminal
- `universal-argument' or positive, it will kill the daemon
- a number, it will save all buffers automatically"
  (interactive "P")
  (when (or (numberp arg) (eq arg '-))
    (setq arg (prefix-numeric-value arg)))
  (let* ((save-without-asking (numberp arg))
         (kill-server (or (equal arg '(4))
                          (and save-without-asking
                               (>= arg 0)))))
    (if kill-server
        (save-buffers-kill-emacs save-without-asking)
      (save-buffers-kill-terminal save-without-asking))))


;; ------------------------------
;; Keybindings
(use-package hydra :ensure
  :config (setq-default lv-use-separator t))

(bind-keys
 ("<C-return>" . nox/open-line-below)
 ("<C-M-return>" . nox/open-line-above)
 ("<backtab>" . indent-for-tab-command)
 ("<C-tab>" . indent-region)
 ("M-o" . other-window)
 ("M-O" . (lambda () (interactive) (other-window -1)))
 ("C-x C-c" . nox/exit-emacs))

(defhydra hydra-files (:exit t :foreign-keys warn)
  "Files"
  ("f" find-file "Open")
  ("s" save-buffer "Save")
  ("r" nox/rename-file-and-buffer "Rename current")
  ("k" nox/delete-file-and-buffer "Delete current")
  ("o" ff-find-other-file "Switch header/source")
  ("b" hexl-find-file "Open binary")
  ("l" find-file-literally "Open literally")
  ("q" nil "Quit"))
(bind-key "C-c f" 'hydra-files/body)


;; ------------------------------
;; Packages
(use-package autorevert
  :demand
  :delight (auto-revert-mode)
  :config (global-auto-revert-mode))

(use-package avy :ensure
  :bind ("C-c a" . avy-goto-char))

(use-package calendar
  :config
  (setq-default calendar-week-start-day 1
                calendar-date-display-form calendar-european-date-display-form
                calendar-location-name "Porto"
                calendar-latitude  41.1579
                calendar-longitude -8.6291))

(use-package cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.ino\\'" . c++-mode))
  :config
  (defun nox/header-format ()
    (interactive)
    (let ((definition (concat
                       (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
                       "_H")))
      (insert (format "#if !defined(%s)\n#define %s\n\n\n\n#endif // %s" definition definition definition))
      (forward-line -2)))

  (defun nox/c-hook ()
    (c-add-style
     "NoxStyle"
     '((c-tab-always-indent . t)
       (c-comment-only-line-offset . 0)

       (c-hanging-braces-alist . ((class-open) (class-close) (defun-open) (defun-close)
                                  (inline-open) (inline-close) (brace-list-open) (brace-list-close)
                                  (brace-list-intro) (brace-list-entry) (block-open) (block-close)
                                  (substatement-open) (statement-case-open) (class-open)))

       (c-hanging-colons-alist . ((inher-intro) (case-label) (label) (access-label)
                                  (access-key) (member-init-intro)))

       (c-cleanup-list . (scope-operator list-close-comma defun-close-semi))

       (c-offsets-alist . ((arglist-close . c-lineup-arglist) (label . -4) (access-label . -4)
                           (substatement-open . 0) (statement-case-intro . 4) (case-label . 4)
                           (block-open . 0) (inline-open . 0) (topmost-intro-cont . 0)
                           (knr-argdecl-intro . -4) (brace-list-open . 0) (brace-list-intro . 4)
                           (member-init-intro . ++)))

       (c-echo-syntactic-information-p . t))
     t)
    (c-toggle-auto-hungry-state -1)
    (if buffer-file-name
        (cond ((file-exists-p buffer-file-name) t)
              ((string-match "[.]h" buffer-file-name) (nox/header-format)))))

  (add-hook 'c-mode-common-hook 'nox/c-hook)
  (setq-default c-hanging-semi&comma-criteria '((lambda () 'stop))))

(use-package cdlatex :ensure)

(use-package company :ensure
  :demand
  :delight
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
  :config
  (setq-default company-require-match nil
                company-idle-delay nil
                company-transformers '(company-sort-by-occurrence
                                       company-sort-by-backend-importance)
                company-backends '((company-capf
                                    company-files
                                    company-keywords
                                    company-gtags
                                    company-dabbrev-code)
                                   company-dabbrev))
  (global-company-mode))

(use-package company-dabbrev
  :after company
  :config
  (defun nox/company-dabbrev-buffer-check (buffer)
    (with-current-buffer buffer
      (or (derived-mode-p 'pdf-view-mode
                          'doc-view-mode))))

  (setq-default company-dabbrev-downcase nil
                company-dabbrev-ignore-case t
                company-dabbrev-ignore-invisible t
                company-dabbrev-ignore-buffers 'nox/company-dabbrev-buffer-check))

(use-package compile
  :bind (("<f12>" . nox/compile)
         ("M-g n" . hydra-error/next-error)
         ("M-g p" . hydra-error/previous-error))
  :config
  (defhydra hydra-error ()
    "Errors"
    ("f" first-error "First")
    ("n" next-error "Next")
    ("p" previous-error "Previous")
    ("q" nil "Quit"))

  (cl-defstruct nox/compile-info type path last-args last-time buffer-name should-close)
  (defvar-local nox/compile-info nil)
  (defvar nox/compile-info-table (make-hash-table :test 'equal))

  (defconst nox/compile-sh-names (if (eq system-type 'windows-nt)
                                     '("build-nox.bat" "build.bat")
                                   '("build-nox.sh" "build.sh")))
  (defconst nox/compile-makefile-names '("makefile" "Makefile"))
  (defconst nox/compile-script-names (append nox/compile-sh-names nox/compile-makefile-names))

  (defun nox/compile-buffer-name (arg)
    ;; NOTE(nox): This serves 2 purposes: one is creating the buffer name itself, called
    ;; from nox/make with arg = info; the other is returning the buffer name when called
    ;; from compilation-start, which is returned from the local info in the compilation
    ;; buffer.
    (if nox/compile-info (nox/compile-info-buffer-name nox/compile-info)
      (let ((project-name (projectile-project-name)))
        (if project-name
            (format "*[%s] - Compile %s*" project-name (file-name-nondirectory (nox/compile-info-path arg)))
          (format "*Compile %s*" (nox/compile-info-path arg))))))

  (defun nox/compile (arg)
    (interactive "P")
    (let ((default-script (when nox/compile-info (cons (nox/compile-info-path nox/compile-info)
                                                       nox/compile-info)))
          (start-file-name (or (buffer-file-name) ""))
          script script-list  buffer)
      (dolist (test-script nox/compile-script-names (setq script-list (nreverse script-list)))
        (let ((found-script-dir (locate-dominating-file default-directory test-script))
              full-path info)
          (when found-script-dir
            (setq full-path (expand-file-name test-script found-script-dir)
                  info
                  (or
                   (gethash full-path nox/compile-info-table)
                   (make-nox/compile-info :type (if (member test-script nox/compile-makefile-names) 'make 'sh)
                                          :path full-path
                                          :last-time '(0 0 0 0))))

            (push (cons (nox/compile-info-path info) info) script-list)
            (when (and (not nox/compile-info)
                       (or (not default-script) (time-less-p (nox/compile-info-last-time (cdr default-script))
                                                             (nox/compile-info-last-time info))))
              (setq default-script (car script-list))))))

      (if (not script-list)
          (error "No build script found")
        (if (or (not arg) (= (length script-list) 1))
            (setq script (cdr default-script))
          (setq script (cdr (assoc
                             (completing-read "Which build script? "
                                              script-list nil t nil nil (car default-script))
                             script-list))))

        (setf (nox/compile-info-last-time script) (current-time)
              (nox/compile-info-buffer-name script) (nox/compile-buffer-name script))
        (setq buffer (get-buffer-create (nox/compile-info-buffer-name script)))

        (if (projectile-project-p) (projectile-save-project-buffers) (save-some-buffers t))

        (if (= (length (window-list)) 1)
            (setf (nox/compile-info-should-close script) t)
          (unless (get-buffer-window buffer) (setf (nox/compile-info-should-close script) nil)))

        (with-current-buffer buffer
          (cd (file-name-directory (nox/compile-info-path script)))
          (let (command command-args)
            (if (eq (nox/compile-info-type script) 'make)
                (setq command "make"
                      command-args (or (nox/compile-info-last-args script) "-k"))

              (setq command (shell-quote-argument
                             (concat "./" (file-name-nondirectory (nox/compile-info-path script))))
                    command-args (or (nox/compile-info-last-args script) "%f")))

            (when arg (setq command-args (read-string "Arguments: " command-args)))

            (setf (nox/compile-info-last-args script) command-args)
            (setq command-args (replace-regexp-in-string
                                (regexp-quote "%f") (shell-quote-argument start-file-name) command-args))

            (setq nox/compile-info script)
            (compilation-start (concat command " " command-args) nil 'nox/compile-buffer-name)
            ;; NOTE(nox): Need to set it again in order to persist after changing the
            ;; major mode
            (setq nox/compile-info script)))

        (puthash (nox/compile-info-path script) script nox/compile-info-table))))

  (defun nox/compile-bury-buffer (buffer string)
    "Bury compilation buffer if it succeeded."
    (with-current-buffer buffer
      (when nox/compile-info
        (when (and (string= string "finished\n")
                   (save-excursion (not (ignore-errors (compilation-next-error 1 nil 1)))))
          (let ((windows (get-buffer-window-list buffer t)))
            (dolist (window windows)
              (if (and (> (length (window-list (window-frame window))) 1)
                       (nox/compile-info-should-close nox/compile-info))
                  (delete-window window)
                (switch-to-prev-buffer window))))
          (bury-buffer buffer)))))
  (add-hook 'compilation-finish-functions 'nox/compile-bury-buffer)

  (require 'ansi-color)
  (defun nox/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'nox/colorize-compilation-buffer)

  (setq-default compilation-ask-about-save nil
                compilation-always-kill t
                compilation-context-lines 2
                compilation-environment '("TERM=xterm")))

(use-package counsel :ensure
  :demand
  :delight (ivy-mode) (counsel-mode)
  :bind (("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . isearch-forward)
         (:map ivy-minibuffer-map
               ("<return>" . ivy-alt-done)
               ("C-j" . ivy-done))
         (:map read-expression-map ("C-r" . counsel-expression-history)))
  :config
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
        (find-file file-name))))

  (setq-default ivy-height 10
                ivy-use-virtual-buffers t
                ivy-extra-directories nil
                ivy-use-selectable-prompt t
                ivy-count-format "(%d/%d) "
                ivy-virtual-abbreviate 'full
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                        (t . ivy--regex-fuzzy))
                counsel-grep-base-command
                (cond ((executable-find "rg")
                       "rg -S -M 120 --no-heading --line-number --color never %s %s")
                      ((executable-find "ag") "ag --nogroup --nocolor %s %s")
                      (t "grep -i -E -n -e %s %s")))

  (add-to-list 'swiper-font-lock-exclude 'c-mode)
  (add-to-list 'swiper-font-lock-exclude 'c++-mode)

  (ivy-mode)
  (counsel-mode))

(use-package counsel-projectile :ensure
  :demand
  :delight (projectile-mode)
  :config
  (setq-default projectile-completion-system 'ivy)
  (counsel-projectile-mode))

(use-package delight :ensure)

(use-package ivy-hydra :ensure
  :demand :after ivy)

(use-package dired+
  :demand
  :after dired
  :bind (:map dired-mode-map
              ("e" . nox/ediff-files)
              ("Y" . nox/dired-rsync))
  :config
  (defun nox/pdf-compress-merge-sentinel (process event)
    (unless (process-live-p process)
      (let ((exit-code (process-exit-status process)))
        (if (/= exit-code 0)
            (error "Something went wrong with the process! Exit code: %d" exit-code)
          (let* ((data (process-get process 'data))
                 (output (car data))
                 (temp-output-name (car output))
                 (output-name (cdr output))
                 (files (cdr data)))
            (dolist (file files)
              (move-file-to-trash file))
            (ignore-errors (rename-file temp-output-name output-name 1))
            (message "Done compressing/merging PDF(s)."))))))

  (defun nox/pdf-compress-merge (arg)
    (interactive "P")
    (let ((files (dired-get-marked-files))
          (quality "ebook")
          (color-conv-strat "UseDeviceIndependentColor")
          (temp-output-name (format "MERGED_FILE_%d.pdf" (random 100000)))
          output-name)
      (if (< (length files) 1)
          (user-error "You must select at least one file!")
        (when arg
          (setq quality (completing-read
                         "Compression type: "
                         '("default" "screen" "ebook" "printer" "prepress")
                         nil t nil nil quality)
                color-conv-strat (completing-read
                                  "Color conversion strategy: "
                                  '("LeaveColorUnchanged" "Gray" "RGB" "sRGB" "CMYK" "UseDeviceIndependentColor")
                                  nil t nil nil color-conv-strat)))
        (setq output-name (completing-read "Output name: "
                                           (when (= (length files) 1)
                                             files)))
        (when (= (length output-name) 0) (setq output-name "Output.pdf"))
        (process-put (make-process
                      :name "PDF Compressor/Merger"
                      :connection-type 'pipe
                      :sentinel 'nox/pdf-compress-merge-sentinel
                      :command
                      (append
                       (list "gs" "-dBATCH" "-dNOPAUSE" "-q" "-sDEVICE=pdfwrite"
                             (concat "-sColorConversionStrategy=/" color-conv-strat)
                             (concat "-sOutputFile=" temp-output-name)
                             (concat "-dPDFSETTINGS=/" quality))
                       files))
                     'data (cons (cons temp-output-name output-name)  files)))))

  ;; From abo-abo
  (defun nox/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))

  (defun nox/dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name
        "Rsync to:"
        (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          (rsync-command
           "rsync -arvz --progress "))
      (dolist (file files)
        (setq rsync-command
              (concat rsync-command
                      (shell-quote-argument file)
                      " ")))
      (setq rsync-command
            (concat rsync-command
                    (shell-quote-argument dest)))
      (async-shell-command rsync-command "*rsync*")
      (other-window 1)))

  (setq-default dired-listing-switches "-alh"
                dired-recursive-deletes 'always
                dired-recursive-copies 'always
                dired-auto-revert-buffer t
                dired-dwim-target t))

(use-package diredfl :ensure
  :after dired
  :init (diredfl-global-mode))

(use-package dumb-jump :ensure
  :bind (("M-g j" . dumb-jump-go)
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq-default dumb-jump-selector 'ivy))

(use-package ediff
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-diff-options "-w")
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (derived-mode-p 'outline-mode)
                (outline-show-all)))))

(use-package eldoc :delight)

(use-package elisp-mode
  :delight (emacs-lisp-mode "Elisp" :major))

(use-package expand-region :ensure
  :bind ("C-=" . er/expand-region))

(use-package find-file
  :bind ("C-c o" . ff-find-other-file)
  :config
  (setq-default ff-always-try-to-create t))

(use-package flx :ensure)

(use-package gnuplot :ensure)

(use-package gdb-mi
  :bind ("C-c d" . hydra-gdb/body)
  :config
  (defvar nox/gdb-frame nil)
  (defvar nox/gdb-last-file nil)
  (defvar nox/gdb-last-args nil)
  (defvar nox/gdb-disassembly-show-source t)

  (defhydra hydra-gdb (:hint nil :exit t)
    "
Debug it!!
_O_pen    _R_un          _b_reak      _n_ext (_N_: inst)     _w_atch     _M_ixed? %-3`nox/gdb-disassembly-show-source
_k_ill    _S_tart        _t_break     _i_n (_I_: inst)
^ ^       _c_ontinue     _r_emove     _o_ut
^ ^       _s_top         ^ ^          _u_ntil"
    ("O" gdb)
    ("k" nox/gdb-kill)
    ("R" gud-run)
    ("S" gud-start)
    ("c" gud-cont)
    ("s" nox/gdb-stop)
    ("b" gud-break)
    ("t" gud-tbreak)
    ("r" gud-remove)
    ("n" gud-next :exit nil)
    ("N" gud-nexti :exit nil)
    ("i" gud-step :exit nil)
    ("I" gud-stepi :exit nil)
    ("o" gud-finish :exit nil)
    ("u" gud-until :exit nil)
    ("w" nox/gdb-watch)
    ("M" (lambda () (interactive) (setq nox/gdb-disassembly-show-source
                                        (not nox/gdb-disassembly-show-source))) :exit nil))

  (setq-default gdb-many-windows t
                gdb-show-main t
                gdb-display-buffer-other-frame-action
                '((display-buffer-reuse-window display-buffer-pop-up-frame)
                  (reusable-frames . visible)
                  (inhibit-same-window . t)
                  (pop-up-frame-parameters (minibuffer . t)
                                           (unsplittable . t)
                                           (width . 100)
                                           (fullscreen . fullheight)
                                           (border-width . 0))))

  (add-to-list 'gdb-disassembly-font-lock-keywords '("0x[[:xdigit:]]+" . font-lock-constant-face) t)
  (add-to-list 'gdb-disassembly-font-lock-keywords '("Line.*$" . font-lock-comment-face) t)

  (gud-def gud-start    "-exec-run --start"
           nil
           "Run the program and stop at the start of the inferior's main subprogram.")

  (defun nox/gdb-stop ()
    (interactive)
    (with-current-buffer gud-comint-buffer
      (comint-interrupt-subjob)
      (gud-call (gdb-gud-context-command "-exec-interrupt"))))

  (defun nox/gdb-watch (expr)
    (interactive "sEnter expression: ")
    (when (eq 'gdbmi (buffer-local-value 'gud-minor-mode gud-comint-buffer))
      (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" expr)
      (when (= (length expr) 0)
        (setq expr (if (and transient-mark-mode mark-active)
                       (buffer-substring (region-beginning) (region-end))
                     (concat (if (derived-mode-p 'gdb-registers-mode) "$")
                             (tooltip-identifier-from-point (point))))))
      (set-text-properties 0 (length expr) nil expr)
      (gdb-input (concat "-var-create - * \"" expr "\"")
                 `(lambda () (gdb-var-create-handler ,expr)))))

  (defun nox/gdb-kill (&optional frame)
    (interactive)
    (let ((more-frames (< 1 (length (visible-frame-list)))))
      (if (and more-frames (not frame) (frame-live-p nox/gdb-frame))
          (delete-frame nox/gdb-frame t) ; Only delete frame when running command, this
                                        ; function will be called again
        (let ((process (get-buffer-process gud-comint-buffer)))
          (if (and process (or (not frame) (eq frame nox/gdb-frame)))
              (kill-process process))))))
  (add-to-list 'delete-frame-functions 'nox/gdb-kill) ; Kill GDB when closing its frame

  (advice-add
   'gdb :around
   (lambda (gdb arg)
     (interactive "P")
     (let* ((this-frame (equal arg '(16)))
            (stop-or-specify (or this-frame (equal arg '(4)))))
       (if stop-or-specify (nox/gdb-kill))
       (let ((frame-live (and (not stop-or-specify) (frame-live-p nox/gdb-frame)))
             (gdb-running (and (not stop-or-specify) (get-buffer-process gud-comint-buffer))))
         (cond ((and gdb-running frame-live)
                (with-selected-frame nox/gdb-frame (gdb-restore-windows)))
               ((and gdb-running (not frame-live))
                (setq nox/gdb-frame (make-frame '((fullscreen . maximized) (name . "Emacs GDB"))))
                (with-selected-frame nox/gdb-frame (gdb-restore-windows)))
               (t
                (let* ((executable (or (unless stop-or-specify nox/gdb-last-file)
                                       (expand-file-name (read-file-name "Select file to debug: " nil nox/gdb-last-file t nox/gdb-last-file 'file-executable-p))))
                       (extra-args (or (unless stop-or-specify nox/gdb-last-args)
                                       (read-string "Extra arguments: " nox/gdb-last-args)))
                       (command-line (concat "gdb -i=mi " executable " " extra-args)))
                  (when (file-executable-p executable)
                    (setq nox/gdb-last-file executable)
                    (setq nox/gdb-last-args extra-args)
                    (if this-frame
                        (progn
                          (setq nox/gdb-frame (selected-frame))
                          (modify-frame-parameters nil '((name . "Emacs GDB"))))
                      (unless frame-live
                        (setq nox/gdb-frame (make-frame '((fullscreen . maximized) (name . "Emacs GDB"))))))
                    (with-selected-frame nox/gdb-frame (funcall gdb command-line)))))))
       (select-frame-set-input-focus nox/gdb-frame))))

  ;; Prevent buffer stealing, https://stackoverflow.com/a/24923325/2175348
  (defun gdb-inferior-filter (proc string)
    (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
      (comint-output-filter proc string)))

  (defun gud-display-line (true-file line)
    (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
           (buffer
            (with-current-buffer gud-comint-buffer
              (gud-find-file true-file)))
           (window (and buffer
                        (or (get-buffer-window buffer)
                            (display-buffer buffer '(nil (inhibit-same-window . t)
                                                         (inhibit-switch-frame t))))))
           (pos))
      (when buffer
        (with-current-buffer buffer
          (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
            (if (yes-or-no-p
                 (format "File %s changed on disk.  Reread from disk? "
                         (buffer-name)))
                (revert-buffer t t)
              (setq gud-keep-buffer t)))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line (1- line))
            (setq pos (point))
            (or gud-overlay-arrow-position
                (setq gud-overlay-arrow-position (make-marker)))
            (set-marker gud-overlay-arrow-position (point) (current-buffer))
            (when (featurep 'hl-line)
              (cond
               (global-hl-line-mode
                (global-hl-line-highlight))
               ((and hl-line-mode hl-line-sticky-flag)
                (hl-line-highlight)))))
          (cond ((or (< pos (point-min)) (> pos (point-max)))
                 (widen)
                 (goto-char pos))))
        (when window
          (set-window-point window gud-overlay-arrow-position)
          (if (eq gud-minor-mode 'gdbmi)
              (setq gdb-source-window window))
          (with-selected-window window (recenter 0))))))

  ;; Better assembly view
  (def-gdb-auto-update-trigger gdb-invalidate-disassembly
    (if nox/gdb-disassembly-show-source
        "-data-disassemble -s $pc -e \"$pc + 200\" -- 4"
      "-data-disassemble -s $pc -e \"$pc + 200\" -- 0")
    gdb-disassembly-handler
    '(start update-disassembly))

  (defun gdb-disassembly-handler-custom ()
    (let* ((lines (bindat-get-field (gdb-json-partial-output "src_and_asm_line") 'asm_insns))
           (address (bindat-get-field (gdb-current-buffer-frame) 'addr))
           (table (make-gdb-table))
           (marked-line nil))
      (dolist (line lines)
        (let ((line-number (bindat-get-field line 'line)))
          (if (not line-number)
              (progn
                (gdb-table-add-row table
                                   (list
                                    (bindat-get-field line 'address)
                                    (bindat-get-field line 'inst)))
                (when (string-equal (bindat-get-field line 'address) address)
                  (setq marked-line (length (gdb-table-rows table)))))
            (gdb-table-add-row table
                               (list
                                (format "Line %s" (bindat-get-field line 'line))
                                (nox/get-line-from-file
                                 (bindat-get-field line 'fullname)
                                 (string-to-number (bindat-get-field line 'line)) t)))
            (dolist (instr (bindat-get-field line 'line_asm_insn))
              (gdb-table-add-row table
                                 (list
                                  (bindat-get-field instr 'address)
                                  (bindat-get-field instr 'inst)))
              (when (string-equal (bindat-get-field instr 'address) address)
                (setq marked-line (length (gdb-table-rows table))))))))
      (insert (gdb-table-string table " "))
      (gdb-disassembly-place-breakpoints)
      (when marked-line
        (setq fringe-indicator-alist
              (if (string-equal gdb-frame-number "0")
                  nil
                '((overlay-arrow . hollow-right-triangle))))
        (let ((window (get-buffer-window (current-buffer) 0)))
          (set-window-point window (gdb-mark-line marked-line
                                                  gdb-disassembly-position))))
      (setq mode-name (gdb-current-context-mode-name
                       (concat "Disassembly: " (bindat-get-field (gdb-current-buffer-frame) 'func))))))

  (defun gdb-var-update-handler ()
    (let ((changelist (bindat-get-field (gdb-json-partial-output) 'changelist)))
      (dolist (var gdb-var-list)
        (setcar (nthcdr 5 var) nil))
      (let ((temp-var-list gdb-var-list))
        (dolist (change changelist)
          (let* ((varnum (bindat-get-field change 'name))
                 (var (assoc varnum gdb-var-list))
                 (new-num (bindat-get-field change 'new_num_children))
                 (new-value (bindat-get-field change 'value)))
            (when var
              (let ((scope (bindat-get-field change 'in_scope))
                    (has-more (bindat-get-field change 'has_more)))
                (cond ((string-equal scope "false")
                       (if gdb-delete-out-of-scope
                           (gdb-var-delete-1 var varnum)
                         (setcar (nthcdr 5 var) 'out-of-scope)))
                      ((string-equal scope "true")
                       (setcar (nthcdr 6 var) has-more)
                       (when (and new-value
                                  (not new-num)
                                  (or (not has-more)
                                      (string-equal has-more "0")))
                         (setcar (nthcdr 4 var) new-value)
                         (setcar (nthcdr 5 var) 'changed)))
                      ((string-equal scope "invalid")
                       (gdb-var-delete-1 var varnum)))))
            (let ((var-list nil) var1
                  (children (bindat-get-field change 'new_children)))
              (when new-num
                (setq var1 (pop temp-var-list))
                (while var1
                  (if (string-equal varnum (car var1))
                      (let ((new (string-to-number new-num))
                            (previous (string-to-number (nth 2 var1))))
                        (setcar (nthcdr 2 var1) new-num)
                        (push var1 var-list)
                        (cond
                         ((> new previous)
                          ;; Add new children to list.
                          (dotimes (_ previous)
                            (push (pop temp-var-list) var-list))
                          (dolist (child children)
                            (let ((varchild
                                   (list (bindat-get-field child 'name)
                                         (bindat-get-field child 'exp)
                                         (bindat-get-field child 'numchild)
                                         (bindat-get-field child 'type)
                                         (bindat-get-field child 'value)
                                         'changed
                                         (bindat-get-field child 'has_more))))
                              (push varchild var-list))))
                         ;; Remove deleted children from list.
                         ((< new previous)
                          (dotimes (_ new)
                            (push (pop temp-var-list) var-list))
                          (dotimes (_ (- previous new))
                            (pop temp-var-list)))))
                    (push var1 var-list))
                  (setq var1 (pop temp-var-list)))
                (setq gdb-var-list (nreverse var-list))))))))
    (gdb-speedbar-update)))

(use-package go-mode :ensure
  :config
  (setq-default gofmt-command (substitute-in-file-name "$GOPATH/bin/goimports"))
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package help
  :config
  (setq-default help-window-select t))

(use-package holidays
  :config
  (setq-default calendar-holidays
                '((holiday-fixed  1  1    "New Year's Day")
                  (holiday-easter-etc -47 "Carnival")
                  (holiday-easter-etc  -2 "Good Friday")
                  (holiday-easter-etc   0 "Easter")
                  (holiday-fixed  4 25    "Freedom Day")
                  (holiday-fixed  5  1    "Labour Day")
                  (holiday-easter-etc  60 "Corpus Christi")
                  (holiday-fixed  6 10    "Portugal Day")
                  (holiday-fixed  8 15    "Assumption")
                  (holiday-fixed 10  5    "Republic Day")
                  (holiday-fixed 11  1    "All Saints Day")
                  (holiday-fixed 12  1    "Restoration of Independence")
                  (holiday-fixed 12  8    "Immaculate Conception")
                  (holiday-fixed 12 25    "Christmas"))))

(use-package imenu
  :config
  (set 'imenu-auto-rescan-maxout 500000)
  (set 'imenu-auto-rescan t))

(use-package imenu-anywhere :ensure
  :bind ("C-c i" . nox/ivy-imenu-center)
  :config
  (defun nox/ivy-imenu-center ()
    (interactive)
    (call-interactively 'ivy-imenu-anywhere)
    (recenter-top-bottom)))

(use-package ispell
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell"
                  ispell-really-hunspell t)))

(use-package magit :ensure
  :if (executable-find "git")
  :config
  (setq-default magit-completing-read-function 'ivy-completing-read
                magit-diff-refine-hunk 'all))

(use-package multiple-cursors :ensure
  :bind (("C-c l" . mc/edit-lines)
         ("M-»" . mc/mark-next-like-this)
         ("M-«" . mc/mark-previous-like-this)
         ("C-M-«" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (unbind-key "M-<down-mouse-1>"))

(use-package nov :ensure
  :mode (("\\.epub\\'" . nov-mode)))

(use-package octave
  :mode (("\\.m\\'" . octave-mode))
  :config
  (setq-default inferior-octave-startup-args '("-i" "--line-editing"))
  ;; NOTE(nox): Defining functions on octave sometimes failed without this!
  (add-hook 'inferior-octave-mode-hook (lambda () (setq eldoc-documentation-function nil))))

(use-package org :ensure
  :delight (org-cdlatex-mode)
  :bind (("C-c o" . hydra-org/body)
         (:map org-mode-map
               ("C-c C-q" . counsel-org-tag)))
  :config
  (defhydra hydra-org (:exit t :foreign-keys warn)
    "Org-mode"
    ("c" org-capture "Capture")
    ("a" org-agenda "Agenda")
    ("n" org-noter "Noter")
    ("l" org-store-link "Store link")
    ("q" nil "Quit"))

  ;; NOTE(nox): General setup
  (setq-default org-modules '(org-habit org-id org-protocol org-timer)
                org-directory "~/Personal/Org/"
                org-default-notes-file (concat org-directory "Inbox.org"))
  (defconst nox/org-agenda-main-file (concat org-directory "GTD.org"))
  (defconst nox/org-agenda-journal-file (concat org-directory "Journal.org"))
  (defconst nox/org-reference-file (concat org-directory "Reference.org"))
  (setq-default org-agenda-files (list org-default-notes-file nox/org-agenda-main-file))

  ;; NOTE(nox): Appearance & behavior
  (setq-default org-startup-indented t
                org-startup-with-inline-images t
                org-startup-with-latex-preview t
                org-pretty-entities t
                org-image-actual-width nil
                org-fontify-quote-and-verse-blocks t

                org-tags-column -102
                org-catch-invisible-edits 'smart
                org-return-follows-link t
                org-list-allow-alphabetical t
                org-loop-over-headlines-in-active-region t)

  (add-hook 'org-mode-hook (lambda () (org-hide-block-all) (turn-on-org-cdlatex)))

  ;; NOTE(nox): Tasks & states
  (setq-default org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                    (sequence "HOLD(h@/!)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))
                org-treat-S-cursor-todo-selection-as-state-change nil
                org-todo-state-tags-triggers
                '((todo ("CANCELLED"))
                  (done ("HOLD") ("WAITING"))
                  ("TODO" ("HOLD") ("WAITING"))
                  ("NEXT" ("HOLD") ("WAITING"))
                  ("DONE" ("CANCELLED"))
                  ("WAITING" ("WAITING" . t) ("HOLD"))
                  ("HOLD" ("WAITING") ("HOLD" . t))
                  ("CANCELLED" ("CANCELLED" . t)))
                org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
                org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 7:00")))

  (defun nox/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states) (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'nox/org-summary-todo)

  ;; NOTE(nox): Refiling
  (setq-default org-refile-targets `((nil . (:maxlevel . 9))
                                     (org-agenda-files . (:maxlevel . 9))
                                     (,nox/org-reference-file . (:maxlevel . 9)))
                org-refile-use-outline-path 'file
                org-outline-path-complete-in-steps nil
                org-refile-allow-creating-parent-nodes 'confirm)

  (defun nox/verify-refile-target ()
    (if (member (org-get-todo-state) org-done-keywords)
        (progn (org-end-of-subtree t t) nil)
      t))
  (setq-default org-refile-target-verify-function 'nox/verify-refile-target)

  ;; NOTE(nox): Logging
  (setq-default org-log-done 'time
                org-log-reschedule 'time
                org-log-into-drawer t)

  ;; NOTE(nox): Helper functions
  (defun nox/org-has-subtasks-p (&optional specific-keywords ignore-keywords)
    "Any heading with subtasks.
When SPECIFIC-KEYWORDS is nil, returns t or `all-planned' when has subtasks, and nil otherwise.
When it is a list of keywords, returns (HAS-SUBTASKS . HAS-SPECIFIC).
HAS-SPECIFIC is t or `all-planned'.
`all-planned' is returned when every subtask found will not be shown in the agenda because of
scheduling/deadline.
Subtrees whose parent todo keyword is in IGNORE-KEYWORDS are ignored."
    (org-with-wide-buffer
     (let ((subtree-end (save-excursion (org-end-of-subtree t)))
           has-subtasks has-specific)
       (forward-line)
       (while (and (< (point) subtree-end)
                   (or (not (eq has-subtasks t))
                       (and specific-keywords (not (eq has-specific t))))
                   (re-search-forward org-todo-line-regexp subtree-end t))
         (let ((keyword (match-string 2))
               (planned (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)))
           (if (member keyword ignore-keywords)
               (org-end-of-subtree t)
             (if (member keyword specific-keywords)
                 (setq has-subtasks (or (eq has-subtasks t) (if planned 'all-planned t))
                       has-specific (or (eq has-specific t) (if planned 'all-planned t)))
               (when (member keyword org-todo-keywords-1)
                 (setq has-subtasks (or (eq has-subtasks t) (if planned 'all-planned t))))))))
       (if specific-keywords
           (cons has-subtasks has-specific)
         has-subtasks))))

  (defun nox/org-project-p ()
    "Any task that has subtasks."
    (and (org-get-todo-state) (nox/org-has-subtasks-p)))

  (defun nox/org-not-project-p ()
    "Any task that doesn't have subtasks."
    (and (org-get-todo-state) (not (nox/org-has-subtasks-p))))

  (defun nox/org-parent-projects (&optional first)
    "Get projects that enclose this heading, each specified by (NAME . POS).
When FIRST is non-nil, return only the closest parent project that contains this heading.
Else, return full list of projects."
    (let (projects components)
      (org-with-wide-buffer
       (org-back-to-heading 'invisible-ok)
       (while (and (not (and first projects))
                   (org-up-heading-safe))
         (setq components (org-heading-components))
         (when (org-get-todo-state)
           (push (cons (substring-no-properties (org-get-heading t t t t)) (point)) projects))))
      (if first (car projects) projects)))

  (defun nox/org-project-status ()
    "Return nil when heading is not project. When it is, returns `done', `stuck', `planned' or `has-next'."
    (let ((todo-state (org-get-todo-state))
          subtasks-p)
      (when todo-state
        (setq subtasks-p (nox/org-has-subtasks-p '("NEXT") '("CANCELLED" "HOLD")))
        (when (car subtasks-p)
          (if (member todo-state org-done-keywords)
              'done
            (if (cdr subtasks-p)
                (if (eq (cdr subtasks-p) 'all-planned) 'planned 'has-next)
              'stuck))))))

  (defun nox/org-project-set-next-after-done ()
    "Ask to TODO to NEXT when changing previous states to DONE."
    (let ((done-keywords (or org-done-keywords org-done-keywords-for-agenda))
          target break)
      (when (and (member org-state done-keywords) (nox/org-parent-projects t))
        (org-with-wide-buffer
         (org-back-to-heading t)
         (save-excursion
           (while (and (not target) (org-get-next-sibling))
             (let ((keyword (org-get-todo-state)))
               (if (nox/org-project-p)
                   (setq target 'cancel)
                 (if (string= keyword "TODO")
                     (setq target (cons (point) (org-get-heading t t t t)))
                   (unless (member keyword done-keywords)
                     (setq target 'cancel)))))))
         (save-excursion
           (while (and (consp target) (not break) (org-get-last-sibling))
             (let ((keyword (org-get-todo-state)))
               (if (string= keyword "NEXT")
                   (setq target nil)
                 (when (member keyword org-todo-keywords)
                   (setq break t))))))
         (when (consp target)
           (when (y-or-n-p (concat "Do you want to set " (cdr target) " to NEXT?"))
             (goto-char (car target))
             (org-todo "NEXT")))))))
  (add-hook 'org-after-todo-state-change-hook 'nox/org-project-set-next-after-done)

  (defun nox/org-offer-all-agenda-tags ()
    (setq-local org-complete-tags-always-offer-all-agenda-tags t))

  ;; NOTE(nox): LaTeX
  (setq-default org-preview-latex-default-process 'dvisvgm
                org-latex-packages-alist '(("" "tikz" t)
                                           ("american,siunitx,smartlabels" "circuitikz" t)
                                           ("" "mathtools" t))
                org-latex-preview-ltxpng-directory (locate-user-emacs-file "Latex Previews/")
                org-format-latex-options
                '(:foreground default :background default :scale 1.7
                              :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                org-preview-latex-process-alist
                '((dvisvgm :programs ("latex" "dvisvgm")
                           :description "dvi > svg"
                           :message "you need to install the programs: latex and dvisvgm."
                           :use-xcolor t
                           :image-input-type "dvi"
                           :image-output-type "svg"
                           :image-size-adjust (1.7 . 1.5)
                           :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                           :image-converter ("dvisvgm %f -n -b 1 -c %S -o %O"))
                  (imagemagick :programs ("latex" "convert")
                               :description "pdf > png"
                               :message "you need to install the programs: latex and imagemagick."
                               :use-xcolor t
                               :image-input-type "pdf"
                               :image-output-type "png"
                               :image-size-adjust (1.0 . 1.0)
                               :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
                               :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))
                  (dvipng :programs ("latex" "dvipng")
                          :description "dvi > png"
                          :message "you need to install the programs: latex and dvipng."
                          :image-input-type "dvi"
                          :image-output-type "png"
                          :image-size-adjust (1.0 . 1.0)
                          :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
                          :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f")))
                org-format-latex-header
                "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
\\tikzset{every picture/.style={color=fg}}")

  ;; NOTE(nox): Get different latex fragments for different themes
  (defvar nox/org-sha-salt)
  (advice-add
   'org-format-latex :around
   (lambda (orig-function &rest args)
     (setq nox/org-sha-salt (concat (face-attribute 'default :foreground)
                                    (face-attribute 'default :background)))
     (cl-letf (((symbol-function 'sha1)
                (lambda (object &optional start end binary)
                  (secure-hash 'sha1 (concat object nox/org-sha-salt)
                               start end binary))))
       (apply orig-function args))))

  ;; NOTE(nox): Babel
  (setq-default org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (octave . t)
     (python . t)
     (latex . t)
     (shell . t)
     (calc . t)))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; NOTE(nox): PDF Tools support
  (org-link-set-parameters "pdfview"
                           :follow 'org-pdfview-open
                           :complete 'org-pdfview-complete-link
                           :store 'org-pdfview-store-link))

(use-package org-agenda
  :bind ((:map org-agenda-mode-map
               ("C-c C-q" . counsel-org-tag)))
  :config
  (defun nox/org-agenda-finalize ()
    ;; NOTE(nox): Reset project hierarchy builder helper variable
    (setq nox/org-agenda-first-project t)

    ;; NOTE(nox): Remove empty blocks
    (save-excursion
      (goto-char (point-min))
      (let ((prev (if (get-text-property (point-min) 'org-agenda-structural-header)
                      (point-min)
                    (next-single-property-change (point-min) 'org-agenda-structural-header)))
            next)
        (while (and prev (/= prev (point-max)))
          (setq next
                (or (next-single-property-change (next-single-property-change prev 'org-agenda-structural-header)
                                                 'org-agenda-structural-header)
                    (point-max)))
          (if (or (and (< next (point-max)) (< (count-lines prev next) 4))
                  (and (= next (point-max)) (< (count-lines prev next) 2)))
              (delete-region prev next)
            (setq prev next)))))

    ;; NOTE(nox): Check for sync conflicts!
    (catch 'break
      (message
       (dolist (file (directory-files org-directory))
         (when (string-match-p "sync-conflict" file)
           (message-box "AVISO: Há conflitos de sincronização!")
           (throw 'break t))))))

  (defun nox/org-agenda-stuck-skip-function ()
    (org-with-wide-buffer
     (let ((next-heading (save-excursion (or (outline-next-heading) (point-max)))))
       (when (not (eq (nox/org-project-status) 'stuck)) next-heading))))

  (defun nox/org-agenda-projects-next-skip-function ()
    (org-with-wide-buffer
     (let ((next-heading (save-excursion (or (outline-next-heading) (point-max))))
           (project-status (nox/org-project-status)))
       (if (eq project-status 'done)
           (org-end-of-subtree t)
         (if (or (eq project-status 'has-next)
                 (and (string= (org-get-todo-state) "NEXT")
                      (nox/org-parent-projects t)))
             (when (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)
               (org-end-of-subtree t))
           next-heading)))))

  (defvar nox/org-agenda-first-project t)
  (defun nox/org-agenda-projects-next-prefix ()
    (let* ((is-project (nox/org-project-p))
           (parent-projects (nox/org-parent-projects))
           (number-of-proj (length parent-projects))
           result)
      (if is-project
          (setq result (concat "  " (apply 'concat (make-list number-of-proj "| "))))
        (setq result (concat "  " (apply 'concat (make-list (1- number-of-proj) "| ")) "├─⮞ ")))
      (setq nox/org-agenda-first-project nil)
      result))

  (defun nox/org-agenda-tasks-skip-function ()
    (org-with-wide-buffer
     (let ((next-heading (save-excursion (or (outline-next-heading) (point-max)))))
       (when (or (nox/org-project-p) (nox/org-parent-projects t)) next-heading))))

  (defun nox/org-agenda-waiting-skip-function ()
    (org-with-wide-buffer
     (let ((next-heading (save-excursion (or (outline-next-heading) (point-max))))
           (keyword (org-get-todo-state))
           (project-status (nox/org-project-status)))
       (unless (or (eq project-status 'planned)
                   (and project-status (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item))
                   (member keyword '("WAITING" "HOLD")))
         next-heading))))

  (defun nox/org-agenda-archivable-skip-function ()
    (org-with-wide-buffer
     (let ((next-heading (save-excursion (or (outline-next-heading) (point-max))))
           (subtree-end (save-excursion (org-end-of-subtree t)))
           (keyword (org-get-todo-state)))
       (if (member keyword org-todo-keywords-1)
           (if (member keyword org-done-keywords)
               (let* ((day (nth 3 (decode-time)))
                      (this-month (format-time-string "%Y-%m-" (current-time)))
                      (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time (* 60 60 24 (+ day 1)))))))
                 (forward-line 1)
                 (when (and (< (point) subtree-end)
                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t))
                   subtree-end))
             subtree-end)
         next-heading))))

  (setq-default
   org-agenda-custom-commands
   '(("n" "Agenda"
      ((agenda ""
               ((org-agenda-files (list org-default-notes-file nox/org-agenda-main-file
                                        nox/org-agenda-journal-file))
                (org-agenda-skip-scheduled-if-deadline-is-shown t)))
       (tags "REFILE"
             ((org-agenda-overriding-header "Coisas por organizar")
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list org-default-notes-file))))
       (tags-todo "-CANCELLED-HOLD/!"
                  ((org-agenda-overriding-header "Projetos estagnados")
                   (org-agenda-skip-function 'nox/org-agenda-stuck-skip-function)
                   (org-agenda-sorting-strategy '(category-keep))))
       (tags-todo "-CANCELLED+PRIORITY=\"A\"/!"
                  ((org-agenda-overriding-header "Prioritário")
                   (org-agenda-sorting-strategy '(time-up category-keep))
                   (org-agenda-todo-ignore-scheduled 'future)))
       (tags "-REFILE-CANCELLED-WAITING-HOLD"
             ((org-agenda-overriding-header "Projetos")
              (org-agenda-skip-function 'nox/org-agenda-projects-next-skip-function)
              (org-agenda-prefix-format "%(nox/org-agenda-projects-next-prefix)")
              (org-agenda-sorting-strategy '(category-keep))
              (org-agenda-tags-todo-honor-ignore-options nil)
              (org-agenda-todo-ignore-scheduled 'future)))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-PRIORITY=\"A\"-PRIORITY=\"C\"/!"
                  ((org-agenda-overriding-header "Tarefas isoladas")
                   (org-agenda-skip-function 'nox/org-agenda-tasks-skip-function)
                   (org-agenda-todo-ignore-with-date t)
                   (org-agenda-sorting-strategy '(deadline-down priority-down effort-up category-keep))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD+PRIORITY=\"C\"/!"
                  ((org-agenda-overriding-header "Tarefas de baixa prioridade")
                   (org-agenda-skip-function 'nox/org-agenda-tasks-skip-function)
                   (org-agenda-sorting-strategy '(deadline-down effort-up category-keep))))
       (tags "+Interessante+Level=2"
             ((org-agenda-overriding-header "Coisas interessantes")
              (org-agenda-sorting-strategy '(effort-up category-keep))
              (org-agenda-show-inherited-tags nil)))
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tarefas a arquivar")
              (org-agenda-skip-function 'nox/org-agenda-archivable-skip-function)
              (org-tags-match-list-sublevels nil)
              (org-agenda-files (list nox/org-agenda-main-file))))
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Tarefas à espera ou em pausa")
                   (org-agenda-skip-function 'nox/org-agenda-waiting-skip-function)
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-tags-todo-honor-ignore-options nil)
                   (org-agenda-todo-ignore-scheduled 'future))))
      ((org-agenda-finalize-hook 'nox/org-agenda-finalize))))
   org-agenda-span 'day
   org-agenda-prefix-format '((agenda . "  %?-12t% s")
                              (todo   . "  ")
                              (tags   . "  ")
                              (search . "  "))
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-todo-ignore-scheduled 'all
   org-agenda-todo-ignore-deadlines 'far
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
   org-agenda-columns-add-appointments-to-effort-sum t
   org-agenda-dim-blocked-tasks nil
   org-agenda-todo-list-sublevels nil
   org-agenda-block-separator ""
   org-agenda-time-grid '((daily today require-timed) nil "......" "----------------"))

  (add-hook 'org-agenda-mode-hook 'nox/org-offer-all-agenda-tags))

(use-package org-capture
  :init
  (defun nox/org-capture-frame ()
    (modify-frame-parameters nil '((name . "Org Capture")
                                   (org-capture-frame . t)
                                   (width . 110) (height . 40)))
    (org-capture))

  :config
  (defun nox/org-capture-add-created-property ()
    (org-set-property "CREATED" (format-time-string
                                 (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))))
  (add-hook 'org-capture-before-finalize-hook 'nox/org-capture-add-created-property)

  (setq-default
   org-capture-templates '(("t" "Tarefa" entry (file "")
                            "* NEXT %i%?" :clock-in t :clock-resume t)
                           ("c" "Calendário" entry (file "")
                            "* %?\n%^t")
                           ("n" "Nota" entry (file "")
                            "* %?" :clock-in t :clock-resume t)
                           ("d" "Diário" entry (file+olp+datetree nox/org-agenda-journal-file)
                            "* %?" :clock-in t :clock-resume t)
                           ("w" "Web bookmark" entry (file "")
                            "* [[%:link][%^{Title|%:description}]]\n%?" :clock-in t :clock-resume t)))

  (add-hook 'org-capture-mode-hook 'nox/org-offer-all-agenda-tags)

  ;; NOTE(nox): Handle capture frame
  (advice-add
   'org-switch-to-buffer-other-window :after
   (lambda (&rest _) (when (frame-parameter nil 'org-capture-frame) (delete-other-windows))))
  (advice-add
   'org-capture :around
   (lambda (capture-function &rest args)
     (condition-case nil (apply capture-function args)
       (error (when (frame-parameter nil 'org-capture-frame)
                (delete-frame))))))
  (add-hook
   'org-capture-after-finalize-hook
   (lambda (&rest _)
     (when (and (frame-parameter nil 'org-capture-frame) (not org-capture-is-refiling))
       (org-save-all-org-buffers)
       (delete-frame))))
  (advice-add
   'org-capture-refile :after
   (lambda (&rest _)
     (when (frame-parameter nil 'org-capture-frame)
       (org-save-all-org-buffers)
       (delete-frame)))))

(use-package org-clock
  :config
  (defun nox/org-clock-in-switch-state (current)
    "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond ((and (string= current "TODO") (nox/org-not-project-p))
             "NEXT")
            ((and (string= current "NEXT") (nox/org-project-p))
             "TODO"))))

  (setq-default org-clock-in-resume t
                org-clock-in-switch-to-state 'nox/org-clock-in-switch-state
                org-clock-out-remove-zero-time-clocks t
                org-clock-report-include-clocking-task t
                org-clock-persist t
                org-clock-persist-file (locate-user-emacs-file "clock-persist.el")
                org-clock-history-length 25)
  (org-clock-persistence-insinuate))

(use-package org-edit-latex :ensure)

(use-package org-element :commands org-element-update-syntax)

(use-package org-habit
  :config
  (setq-default org-habit-graph-column 70
                org-habit-today-glyph ?@))

(use-package org-id
  :config
  (setq-default org-id-link-to-org-use-id 'create-if-interactive))

(use-package org-indent :delight)

(use-package org-noter :ensure
  :config
  (setq-default org-noter-default-heading-title "Notas da página $p$"
                org-noter-default-notes-file-names '("Notes.org" "Notas.org")
                org-noter-hide-other t))

(use-package org-src
  :config
  (setq-default org-src-fontify-natively t
                org-src-tab-acts-natively t
                org-edit-src-content-indentation 0)

  (add-to-list 'org-src-lang-modes '("html" . web)))

(use-package pdf-tools :ensure
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link)
  :config
  ;; Adapted from https://github.com/markus1189/org-pdfview
  (defun org-pdfview-open (link)
    "Open LINK in pdf-view-mode."
    (cond ((string-match "\\(.*\\)::\\([0-9]*\\)\\+\\+\\([[0-9]\\.*[0-9]*\\)"  link)
           (let* ((path (match-string 1 link))
                  (page (string-to-number (match-string 2 link)))
                  (height (string-to-number (match-string 3 link))))
             (org-open-file path 1)
             (pdf-view-goto-page page)
             (image-set-window-vscroll
              (round (/ (* height (cdr (pdf-view-image-size))) (frame-char-height))))))
          ((string-match "\\(.*\\)::\\([0-9]+\\)$"  link)
           (let* ((path (match-string 1 link))
                  (page (string-to-number (match-string 2 link))))
             (org-open-file path 1)
             (pdf-view-goto-page page)))
          (t (org-open-file link 1))))

  (defun org-pdfview-store-link ()
    "Store a link to a pdfview buffer."
    (when (eq major-mode 'pdf-view-mode)
      (let* ((path buffer-file-name)
             (page (number-to-string (pdf-view-current-page)))
             (link (concat "pdfview:" path "::" page))
             (description (concat (file-name-nondirectory path) " at page " page)))
        (org-store-link-props
         :type "pdfview"
         :link link
         :description description))))

  (defun org-pdfview-complete-link (&optional arg)
    "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
    (concat (replace-regexp-in-string "^file:" "pdfview:" (org-file-complete-link arg))
            "::"
            (read-from-minibuffer "Page:" "1")))

  (setq-default pdf-view-display-size 'fit-page
                pdf-cache-image-limit 200
                pdf-view-use-imagemagick t)
  (pdf-tools-install))

(use-package paren
  :demand
  :config
  (show-paren-mode)
  (setq-default show-paren-delay 0))

(use-package recentf
  :config
  (setq-default recentf-max-saved-items 300
                recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG")))

(use-package server
  :config
  (add-hook 'after-make-frame-functions (lambda (frame) (select-frame-set-input-focus frame)) t))

(use-package smex :ensure)

(use-package speedbar
  :config
  (setq-default speedbar-frame-parameters '((minibuffer . t)
                                            (unsplittable . t)
                                            (width . 30)
                                            (border-width . 0)
                                            (left-fringe . 0))))

(use-package tex :ensure auctex)

(use-package time
  :config
  (setq-default display-time-24hr-format t
                display-time-load-average-threshold 1.5))

(use-package tramp
  :config
  (setq-default tramp-default-method "ssh"
                tramp-default-proxies-alist
                '(((regexp-quote (system-name)) nil nil)
                  (nil "\\`root\\'" "/ssh:%h:"))
                tramp-backup-directory-alist '(("." . "/tmp/"))))

(use-package treemacs :ensure)

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-after-kill-buffer-p t))

(use-package web-mode :ensure
  :mode (("\\.\\(go\\)?html?\\'" . web-mode)))

;; ------------------------------
;; Machine specific settings
(if (file-exists-p private-settings-file)
    (load private-settings-file))
