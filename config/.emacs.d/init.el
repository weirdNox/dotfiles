;; Author: weirdNox (Gonçalo Santos)               -*- lexical-binding: t -*-

;; ------------------------------
;; Core
(when (version< emacs-version "25.1")
  (error "This config requires at least GNU Emacs 25.1, but you're running version %s."
         emacs-version))

(setq-default gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq-default gc-cons-threshold 1000000)))

(setq-default default-directory "~/"
              private-settings-file (locate-user-emacs-file "private.el")
              temp-dir (locate-user-emacs-file "temp")
              custom-file (locate-user-emacs-file "custom.el")
              trash-directory (locate-user-emacs-file "trash"))

(require 'package)
(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/"))
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


;; ------------------------------
;; Appearance
(use-package gruvbox :ensure gruvbox-theme
  :demand
  :config (load-theme 'gruvbox-dark-hard t))

(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default initial-frame-alist '((fullscreen . fullboth)))

(defun nox/font-setup (&optional frame)
  (catch 'break
    (dolist (font '(("DejaVu Sans Mono" . 11)
                    ("Source Code Pro" . 11)))
      (let* ((font-name (car font))
             (font-size (cdr font))
             (font-setting (format "%s-%d" font-name font-size)))
        (when (member font-name (font-family-list frame))
          (set-frame-font font-setting nil t)
          (add-to-list 'default-frame-alist (cons 'font font-setting))
          (throw 'break t)))))
  (remove-hook 'focus-in-hook 'nox/font-setup)
  (remove-hook 'after-make-frame-functions 'nox/font-setup))

(add-hook 'focus-in-hook 'nox/font-setup)
(add-hook 'after-make-frame-functions 'nox/font-setup)

(setq-default inhibit-startup-screen t
              initial-scratch-message "")

(setq-default truncate-partial-width-windows 70
              word-wrap t)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(global-hl-line-mode 1)
(blink-cursor-mode 0)

(setq-default ring-bell-function 'ignore)

(line-number-mode t)
(column-number-mode t)
(display-time-mode)
(setq-default display-time-24hr-format t
              display-time-load-average-threshold 1.5)


;; ------------------------------
;; Behaviour
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

 save-interprogram-paste-before-kill t

 delete-by-moving-to-trash t
 backup-directory-alist `((".*" . ,temp-dir))
 auto-save-file-name-transforms `((".*" ,temp-dir t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default default-input-method "TeX")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(global-auto-revert-mode)

(mouse-avoidance-mode 'jump)

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


;; ------------------------------
;; Keybindings
(use-package hydra :ensure t
  :config (setq-default lv-use-separator t))

(bind-keys
 ("<C-return>" . nox/open-line-below)
 ("<C-M-return>" . nox/open-line-above)
 ("<backtab>" . indent-for-tab-command)
 ("<C-tab>" . indent-region)
 ("M-o" . other-window)
 ("M-O" . (lambda () (interactive) (other-window -1))))

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
(use-package avy :ensure t
  :bind ("C-c a" . avy-goto-char))

(use-package calendar
  :config
  (setq-default calendar-week-start-day 1
                calendar-date-display-form calendar-european-date-display-form))

(use-package cdlatex :ensure t)

(use-package company :ensure t
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
  :init
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
  :bind (("<f12>" . nox/make)
         ("C-c n" . hydra-error/next-error))
  :config
  (defhydra hydra-error ()
    "Errors"
    ("f" first-error "First")
    ("n" next-error "Next")
    ("p" previous-error "Previous")
    ("q" nil "Quit"))

  (setq-default compilation-ask-about-save nil
                compilation-always-kill t
                compilation-context-lines 0
                compilation-environment '("TERM=xterm"))

  (defvar-local nox/build-script-names
    (append (if (eq system-type 'windows-nt)
                '("build-nox.bat" "build.bat")
              '("build-nox.sh" "build.sh"))
            '("Makefile"))
    "List of build script names to run when calling `nox/make'.")
  (defvar nox/should-close-compile-window nil)
  (defconst nox/compile-buffer-name "*nox/compilation*")
  (defconst nox/compile-last-make nil)

  (defun nox/make (arg)
    (interactive "P")
    (catch 'break
      (dolist (build-script-name nox/build-script-names)
        (let ((script-dir (locate-dominating-file default-directory build-script-name)))
          (when script-dir
            (let ((file (buffer-file-name))
                  (compilation-buffer-name-function
                   (lambda (mode) nox/compile-buffer-name))
                  (window (get-buffer-window nox/compile-buffer-name)))
              (if (= (length (window-list)) 1)
                  (setq nox/should-close-compile-window t)
                (unless window
                  (setq nox/should-close-compile-window nil)))
              (unless window
                (setq window (display-buffer (get-buffer-create nox/compile-buffer-name) t)))
              (with-selected-window window
                (cd script-dir)
                (if (not (string= build-script-name "Makefile"))
                    (compile (concat "\"" (expand-file-name build-script-name script-dir)
                                     "\" \"" file "\""))
                  (when (or arg (not nox/compile-last-make))
                    (setq nox/compile-last-make
                          (read-string "Command: " nil nil (or nox/compile-last-make "make -k"))))
                  (compile nox/compile-last-make)))
              (throw 'break t)))))))

  (defun nox/bury-compilation-buffer (buffer string)
    "Bury compilation buffer if it succeeded."
    (when (and (string= (buffer-name buffer) nox/compile-buffer-name)
               (string= string "finished\n")
               (save-excursion (not (ignore-errors (compilation-next-error 1 nil 1)))))
      (let ((window (get-buffer-window buffer)))
        (when window
          (if nox/should-close-compile-window
              (delete-window window)
            (switch-to-prev-buffer window)))
        (bury-buffer buffer))))
  (add-hook 'compilation-finish-functions 'nox/bury-compilation-buffer)

  (require 'ansi-color)
  (defun nox/colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook 'nox/colorize-compilation-buffer))

(use-package counsel :ensure t
  :delight (ivy-mode) (counsel-mode)
  :bind (("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . isearch-forward))
  :bind (:map ivy-minibuffer-map
              ("<return>" . ivy-alt-done)
              ("C-j" . ivy-done))
  :bind (:map read-expression-map ("C-r" . counsel-expression-history))

  :init
  (ivy-mode 1)
  (counsel-mode 1)
  (setq-default ivy-height 10
                ivy-use-virtual-buffers t
                ivy-extra-directories nil
                ivy-use-selectable-prompt t
                ivy-count-format "(%d/%d) "
                ivy-virtual-abbreviate 'full
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                        (t . ivy--regex-fuzzy)))
  (bind-key "M-y" (lambda () (interactive) (yank-pop)) ivy-minibuffer-map)
  (add-to-list 'swiper-font-lock-exclude 'c-mode t)
  (add-to-list 'swiper-font-lock-exclude 'c++-mode t)

  (if (executable-find "rg")
      (setq-default counsel-grep-base-command
                    "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

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

(use-package delight :ensure t)

(use-package ivy-hydra :ensure t
  :defer 2)

(use-package dired+
  :ensure t
  :demand t
  :after dired
  :bind (:map dired-mode-map
              ("e" . nox/ediff-files)
              ("Y" . nox/dired-rsync))
  :config
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

(use-package dumb-jump :ensure t
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

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package find-file
  :bind ("C-c o" . ff-find-other-file)
  :config
  (setq-default ff-always-try-to-create t))

(use-package flx :ensure t)

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

(use-package gnuplot :ensure t)

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

(use-package help
  :config
  (setq-default help-window-select t))

(use-package imenu
  :config
  (set 'imenu-auto-rescan-maxout 500000)
  (set 'imenu-auto-rescan t))

(use-package imenu-anywhere :ensure t
  :bind ("C-c i" . nox/ivy-imenu-center)
  :config
  (defun nox/ivy-imenu-center ()
    (interactive)
    (call-interactively 'ivy-imenu-anywhere)
    (recenter-top-bottom)))

(use-package org-noter :ensure t
  :config
  (setq-default org-noter-default-heading-title "Notas da página $p$"))

(use-package magit :ensure t
  :if (executable-find "git")
  :bind ("C-c g" . magit-status)
  :config
  (setq-default magit-completing-read-function 'ivy-completing-read))

(use-package multiple-cursors :ensure t
  :bind (("C-c l" . mc/edit-lines)
         ("M-»" . mc/mark-next-like-this)
         ("M-«" . mc/mark-previous-like-this)
         ("C-M-«" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (unbind-key "M-<down-mouse-1>"))

(use-package org :ensure t
  :bind ("C-c o" . hydra-org/body)
  :init
  (defun nox/org-capture-frame ()
    (modify-frame-parameters nil '((name . "Org Capture")
                                   (org-capture-frame . t)))
    (org-capture))

  :config
  (defhydra hydra-org (:exit t :foreign-keys warn)
    "Org-mode"
    ("c" org-capture "Capture")
    ("a" org-agenda "Agenda")
    ("n" org-noter "Noter")
    ("l" org-store-link "Store link")
    ("q" nil "Quit"))

  (setq-default
   org-directory "~/Personal/Org/"
   org-agenda-files (list (concat org-directory "Inbox.org")
                          (concat org-directory "GTD.org"))
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n@)" "WAITING(w@/!)" "|"
                                 "DONE(d!)" "CANCELED(c@)"))
   org-capture-templates
   '(("t" "Tarefa" entry (file "Inbox.org")
      "* TODO %i%?"))
   org-refile-targets '((nil . (:maxlevel . 6))
                        (org-agenda-files . (:maxlevel . 6)))
   org-agenda-skip-deadline-prewarning-if-scheduled t

   org-modules '(org-id)

   org-startup-indented t
   org-startup-with-inline-images t
   org-image-actual-width '(500)
   org-startup-with-latex-preview t

   org-return-follows-link t
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-catch-invisible-edits 'smart
   org-list-allow-alphabetical t
   org-pretty-entities t

   org-id-link-to-org-use-id 'create-if-interactive

   org-latex-packages-alist '(("" "tikz" t)
                              ("" "mathtools" t))
   org-preview-latex-default-process 'imagemagick
   org-latex-preview-ltxpng-directory (locate-user-emacs-file "Latex Previews/")
   org-format-latex-options
   '(:foreground default :background default :scale 1.7
                 :html-foreground "Black" :html-background "Transparent"
                 :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))

   org-confirm-babel-evaluate nil)

  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'org-hide-block-all)

  (set-face-attribute 'org-block nil :inherit 'default)

  (org-link-set-parameters "pdfview"
                           :follow 'org-pdfview-open
                           :complete 'org-pdfview-complete-link
                           :store 'org-pdfview-store-link)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (octave . t)
     (python . t)
     (latex . t)))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; NOTE(nox): Capture frame related
  (advice-add 'org-switch-to-buffer-other-window :after
              (lambda (&rest _) (when (frame-parameter nil 'org-capture-frame)
                                  (delete-other-windows))))
  (advice-add 'org-capture :around
              (lambda (capture-function &rest args)
                (condition-case nil (apply capture-function args)
                  (error (when (frame-parameter nil 'org-capture-frame)
                           (delete-frame))))))
  (add-hook 'org-capture-after-finalize-hook
            (lambda (&rest _) (when (frame-parameter nil 'org-capture-frame)
                                (delete-frame)))))

(use-package pdf-tools :ensure t
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

  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page
                pdf-cache-image-limit 200))

(use-package paren
  :init
  (show-paren-mode)
  (setq-default show-paren-delay 0))

(use-package recentf
  :init
  (recentf-mode 1)
  (setq-default recentf-auto-cleanup 'never
                recentf-max-saved-items 150
                recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
  (add-hook 'server-visit-hook 'recentf-save-list))

(use-package server
  :init
  (add-hook 'after-make-frame-functions (lambda (frame) (select-frame-set-input-focus frame)) t))

(use-package speedbar
  :config
  (setq-default speedbar-frame-parameters '((minibuffer . t)
                                            (unsplittable . t)
                                            (width . 30)
                                            (border-width . 0)
                                            (left-fringe . 0))))

(use-package tex :ensure auctex)

(use-package tramp
  :config
  (setq-default tramp-default-method "ssh"
                tramp-default-proxies-alist
                '(((regexp-quote (system-name)) nil nil)
                  (nil "\\`root\\'" "/ssh:%h:"))))

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-after-kill-buffer-p t))


;; ------------------------------
;; Modes
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

(use-package go-mode
  :ensure t
  :config
  (setq-default gofmt-command (substitute-in-file-name "$GOPATH/bin/goimports"))
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save nil t))))

(use-package octave
  :mode (("\\.m\\'" . octave-mode))
  :config
  (setq-default inferior-octave-startup-args '("-i" "--line-editing"))
  ;; NOTE(nox): Defining functions on octave sometimes failed without this!
  (add-hook 'inferior-octave-mode-hook (lambda () (setq eldoc-documentation-function nil))))

(use-package web-mode :ensure t
  :mode (("\\.\\(go\\)?html?\\'" . web-mode)))


;; ------------------------------
;; Machine specific settings
(if (file-exists-p private-settings-file)
    (load private-settings-file))
