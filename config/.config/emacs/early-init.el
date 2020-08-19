;; early-init.el -*- lexical-binding: t -*-
;; Author: weirdNox (Gonçalo Santos)

(defvar nox--file-name-handler-alist file-name-handler-alist)

(defun nox|reset-temporary-init-values ()
  "Resets garbage collection settings to reasonable defaults (if you don't do
this, you'll get stuttering and random freezes) and resets `file-name-handler-alist'."
  (setq file-name-handler-alist nox--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.15))

(unless after-init-time
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 1.0
        file-name-handler-alist nil))
(add-hook 'emacs-startup-hook #'nox|reset-temporary-init-values)

(setq package-enable-at-startup nil)

(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
