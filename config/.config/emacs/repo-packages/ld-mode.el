;;; ld-mode-el -- Major mode for editing LD scripts

;; Author: Spencer Nelson <s@spenczar.com>
;; Created: 29 Dec 2015

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is used for editing LD scripts.

;;; Code:

(defvar ld-mode-hook nil)
(defvar ld-mode-map
  (let (map (make-sparse-keymap))
    ;; Example:   (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for LD major mode.")

(defconst ld--keywords
  '(
    ":PHDR"
    "AFTER"
    "ASSERT"
    "AS_NEEDED"
    "AT"
    "BEFORE"
    "BYTE"
    "COMMON"
    "CONSTRUCTORS"
    "COPY"
    "CREATE_OBJECT_SYMBOLS"
    "DSECT"
    "ENTRY"
    "EXTERN"
    "FILEHDR"
    "FILL"
    "FLAGS"
    "FORCE_COMMON_ALLOCATION"
    "GROUP"
    "INCLUDE"
    "INFO"
    "INHIBIT_COMMON_ALLOCATION"
    "INPUT"
    "INSERT"
    "KEEP"
    "LD_FEATURE"
    "LONG"
    "MEMORY"
    "NOCROSSREFS"
    "NOLOAD"
    "ONLY_IF_RO"
    "ONLY_IF_RW"
    "OUTPUT"
    "OUTPUT_ARCH"
    "OUTPUT_FORMAT"
    "OVERLAY"
    "PHDRS"
    "PROVIDE"
    "PROVIDE_HIDDEN"
    "PT_DYNAMIC"
    "PT_INTERP"
    "PT_LOAD"
    "PT_NOTE"
    "PT_NULL"
    "PT_PHDR"
    "PT_SHLIB"
    "QUAD"
    "REGION_ALIAS"
    "SEARCH_DIR"
    "SECTIONS"
    "SHORT"
    "SORT"
    "SORT_BY_ALIGNMENT"
    "SORT_BY_INIT_PRIORITY"
    "SORT_BY_NAME"
    "SQUAD"
    "STARTUP"
    "SUBALIGN"
    "TARGET"
    "VERSION"
    "__CTOR_END__"
    "__CTOR_LIST__"
    "__DTOR_END__"
    "__DTOR_LIST__"
    ))


(defconst ld--builtins
  '("ABSOLUTE"
    "ADDR"
    "ALIGN"
    "BLOCK"
    "DATA_SEGMENT_ALIGN"
    "DATA_SEGMENT_END"
    "DATA_SEGMENT_RELRO_END"
    "DEFINED"
    "LENGTH"
    "len"
    "l"
    "LOADADDR"
    "MAX"
    "MIN"
    "NEXT"
    "ORIGIN"
    "org"
    "o"
    "SEGMENT_START"
    "SIZEOF"
    "SIZEOF_HEADERS"
    "sizeof_headers"))

(defconst ld--section-regexp "\\.\\w*"
  "Regular expression to match sections, which look like `.text` or `.bss`.")

(defconst ld--wildcard-section-regexp
  (concat "\\(\\*\\)(" ld--section-regexp ")"))

(defconst ld--comment-start "/\\*")
(defconst ld--comment-end "\\*/")

(defconst ld--comment-regexp (concat ld--comment-start "\\(.*\\)" ld--comment-end) )

(defconst ld--hex-address-regexp "0x[[:digit:]]+")

(defun ld--word (str)
  "Wrap STR with \< and \> to make it break on word boundaries."
  (concat "\\<" str "\\>"))

(defvar ld-font-lock-keywords
  (append
   `((,(ld--word (regexp-opt ld--keywords t)) . font-lock-keyword-face)
     (,(ld--word (regexp-opt ld--builtins t)) . font-lock-builtin-face)
     (,ld--wildcard-section-regexp 1 font-lock-variable-name-face)
     (,ld--section-regexp . font-lock-variable-name-face)
     (,(ld--word ld--hex-address-regexp) . font-lock-constant-face)
     ("/DISCARD/\\|EXCLUDE_FILE\\|:NONE" . font-lock-warning-face)
     )
   )
  )

(defvar ld-mode-syntax-table
  (let ((st (make-syntax-table)))
                                        ; Comments are like /* */
    (modify-syntax-entry ?/ ". 14b" st)
    (modify-syntax-entry ?* ". 23b" st)
    st))

(defgroup ld nil
  "Major mode for editing LD scripts"
  :group 'languages)

(defcustom ld-mode-indent-width 4
  "Number of columns to indent in ld-mode."
  :type 'integer
  :group 'ld)

(defun ld--in-comment ()
  "Is the current point within a comment?"
  (nth 8 (syntax-ppss)))

(defun ld--move-to-opening-brace ()
  "Move point to the opening enclosing brace."
  (condition-case nil
      (while (or (not (looking-at "{"))
                 (ld--in-comment))
        (backward-up-list))
    (scan-error (goto-char (point-min)))))

(defun ld--desired-indentation ()
  "Compute the desired indentation level at point."
  (save-excursion
    (back-to-indentation)
    (let ((started-on-close-brace (looking-at "[[:space:]]*}.*"))
          (start-line (line-number-at-pos)))
      (ld--move-to-opening-brace)
      (cond
       ;; If we moved to the top of the file, indent to 0
       ((bobp) 0)
       ;; If we didnt change lines, then we started on an opening-brace's
       ;; line. Use the previous line's desired indentation.
       ((= (line-number-at-pos) start-line)
        (forward-line -1)
        (ld--desired-indentation))
       ;; If we started on a closing brace, we moved back to the opening brace;
       ;; use the opening brace's indentation.
       (started-on-close-brace
        (current-indentation))
       ;; Else, we started within a brace; use the parent's indentation + tab
       ;; width.
       (t (+ (current-indentation) ld-mode-indent-width))))))


(defun ld-indent-line ()
  "Indent line of LD script."
  (interactive)
  (indent-line-to (ld--desired-indentation)))

(defun ld-mode ()
  "Major mode for editing LD script files."
  (interactive)
  (kill-all-local-variables)

  ;; Syntax
  (set-syntax-table ld-mode-syntax-table)

  ;; Font lock
  (set (make-local-variable 'font-lock-defaults) '(ld-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'ld-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "/*")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "/\\*+\s *")

  (setq major-mode 'ld-mode)
  (setq mode-name "LD2")
  (run-hooks 'ld-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.ld\\'" 'ld-mode))

(provide 'ld-mode)

;;; ld-mode.el ends here
