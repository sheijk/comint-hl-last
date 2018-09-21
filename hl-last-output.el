;;; comint-hl-last.el --- Highlight output of last command in fringe
;;
;; Copyright 2015 Jan Rehders
;; 
;; Author: Jan Rehders <jan@sheijk.net>
;; URL: https://github.com/sheijk/comint-hl-last
;; Version: 0.3
;; Created: 2015-02-11
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This defines a minor mode which will highlight the output of the last command
;; by a line in the fringe. It works with all comint derived modes. Call
;; comint-hl-last-mode or global-comint-hl-last-mode and highlighting will
;; appear in shells, gud, etc.
;;
;;; Changelog
;;
;; v0.3, 2015-02-22
;; - Mini bug fix when mode was started in empty comint buffers.
;; - Updated package meta data.
;; - Added screen shots to readme.
;;
;; v0.2, 2015-02-12
;; - Mode can only be enabled in major modes derived from comint-mode.
;; - Toggling global mode will add/remove display in all buffers.
;; - Do not highlight last input line.
;;
;;; Tests
;;
;; For now only be hand. Check the following:
;; - Basic functionality
;;   - M-x, shell, M-x comint-hl-mode, enter several commands and check that the
;;     output of the last command gets highlighted each time in the fringe.
;;   - M-x, comint-hl-mode, check that highlighting gets removed.
;;
;; - Global mode
;;   - M-x, shell, pwd, ls, M-x, rename-buffer, *shell2*, M-x, shell, ls, pwd,
;;     M-x, global-comint-hl-last-mode, check highlighting is in both shells
;;   - M-x, global-comint-hl-last-mode, check if highlighting gets removed in
;;     both shells.
;;
;;; Code:

(require 'fringe-helper)

(defvar comint-hl-last-highlight nil "Helper to remove highlight of last output.")
(make-variable-buffer-local 'comint-hl-last-highlight)

(fringe-helper-define 'comint-hl-last-marker '(center t)
  "XX......"
  "XX......"
  "XX......"
  "XX......")

(defgroup comint-hl-last
  nil
  "Customization group for comint-hl-last."
  :group 'comint)

(defface comint-hl-last-marker-face
  '((t (:inherit comint-highlight-prompt)))
  "Face used to highlight the fringe on folded regions"
  :group 'comint-hl-last)

(defun comint-hl-last-remove ()
  "Remove marker from fringe."
  (interactive)
  (when comint-hl-last-highlight
    (fringe-helper-remove comint-hl-last-highlight)))

(defmacro comint-hl-by-shell (postfix)
  "Used to choose between similarly named symbols for comint and eshell.

Will expand into code which will select either eshell-postfix or
comint-postfix depending on which of the shells we're running in.

`postfix' can be a symbol or a quoted symbol. If it its quoted
the returned symbol will also be quoted."
  (pcase postfix
    (`(quote ,symbol)
     `(if (derived-mode-p 'comint-mode)
          ',(symbol-append 'comint- (symbol-name symbol))
        ',(symbol-append 'eshell- (symbol-name symbol))))

    ((guard (atom postfix))
     `(if (derived-mode-p 'comint-mode)
          ,(symbol-append 'comint- (symbol-name postfix))
        ,(symbol-append 'eshell- (symbol-name postfix))))

    (invalid
     (error "invalid form passed to comint-hl-by-shell: %s" invalid))))

(defun comint-hl-last-update (&optional _)
  "Update the highlighting.

Will be added to `comint-output-filter-functions' when mode is active.
`_' is ignored."
  (comint-hl-last-remove)
  (when (comint-hl-by-shell last-input-start)
    (setq comint-hl-last-highlight
          (fringe-helper-insert-region (save-excursion
                                         (goto-char (comint-hl-by-shell last-input-start))
                                         (next-line)
                                         (beginning-of-line 1)
                                         (point))
                                       (point-max)
                                       'comint-hl-last-marker
                                       'left-fringe
                                       'comint-hl-last-marker-face)))
  nil)

(defun comint-hl-last-toggle (global)
  "Toggle either global-comint-hl-last-mode or comint-hl-last-mode.

GLOBAL decides between global and local mode."
  (let ((turned-on (if global global-comint-hl-last-mode comint-hl-last-mode)))
    (cond
     ((and global turned-on)
      (add-hook (comint-hl-by-shell 'output-filter-functions) 'comint-hl-last-update t nil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (or (derived-mode-p 'comint-mode)
                    (derived-mode-p 'eshell-mode))
            (comint-hl-last-update "")))))

     ((and global (not turned-on))
      (remove-hook (comint-hl-by-shell 'output-filter-functions) 'comint-hl-last-update nil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (or (derived-mode-p 'comint-mode)
                    (derived-mode-p 'eshell-mode))
            (comint-hl-last-remove)))))

     ((and (not global) turned-on)
      (add-hook (comint-hl-by-shell 'output-filter-functions) 'comint-hl-last-update t t)
      (comint-hl-last-update ""))

     ((and (not global) (not turned-on))
      (comint-hl-last-remove)
      (remove-hook (comint-hl-by-shell 'comint-output-filter-functions)
                   'comint-hl-last-update
                   (not global))))))

;;;###autoload
(define-minor-mode comint-hl-last-mode
  "Minor mode to highlight the output of the last command in the fringe."
  :init-value nil
  :require 'comint
  :group 'comint-hl-last
  :lighter " hll"
  :version "0.1"

  (comint-hl-last-toggle nil))

;;;###autoload
(define-minor-mode global-comint-hl-last-mode
  "Minor mode to highlight the output of the last command in the fringe."
  :init-value nil
  :require 'comint
  :group 'comint-hl-last
  :global t
  :lighter " hll"
  :version "0.1"

  (comint-hl-last-toggle t))

(provide 'comint-hl-last)
;;; comint-hl-last ends here
