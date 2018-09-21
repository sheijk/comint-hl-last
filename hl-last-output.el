;;; hl-last-output.el --- Highlight output of last command in fringe
;;
;; Copyright 2015-2018 Jan Rehders
;; 
;; Author: Jan Rehders <jan@sheijk.net>
;; URL: https://github.com/sheijk/hl-last-output
;; Version: 0.4
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
;; hl-last-output-mode or global-hl-last-output-mode and highlighting will
;; appear in shells, gud, etc.
;;
;;; Changelog
;;
;; v0.4, 2018-09-21
;; - Support eshell as well as comint based modes
;; - Renamed to hl-last-output
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
;;     M-x, global-hl-last-output-mode, check highlighting is in both shells
;;   - M-x, global-hl-last-output-mode, check if highlighting gets removed in
;;     both shells.
;;
;;; Code:

(require 'fringe-helper)

(defvar hl-last-output-highlight nil "Helper to remove highlight of last output.")
(make-variable-buffer-local 'hl-last-output-highlight)

(fringe-helper-define 'hl-last-output-marker '(center t)
  "XX......"
  "XX......"
  "XX......"
  "XX......")

(defgroup hl-last-output
  nil
  "Customization group for hl-last-output."
  :group 'comint)

(defface hl-last-output-marker-face
  '((t (:inherit comint-highlight-prompt)))
  "Face used to highlight the fringe on folded regions"
  :group 'hl-last-output)

(defun hl-last-output-remove ()
  "Remove marker from fringe."
  (interactive)
  (when hl-last-output-highlight
    (fringe-helper-remove hl-last-output-highlight)))

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

(defun hl-last-output-update (&optional _)
  "Update the highlighting.

Will be added to `comint-output-filter-functions' when mode is active.
`_' is ignored."
  (hl-last-output-remove)
  (when (comint-hl-by-shell last-input-start)
    (setq hl-last-output-highlight
          (fringe-helper-insert-region (save-excursion
                                         (goto-char (comint-hl-by-shell last-input-start))
                                         (next-line)
                                         (beginning-of-line 1)
                                         (point))
                                       (point-max)
                                       'hl-last-output-marker
                                       'left-fringe
                                       'hl-last-output-marker-face)))
  nil)

(defun hl-last-output-toggle (global)
  "Toggle either global-hl-last-output-mode or hl-last-output-mode.

GLOBAL decides between global and local mode."
  (let ((turned-on (if global global-hl-last-output-mode hl-last-output-mode)))
    (cond
     ((and global turned-on)
      (add-hook (comint-hl-by-shell 'output-filter-functions) 'hl-last-output-update t nil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (or (derived-mode-p 'comint-mode)
                    (derived-mode-p 'eshell-mode))
            (hl-last-output-update "")))))

     ((and global (not turned-on))
      (remove-hook (comint-hl-by-shell 'output-filter-functions) 'hl-last-output-update nil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (or (derived-mode-p 'comint-mode)
                    (derived-mode-p 'eshell-mode))
            (hl-last-output-remove)))))

     ((and (not global) turned-on)
      (add-hook (comint-hl-by-shell 'output-filter-functions) 'hl-last-output-update t t)
      (hl-last-output-update ""))

     ((and (not global) (not turned-on))
      (hl-last-output-remove)
      (remove-hook (comint-hl-by-shell 'comint-output-filter-functions)
                   'hl-last-output-update
                   (not global))))))

;;;###autoload
(define-minor-mode hl-last-output-mode
  "Minor mode to highlight the output of the last command in the fringe."
  :init-value nil
  :require 'comint
  :group 'hl-last-output
  :lighter " hll"
  :version "0.1"

  (hl-last-output-toggle nil))

;;;###autoload
(define-minor-mode global-hl-last-output-mode
  "Minor mode to highlight the output of the last command in the fringe."
  :init-value nil
  :require 'comint
  :group 'hl-last-output
  :global t
  :lighter " hll"
  :version "0.1"

  (hl-last-output-toggle t))

(provide 'hl-last-output)
;;; hl-last-output ends here
