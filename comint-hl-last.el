;;; comint-hl-last.el --- Highlight output of last command in fringe

;; Copyright 2015 Jan Rehders
;; 
;; Author: Jan Rehders <cmdkeen@gmx.de>
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This defines a minor mode which will highlight the output of the last command
;; by a line in the fringe. It works with all comint derived modes. Call
;; comint-hl-last-mode or global-comint-hl-last-mode and highlighting will
;; appear in shells, gud, etc.

(defvar comint-hl-last-highlight nil "Helper to remove highlight of last output")
(make-variable-buffer-local 'comint-hl-last-highlight)

(require 'fringe-helper)

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

(defun comint-hl-last-update (arg)
  "Updates the highlighting. Will be added to `comint-output-filter-functions'
when mode is active."
  (comint-hl-last-remove)
  (setq comint-hl-last-highlight
        (fringe-helper-insert-region comint-last-input-start (point-max) 'comint-hl-last-marker 'left-fringe 'comint-hl-last-marker-face)))

(defun comint-hl-last-toggle (global)
  "Toggles either global-comint-hl-last-mode or comint-hl-last-mode."
  (if (if global global-comint-hl-last-mode comint-hl-last-mode)
      (progn
        (add-hook 'comint-output-filter-functions 'comint-hl-last-update t (not global))
        (unless global
          (comint-hl-last-update "")))
    (unless global
      (comint-hl-last-remove))
    (remove-hook 'comint-output-filter-functions 'comint-hl-last-update (not global))))

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

