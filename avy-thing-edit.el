;;; avy-thing-edit.el --- Extension thing edit

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Author: lyjdwh <lyjdwh@gmail.com>
;; Maintainer: lyjdwh <lyjdwh@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2014, Arthur Miller <arthur.miller@live.com>, all rights reserved.
;; Copyright (C) 2020, lyjdwh <lyjdwh@gmail.com>, all rights reserved.
;; Created: 2020-08-30 20:18
;; Version: 1.0

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Require
(require 'thingatpt)
(require 'thing-edit)

;;;###autoload
(defun avy-thing-cut-sexp ()
  "Cut sexp at current point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'sexp t)))

;;;###autoload
(defun avy-thing-copy-sexp (kill-conditional)
  "Copy sexp at current point.
With the universal argument, the text will also be killed."
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'sexp kill-conditional)))

;;;###autoload
(defun avy-thing-replace-sexp ()
  "Replace sexp at current point with the content of kill-ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'sexp)))

;;;###autoload
(defun avy-thing-cut-email ()
  "Cut email at current point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'email t)))

;;;###autoload
(defun avy-thing-copy-email (kill-conditional)
  "Copy email at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'email kill-conditional)))

;;;###autoload
(defun avy-thing-replace-email ()
  "Replace email at current point with the content kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'email)))

;;;###autoload
(defun avy-thing-cut-filename ()
  "Cut filename at current point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'filename t)))

;;;###autoload
(defun avy-thing-copy-filename (kill-conditional)
  "Copy filename at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'filename kill-conditional)))

;;;###autoload
(defun avy-thing-replace-filename ()
  "Replace filename at current point with kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'filename)))

;;;###autoload
(defun avy-thing-cut-url ()
  "Cut url at current point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'url t)))

;;;###autoload
(defun avy-thing-copy-url (kill-conditional)
  "Copy url at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'url kill-conditional)))

;;;###autoload
(defun avy-thing-replace-url ()
  "Replace url at current point with kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'url)))

;;;###autoload
(defun avy-thing-cut-word ()
  "Cut words at point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'word t)))

;;;###autoload
(defun avy-thing-copy-word (kill-conditional)
  "Copy words at point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'word kill-conditional)))

;;;###autoload
(defun avy-thing-replace-word ()
  "Replace words at point with kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'word)))

;;;###autoload
(defun avy-thing-cut-symbol ()
  "Cut symbol around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'symbol t)))

;;;###autoload
(defun avy-thing-copy-symbol (kill-conditional)
  "Copy symbol around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'symbol kill-conditional)))

;;;###autoload
(defun avy-thing-replace-symbol ()
  "Replace symbol around point with kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'symbol)))

;;;###autoload
(defun avy-thing-cut-line ()
  "Cut current line into Kill-Ring without mark the line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'line t)))

;;;###autoload
(defun avy-thing-copy-line (kill-conditional)
  "Copy current line into Kill-Ring without mark the line.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'line kill-conditional)))

;;;###autoload
(defun avy-thing-replace-line ()
  "Replace current line with kill ring"
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'line)))

;;;###autoload
(defun avy-thing-copy-paragraph (kill-conditional)
  "Copy current paragraph around the point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'paragraph kill-conditional)))

;;;###autoload
(defun avy-thing-replace-paragraph ()
  "Replace current paragraph around the point with the content of kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'paragraph)))

;;;###autoload
(defun avy-thing-cut-paragraph (&optional kill-conditional)
  "Cut current paragraph around the point"
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'paragraph t)))

;;;###autoload
(defun avy-thing-cut-defun ()
  "Cut function around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'defun t)))

;;;###autoload
(defun avy-thing-copy-defun (kill-conditional)
  "Cut function around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'defun kill-conditional)))

;;;###autoload
(defun avy-thing-replace-defun ()
  "Replace function around point with the content of kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'defun)))

;;;###autoload
(defun avy-thing-cut-list ()
  "Cut list around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'list t)))

;;;###autoload
(defun avy-thing-copy-list (kill-conditional)
  "Cut list around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'list kill-conditional)))

;;;###autoload
(defun avy-thing-replace-list ()
  "Replace list around point with the content of kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'list)))

;;;###autoload
(defun avy-thing-cut-sentence ()
  "Cut sentence around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'sentence t)))

;;;###autoload
(defun avy-thing-copy-sentence (kill-conditional)
  "Cut sentence around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'sentence kill-conditional)))

;;;###autoload
(defun avy-thing-replace-sentence ()
  "Replace sentence around point with the content of currnt line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'sentence)))

;;;###autoload
(defun avy-thing-cut-whitespace ()
  "Cut whitespace around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'whitespace t)))

;;;###autoload
(defun avy-thing-copy-whitespace (kill-conditional)
  "Cut whitespace around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'whitespace kill-conditional)))

;;;###autoload
(defun avy-thing-replace-whitespace ()
  "Replace whitespace around point with the content of currnt line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'whitespace)))

;;;###autoload
(defun avy-thing-cut-page ()
  "Cut page around point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'page t)))

;;;###autoload
(defun avy-thing-copy-page (kill-conditional)
  "Cut page around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit 'page kill-conditional)))

;;;###autoload
(defun avy-thing-replace-page ()
  "Replace page around point with the content of currnt line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-replace 'page)))

;; Below function is not base on thingatpt, but it's effect like above function.
;; So i add to this package.
;;;###autoload
(defun avy-thing-cut-to-line-end ()
  "Cut content from current point to line end."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-to-line-end t)))

;;;###autoload
(defun avy-thing-copy-to-line-end (&optional kill-conditional)
  "Copy content from current point to line end.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit-internal (point)
                         (line-end-position)
                         kill-conditional)))

;;;###autoload
(defun avy-thing-cut-to-line-beginning ()
  "Cut content from current point to line beginning."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-to-line-beginning t)))

;;;###autoload
(defun avy-thing-copy-to-line-beginning (&optional kill-conditional)
  "Copy content from current point tot line beginning.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-edit-internal (line-beginning-position)
                         (point)
                         kill-conditional)))

;;;###autoload
(defun avy-thing-cut-comment ()
  "Cut the comment around line.
If mark is active, it can cut all comment that in mark."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-comment t)))

;;;###autoload
(defun avy-thing-copy-comment (&optional kill-conditional)
  "Copy the comment around line.
If mark is active, it can copy all comment that in mark.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (when mark-active
        (setq beg (region-beginning))
        (setq end (region-end))
        (deactivate-mark))
      (save-excursion
        (setq end (copy-marker end))
        (goto-char beg)
        (while (< (point) end)
          (if (comment-search-forward end t)
              (if kill-conditional
                  (call-interactively 'comment-kill)
                (call-interactively 'thing-comment-copy))
            (goto-char end)))))))

;;;###autoload
(defun avy-thing-cut-number ()
  "Cut number at point."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-number t)))

;;;###autoload
(defun avy-thing-copy-number (kill-conditional)
  "Copy number at point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (when (thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)
      (thing-edit-internal
       (match-beginning 0)
       (match-end 0)
       kill-conditional))))

;;;###autoload
(defun avy-thing-replace-number ()
  "Replace number at point with kill ring."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (when (thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)
      (thing-replace-internal
       (match-beginning 0)
       (match-end 0)))))

(defun avy-thing-cut-parentheses ()
  "Cut content in match parentheses."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-parentheses t)))

(defun avy-thing-copy-parentheses (kill-conditional)
  "Copy content in match parentheses.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive "P")
  (save-excursion
    (evil-avy-goto-char-2)
    (if (thing-edit-in-string-p)
        (thing-edit-internal
         (1+ (car (thing-edit-string-start+end-points)))
         (cdr (thing-edit-string-start+end-points))
         kill-conditional)
      (thing-edit-internal
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))
       kill-conditional))))

(defun avy-thing-replace-parentheses ()
  "Replace content in match parentheses with the content of currnt line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (if (thing-edit-in-string-p)
        (thing-replace-internal
         (1+ (car (thing-edit-string-start+end-points)))
         (cdr (thing-edit-string-start+end-points)))
      (thing-replace-internal
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

(defun avy-thing-copy-region-or-line (&optional kill-conditional)
  "Copy content of the current region or line.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (let* ((active (region-active-p))
           (pos (or (and active (region-beginning))
                    (line-beginning-position)))
           (pos-end (or (and active (region-end))
                        (line-end-position))))
      (thing-edit-internal pos pos-end kill-conditional))))

(defun avy-thing-cut-region-or-line ()
  "Cut content of the current region or line."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (thing-copy-region-or-line t)))

(defun avy-thing-replace-region-or-line ()
  "Replace the current region or line with the content."
  (interactive)
  (save-excursion
    (evil-avy-goto-char-2)
    (let* ((active (region-active-p))
           (pos (or (and active (region-beginning))
                    (line-beginning-position)))
           (pos-end (or (and active (region-end))
                        (line-end-position))))
      (thing-replace-internal pos pos-end))))

(provide 'avy-thing-edit)
;;; avy-thing-edit.el ends here
