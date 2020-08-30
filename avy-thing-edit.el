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
  (thing-edit 'sexp t))

;;;###autoload
(defun avy-thing-copy-sexp (kill-conditional)
  "Copy sexp at current point.
With the universal argument, the text will also be killed."
  (interactive "P")
  (thing-edit 'sexp kill-conditional))

;;;###autoload
(defun avy-thing-replace-sexp ()
  "Replace sexp at current point with the content of kill-ring."
  (interactive)
  (thing-replace 'sexp))

;;;###autoload
(defun avy-thing-cut-email ()
  "Cut email at current point."
  (interactive)
  (thing-edit 'email t))

;;;###autoload
(defun avy-thing-copy-email (kill-conditional)
  "Copy email at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'email kill-conditional))

;;;###autoload
(defun avy-thing-replace-email ()
  "Replace email at current point with the content kill ring."
  (interactive)
  (thing-replace 'email))

;;;###autoload
(defun avy-thing-cut-filename ()
  "Cut filename at current point."
  (interactive)
  (thing-edit 'filename t))

;;;###autoload
(defun avy-thing-copy-filename (kill-conditional)
  "Copy filename at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'filename kill-conditional))

;;;###autoload
(defun avy-thing-replace-filename ()
  "Replace filename at current point with kill ring."
  (interactive)
  (thing-replace 'filename))

;;;###autoload
(defun avy-thing-cut-url ()
  "Cut url at current point."
  (interactive)
  (thing-edit 'url t))

;;;###autoload
(defun avy-thing-copy-url (kill-conditional)
  "Copy url at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'url kill-conditional))

;;;###autoload
(defun avy-thing-replace-url ()
  "Replace url at current point with kill ring."
  (interactive)
  (thing-replace 'url))

;;;###autoload
(defun avy-thing-cut-word ()
  "Cut words at point."
  (interactive)
  (thing-edit 'word t))

;;;###autoload
(defun avy-thing-copy-word (kill-conditional)
  "Copy words at point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'word kill-conditional))

;;;###autoload
(defun avy-thing-replace-word ()
  "Replace words at point with kill ring."
  (interactive)
  (thing-replace 'word))

;;;###autoload
(defun avy-thing-cut-symbol ()
  "Cut symbol around point."
  (interactive)
  (thing-edit 'symbol t))

;;;###autoload
(defun avy-thing-copy-symbol (kill-conditional)
  "Copy symbol around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'symbol kill-conditional))

;;;###autoload
(defun avy-thing-replace-symbol ()
  "Replace symbol around point with kill ring."
  (interactive)
  (thing-replace 'symbol))

;;;###autoload
(defun avy-thing-cut-line ()
  "Cut current line into Kill-Ring without mark the line."
  (interactive)
  (thing-edit 'line t))

;;;###autoload
(defun avy-thing-copy-line (kill-conditional)
  "Copy current line into Kill-Ring without mark the line.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'line kill-conditional))

;;;###autoload
(defun avy-thing-replace-line ()
  "Replace current line with kill ring"
  (interactive)
  (thing-replace 'line))

;;;###autoload
(defun avy-thing-copy-paragraph (kill-conditional)
  "Copy current paragraph around the point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'paragraph kill-conditional))

;;;###autoload
(defun avy-thing-replace-paragraph ()
  "Replace current paragraph around the point with the content of kill ring."
  (interactive)
  (thing-replace 'paragraph))

;;;###autoload
(defun avy-thing-cut-paragraph (&optional kill-conditional)
  "Cut current paragraph around the point"
  (interactive)
  (thing-edit 'paragraph t))

;;;###autoload
(defun avy-thing-cut-defun ()
  "Cut function around point."
  (interactive)
  (thing-edit 'defun t))

;;;###autoload
(defun avy-thing-copy-defun (kill-conditional)
  "Cut function around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'defun kill-conditional))

;;;###autoload
(defun avy-thing-replace-defun ()
  "Replace function around point with the content of kill ring."
  (interactive)
  (thing-replace 'defun))

;;;###autoload
(defun avy-thing-cut-list ()
  "Cut list around point."
  (interactive)
  (thing-edit 'list t))

;;;###autoload
(defun avy-thing-copy-list (kill-conditional)
  "Cut list around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'list kill-conditional))

;;;###autoload
(defun avy-thing-replace-list ()
  "Replace list around point with the content of kill ring."
  (interactive)
  (thing-replace 'list))

;;;###autoload
(defun avy-thing-cut-sentence ()
  "Cut sentence around point."
  (interactive)
  (thing-edit 'sentence t))

;;;###autoload
(defun avy-thing-copy-sentence (kill-conditional)
  "Cut sentence around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'sentence kill-conditional))

;;;###autoload
(defun avy-thing-replace-sentence ()
  "Replace sentence around point with the content of currnt line."
  (interactive)
  (thing-replace 'sentence))

;;;###autoload
(defun avy-thing-cut-whitespace ()
  "Cut whitespace around point."
  (interactive)
  (thing-edit 'whitespace t))

;;;###autoload
(defun avy-thing-copy-whitespace (kill-conditional)
  "Cut whitespace around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'whitespace kill-conditional))

;;;###autoload
(defun avy-thing-replace-whitespace ()
  "Replace whitespace around point with the content of currnt line."
  (interactive)
  (thing-replace 'whitespace))

;;;###autoload
(defun avy-thing-cut-page ()
  "Cut page around point."
  (interactive)
  (thing-edit 'page t))

;;;###autoload
(defun avy-thing-copy-page (kill-conditional)
  "Cut page around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (thing-edit 'page kill-conditional))

;;;###autoload
(defun avy-thing-replace-page ()
  "Replace page around point with the content of currnt line."
  (interactive)
  (thing-replace 'page))

;; Below function is not base on thingatpt, but it's effect like above function.
;; So i add to this package.
;;;###autoload
(defun avy-thing-cut-to-line-end ()
  "Cut content from current point to line end."
  (interactive)
  (thing-copy-to-line-end t))

;;;###autoload
(defun avy-thing-copy-to-line-end (&optional kill-conditional)
  "Copy content from current point to line end.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (point)
                         (line-end-position)
                         kill-conditional)))

;;;###autoload
(defun avy-thing-cut-to-line-beginning ()
  "Cut content from current point to line beginning."
  (interactive)
  (thing-copy-to-line-beginning t))

;;;###autoload
(defun avy-thing-copy-to-line-beginning (&optional kill-conditional)
  "Copy content from current point tot line beginning.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (line-beginning-position)
                         (point)
                         kill-conditional)))

;;;###autoload
(defun avy-thing-cut-comment ()
  "Cut the comment around line.
If mark is active, it can cut all comment that in mark."
  (interactive)
  (thing-copy-comment t))

;;;###autoload
(defun avy-thing-copy-comment (&optional kill-conditional)
  "Copy the comment around line.
If mark is active, it can copy all comment that in mark.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
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
          (goto-char end))))))

(defun avy-thing-comment-copy (arg)
  "Copy the first comment on this line, if any.
With prefix ARG, copy comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_ (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (kill-ring-save cs (if (bolp) (1- (point)) (point)))
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

;;;###autoload
(defun avy-thing-cut-number ()
  "Cut number at point."
  (interactive)
  (thing-copy-number t))

;;;###autoload
(defun avy-thing-copy-number (kill-conditional)
  "Copy number at point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (save-excursion
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
    (when (thing-at-point-looking-at "-?[0-9]+\\.?[0-9]*" 500)
      (thing-replace-internal
       (match-beginning 0)
       (match-end 0)))))

(defun avy-thing-cut-parentheses ()
  "Cut content in match parentheses."
  (interactive)
  (thing-copy-parentheses t))

(defun avy-thing-copy-parentheses (kill-conditional)
  "Copy content in match parentheses.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive "P")
  (save-excursion
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

(defun avy-thing-edit-in-string-p (&optional state)
  (or (nth 3 (or state (thing-edit-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun avy-thing-edit-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `thing-edit-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    (let ((start (nth 8 (or state (thing-edit-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun avy-thing-edit-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun avy-thing-copy-region-or-line (&optional kill-conditional)
  "Copy content of the current region or line.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (let* ((active (region-active-p))
           (pos (or (and active (region-beginning))
                    (line-beginning-position)))
           (pos-end (or (and active (region-end))
                        (line-end-position))))
      (thing-edit-internal pos pos-end kill-conditional))))

(defun avy-thing-cut-region-or-line ()
  "Cut content of the current region or line."
  (interactive)
  (thing-copy-region-or-line t))

(defun avy-thing-replace-region-or-line ()
  "Replace the current region or line with the content."
  (interactive)
  (save-excursion
    (let* ((active (region-active-p))
           (pos (or (and active (region-beginning))
                    (line-beginning-position)))
           (pos-end (or (and active (region-end))
                        (line-end-position))))
      (thing-replace-internal pos pos-end))))

(defun avy-thing-copy-whole-buffer (&optional kill-conditional)
  "Copy content of the current buffer.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (point-min) (point-max) kill-conditional)))

(defun avy-thing-cut-whole-buffer ()
  "Cut content of the current buffer."
  (interactive)
  (thing-copy-whole-buffer t))

(defun avy-thing-replace-whole-buffer ()
  "Replace the current buffer with the content."
  (interactive)
  (thing-replace 'buffer))

(provide 'avy-thing-edit)
;;; avy-thing-edit.el ends here
