;;; jumplist.el --- Jump like vim jumplist or ex jumplist

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/jumplist
;; Version: 0.0.2
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: jumplist vim

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'cl-lib)

(defgroup jumplist nil
  "jumplist configuration options."
  :prefix "jumplist"
  :group 'convenience)

(defcustom jumplist-max-length 100
  "Max length of jumplist."
  :type 'integer
  :group 'jumplist)

(defcustom jumplist-ex-mode 'nil
  "Original vim like jumplist or not."
  :type 'boolean
  :group 'jumplist)

(defcustom jumplist-hook-commands '(end-of-buffer beginning-of-buffer find-file)
  "Commands to hook."
  :type 'list
  :group 'jumplist)

(defvar jumplist--list '()
  "Jumplist that save file info.")

(defvar jumplist--idx 0
  "Index of jumplist.")

(defvar jumplist--jumping nil
  "Jumplist state.")

(defvar jumplist--pushing nil
  "Jumplist hook in progress.")

(defvar jumplist--edit-dist 80
  "Jumplist edit distance.")

(defvar jumplist--buffer nil
  "Jumplist buffer.")

(defun jumplist--show-list ()
  (interactive)
  (setq jumplist--buffer (get-buffer-create "*Jump List*"))
  (let ((idx (length jumplist--list)))
    (with-current-buffer jumplist--buffer
      (erase-buffer)
      (while (> idx 0)
        (setq idx (- idx 1))
        (insert (format "%s%d: %s:%d\n"
                       (if (= idx jumplist--idx) "*" " ")
                       idx
                       (car (car (nth idx jumplist--list)))
                       (cdr (car (nth idx jumplist--list))))))
      (beginning-of-buffer))
    (fit-window-to-buffer (display-buffer jumplist--buffer))))

(defun jumplist--do-jump (buff)
  "Do jump to target file and point from BUFF."
  (switch-to-buffer (car (car buff)))
  (goto-char (cdr buff))
  (jumplist--refresh-window))

(defun jumplist--reset-idx ()
  "Reset `jumplist--idx'."
  (setq jumplist--idx 0))

(defun jumplist--last? ()
  "Check `jumplist--idx' is last of list."
  (= jumplist--idx (- (length jumplist--list) 1)))

(defun jumplist--first? ()
  "Check `jumplist--idx' is first of list."
  (= jumplist--idx 0))

(defun jumplist--empty? ()
  "Check `jumplist--list' is empty."
  (= (length jumplist--list) 0))

(defun jumplist--dec-idx ()
  "Descrement `jumplist--idx'."
  (setq jumplist--idx (- jumplist--idx 1)))

(defun jumplist--inc-idx ()
  "Increment `jumplist--idx'."
  (setq jumplist--idx (+ jumplist--idx 1)))

(defun jumplist--drop! (idx)
  "Drop item form list of IDX."
  (setq jumplist--list (nthcdr jumplist--idx jumplist--list)))

(defun jumplist--refresh-window ()
  (if (and jumplist--buffer (get-buffer-window jumplist--buffer)) (jumplist--show-list)))

(defun jumplist--push (pointer)
  "Push POINTER to `jumplist'."
  (while (> (length jumplist--list) jumplist-max-length)
    (nbutlast jumplist--list 1))
  (push pointer jumplist--list)
  (jumplist--reset-idx)
  (jumplist--refresh-window))

(defun jumplist--same-position? (pointer)
  (jumplist--same-position-impl? (cdr pointer) jumplist--list))

(defun jumplist--same-position-impl? (pointer jlist)
  (let ((top-point (car jlist)))
    (cond ((not pointer) nil)
          ((not top-point) nil)
          ((not (cdr top-point)) nil)
          ((eq (marker-position pointer) (marker-position (cdr top-point))) 't))))

(defun jumplist--set ()
  "The record data structure is ((file-name . line-number) . pointer)."
  (interactive)
  (unless jumplist--pushing
    (setq jumplist--pushing t)
      (let ((pointer (cons (cons (buffer-name) (line-number-at-pos)) (point-marker))))
        (unless (jumplist--same-position? pointer)
          (when (and jumplist-ex-mode jumplist--jumping)
            (jumplist--drop! jumplist--idx)
            (setq jumplist--jumping nil)
            (jumplist--reset-idx))
          (unless (jumplist--same-position? pointer)
            (jumplist--push pointer))))
  (setq jumplist--pushing nil)))

(defun jumplist--check-edit-dist (f s)
  (if (jumplist--empty?) (jumplist--set)
      (let ((top-point (marker-position (cdar jumplist--list))))
        (cond ((< jumplist--edit-dist (abs (- f s))) (jumplist--set))
              ((< jumplist--edit-dist (abs (- top-point s))) (jumplist--set))
              ((< jumplist--edit-dist (abs (- top-point f))) (jumplist--set))
              ('t nil)))))
(add-hook 'before-change-functions 'jumplist--check-edit-dist)

(defun jumplist--do-command? (command do-hook-command-list)
  (if do-hook-command-list
      (or
       (eq command (car do-hook-command-list))
       (jumplist--do-command? command (cdr do-hook-command-list)))))

(defun jumplist--command-hook ()
  "Pre command hook that call `jumplist--set' when registerd command hook called."
  (cond
   ((jumplist--do-command? this-command jumplist-hook-commands) (jumplist--set))
   ((and jumplist--jumping               ; when jump and move
         (not (memq this-command '(jumplist-previous jumplist-next))))
    (jumplist--set))))
(add-hook 'pre-command-hook 'jumplist--command-hook)

;;;###autoload
(defun jumplist-previous ()
  "Jump back."
  (interactive)
  (if (or (not jumplist--list)
          (and (not (jumplist--first?))
               (jumplist--last?)))
      (progn (setq jumplist--jumping nil)
      (message "No further undo point."))
    (if jumplist-ex-mode
        (unless jumplist--jumping
          (jumplist--set)
          (setq jumplist--jumping 't)))
    (jumplist--inc-idx)
    (let ((buff (nth jumplist--idx jumplist--list)))
      (jumplist--do-jump buff))))

;;;###autoload
(defun jumplist-next ()
  "Jump forward."
  (interactive)
  (if (or (not jumplist--list)
          (jumplist--first?))
      (progn (setq jumplist--jumping nil)
      (message "No further redo point."))
    (if jumplist-ex-mode
        (unless jumplist--jumping
          (jumplist--set)
          (setq jumplist--jumping 't)))
    (jumplist--dec-idx)
    (let ((buff (nth jumplist--idx jumplist--list)))
      (jumplist--do-jump buff))))

(provide 'jumplist)
;;; jumplist.el ends here
