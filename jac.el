;;; jac.el --- Just another buffer cloning method.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  DREWOR020

;; Author: DREWOR020 <toz@smtp.1und1.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;;; Installation

;; Put this file into `load-path'.  Maybe, byte-compile.
;; Add the following line to your init file:
;; (autoload 'jac "jac")
;; Restart Emacs or re-evaluate the init file.

;;;; Usage

;; Call the command `jac' for cloning the current buffer.

;; Both buffers have the same text contents.  If you edit in the
;; original buffer the modifications are transferred to the clone and
;; visa-versa.  Text properties are not transferred.  That means you
;; can switch the major mode for the clone.

;;; Code:

(defvar-local jac-clones nil
  "List of clones that need to be adapted to the original buffer.")
(put 'jac-clones 'permanent-local t)

(defun jac-before-change (b e)
  "Kill region betweenn B and E in the clones."
  (let ((clones jac-clones))
    (cl-loop for buf in jac-clones do
	     (if (buffer-live-p buf)
		 (with-current-buffer buf
		   (let ((inhibit-modification-hooks t))
		     (delete-region b e)))
	       (setq clones (cl-remove buf clones))))
    (setq jac-clones clones)))

(defun jac-after-change (b e len)
  "Copy the text between B and E to B in the clones.
Point's position is corrected with LEN."
  (let ((str (buffer-substring-no-properties b e))
	(clones jac-clones)
	(modified (buffer-modified-p)))
    (cl-loop for buf in jac-clones do
	     (if (buffer-live-p buf)
		 (with-current-buffer buf
		   (let ((inhibit-modification-hooks t)
			 (newPt (if (< (point) b) (point)
				  (+ (point) (- b e) (- len)))))
		     (goto-char b)
		     (insert str)
		     (goto-char newPt)
		     (unless (eq (buffer-modified-p) modified)
		       (set-buffer-modified-p modified))))
	       (setq clones (cl-remove buf clones))))
    (setq jac-clones clones)))

;;;###autoload
(defun jac (&optional newname display-flag)
  "Clone current buffer and give it NEWNAME.
DISPLAY-FLAG works like for `clone-buffer'."
  (interactive (progn
		 (if (get major-mode 'no-clone)
		     (error "Cannot clone a buffer in %s mode" mode-name))
		 (list (if current-prefix-arg
			   (read-buffer "Name of new cloned buffer: " (current-buffer)))
		       t)))
  (let* ((old (current-buffer))
	 (file (buffer-file-name))
	 buffer-file-name
	 (new (clone-buffer newname display-flag)))
  (when (buffer-live-p new)
      (with-current-buffer old
	(setq jac-clones (cons new jac-clones))
      (with-current-buffer new
	(setq jac-clones (cons old jac-clones))
	(setq buffer-file-name file))))))

(defun jac-set-buffer-modified-p (&rest flag)
  "Set the modified flag of the clones to value FLAG.
FLAG defaults to the value of the modified flag of the current buffer."
  (let ((modified (if (consp flag) (car flag) (buffer-modified-p))))
    (cl-loop for buf in jac-clones do
	     (with-current-buffer buf
	       (unless (eq (buffer-modified-p) modified)
		 (set-buffer-modified-p modified))))))

(add-hook 'before-change-functions  #'jac-before-change)
(add-hook 'after-change-functions  #'jac-after-change)

(add-hook 'after-save-hook #'jac-set-buffer-modified-p)

(provide 'jac)
;;; jac.el ends here
