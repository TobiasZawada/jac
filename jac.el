;;; jac.el --- Just another buffer cloning method.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tobias Zawada

;; Author: Tobias Zawada <i@tn-home.de>
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

;; Both buffers have the same text contents.  If you edit the text in the
;; original buffer the modifications are transferred to the clone and
;; visa-versa.  Text properties are not transferred.  That means you
;; can switch the major mode for the clone.

;;; Code:

(defvar-local jac-clones nil
  "List of clones that need to be adapted to the original buffer.")
(put 'jac-clones 'permanent-local t)

(defmacro jac-with-current-wide-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER temporarily current.
This expands to:
\\(with-current-buffer BUFFER (save-restriction (widen) ...BODY...))."
  (declare (debug (sexp body)) (indent 1))
  `(with-current-buffer ,buffer
     (save-excursion
       (save-restriction
	 (widen)
	 ,@body))))

(defmacro jac-do-clones (&rest body)
  "Evaluate BODY in the clones of current buffer.
The clone is widened before running BODY.
Dead buffers are deleted from `jac-clones'."
  (declare (debug body))
  (let ((clones (make-symbol "clones"))
	(buf (make-symbol "buf"))
	(master-name (make-symbol "master")))
    `(let ((,master-name (buffer-file-name))
	   (,clones jac-clones))
       (cl-loop for ,buf in jac-clones do
		(if (and (buffer-live-p ,buf)
			 (string-equal (buffer-file-name ,buf)
				       ,master-name))
		    (jac-with-current-wide-buffer ,buf
		      ,@body)
		  (setq ,clones (cl-remove ,buf ,clones))))
       (setq jac-clones ,clones))))

(defun jac-before-change (b e)
  "Kill region betweenn B and E in the clones."
  (jac-do-clones
    (let ((inhibit-modification-hooks t))
      (delete-region b (min e (point-max))))))

(defun jac-after-change (b e len)
  "Copy the text between B and E to B in the clones.
Point's position is corrected with LEN."
  (let ((str (buffer-substring-no-properties b e))
	(modified (buffer-modified-p)))
    (jac-do-clones
	(let ((inhibit-modification-hooks t)
	      (newPt (if (< (point) b) (point)
		       (+ (point) (- b e) (- len)))))
	  (goto-char b)
	  (insert str)
	  (goto-char newPt)
	  (unless (eq (buffer-modified-p) modified)
	    (set-buffer-modified-p modified))))))

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
	(setq buffer-file-name file)))
      new)))

(defvar jac-inhibit-set-clones-modified-p nil
  "Master for current buffer modifying operation.")

(defun jac-set-clones-modified-p (&rest flag)
  "Set the modified flag of the clones to value FLAG.
FLAG defaults to the value of the modified flag of the current buffer."
  (unless jac-inhibit-set-clones-modified-p
    (let ((modified (if (consp flag) (car flag) (buffer-modified-p)))
	  (jac-inhibit-set-clones-modified-p t))
      (jac-do-clones
	  (unless (eq (buffer-modified-p) modified)
	    (set-buffer-modified-p modified) ;; This re-invokes `jac-set-clones-modified-p'.
	    )))))

(defun jac-revert-clones ()
  "Revert clones with the contents of the master."
  (let ((contents (save-restriction
		    (widen)
		    (buffer-substring-no-properties (point-min) (point-max)))))
    (jac-do-clones
	(delete-region (point-min) (point-max))
      (insert contents))
    (jac-set-clones-modified-p)))

(add-hook 'before-change-functions  #'jac-before-change)
(add-hook 'after-change-functions  #'jac-after-change)
(add-hook 'after-save-hook #'jac-set-clones-modified-p)
(add-hook 'after-revert-hook #'jac-revert-clones)
(advice-add 'set-buffer-modified-p :after #'jac-set-clones-modified-p)
;;< Does not work within c commands but works for undo.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test support

(defun jac-equal-buffer-text-p (buffer1 &optional buffer2)
  "Return non-nil if BUFFER1 and BUFFER2 have the same text.
Text properties are ignored.
BUFFER2 defaults to `current-buffer'."
  (unless buffer2
    (setq buffer2 (current-buffer)))
  (string-equal
   (jac-with-current-wide-buffer buffer1
     (buffer-substring-no-properties (point-min) (point-max)))
   (jac-with-current-wide-buffer buffer2
     (buffer-substring-no-properties (point-min) (point-max)))))



(provide 'jac)
;;; jac.el ends here
