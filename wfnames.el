;;; wfnames.el --- Edit marked files. -*- lexical-binding:t -*-

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2022 Thierry Volpiatto, all rights reserved.
;; X-URL: https://github.com/thierryvolpiatto/wfnames

;; Compatibility: GNU Emacs 24.1+
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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
;; A mode to edit filenames, similar to wdired.

;; TODO:
;; - Handle backing up when overwriting
;; (defvar wfnames-make-backup nil)


;;; Code:

(require 'cl-lib)

;; Internal.
(defvar wfnames-buffer "*Wfnames*")
(defvar wfnames-old-files nil)


(defgroup wfnames nil
  "A mode to edit filenames."
  :group 'wfnames)

(defcustom wfnames-create-parent-directories nil
  "Create parent directories when non nil."
  :type 'boolean)

(defcustom wfnames-interactive-rename nil
  "Ask confirmation when overwriting."
  :type 'boolean)

(defcustom wfnames-backup-overwrite nil
  "Make a backup when overwriting."
  :type 'boolean)

(defface wfnames-modified '((t :background "LightBlue"))
  "Face used when filename is modified.")

(defface wfnames-modified-exists '((t :background "DarkOrange"))
  "Face used when modified fname point to an existing file.")

(defface wfnames-files '((t :foreground "RoyalBlue"))
  "Face used to display filenames in wfnames buffer.")

(defvar wfnames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") #'wfnames-commit-buffer)
    (define-key map (kbd "C-c C-k") #'wfnames-revert-changes)
    map))

(define-derived-mode wfnames-mode
    text-mode "wfnames-mode"
    "Edit HFF marked files.

Special commands:
\\{helm-ff-edit-mode-map}
"
  (add-hook 'after-change-functions #'wfnames-after-change-hook nil t))

(defun wfnames-after-change-hook (beg end _len)
  (with-current-buffer wfnames-buffer
    (save-excursion
      (save-match-data
        (goto-char beg)
        (let* ((bol (point-at-bol))
               (eol (point-at-eol))
               (old (get-text-property bol 'old-name))
               (new (buffer-substring-no-properties bol eol))
               ov face)
          (setq face (if (file-exists-p new)
                         'wfnames-modified-exists 'wfnames-modified))
          (cl-loop for o in (overlays-in bol eol)
                   when (overlay-get o 'hff-changed)
                   return (setq ov o))
          (cond ((string= old new)
                 (cl-loop for o in (overlays-in bol eol)
                          when (overlay-get o 'hff-changed)
                          do (delete-overlay o)))
                (ov
                 (move-overlay ov bol eol)
                 (overlay-put ov 'face face))
                (t (setq ov (make-overlay bol eol))
                   (overlay-put ov 'face face)
                   (overlay-put ov 'hff-changed t)
                   (overlay-put ov 'priority 0)
                   (overlay-put ov 'evaporate t)))
          ;; When text is modified with something else than
          ;; self-insert-command e.g. yank or iedit-rect, it loose its
          ;; properties, so restore props here.
          (put-text-property beg end 'face 'wfnames-files)
          (put-text-property beg end 'old-name old))))))

(cl-defun wfnames-setup-buffer (files
                                &optional (display-fn #'switch-to-buffer))
  (with-current-buffer (get-buffer-create wfnames-buffer)
    (save-excursion
      (cl-loop for file in files
               do (insert (propertize
                           file 'old-name file 'face 'wfnames-files)
                          "\n")))
    (wfnames-mode)
    (set (make-local-variable 'wfnames-old-files) files)
    (funcall display-fn wfnames-buffer)))

(defun wfnames-commit-buffer ()
  (interactive)
  (let ((renamed 0) (skipped 0) delayed)
    (cl-labels ((commit ()
                  (with-current-buffer wfnames-buffer
                    (goto-char (point-min))
                    (while (not (eobp))
                      (let* ((beg (point-at-bol))
                             (end (point-at-eol))
                             (old (get-text-property (point) 'old-name))
                             (new (buffer-substring-no-properties beg end)))
                        (unless (string= old new) ; not modified, skip.
                          (cond (;; New file exists and is one of the
                                 ;; next files to rename, make a temp
                                 ;; file of OLD and assign this temp
                                 ;; file to OLD, then delay renaming
                                 ;; to next turn.
                                 (and (file-exists-p new)
                                      (member new wfnames-old-files)
                                      (not (assoc new delayed)))
                                 ;; Maybe ask
                                 (if (or (null wfnames-interactive-rename)
                                         (y-or-n-p
                                          (format "File `%s' exists, overwrite? "
                                                  new)))
                                     (let ((tmpfile (make-temp-name old)))
                                       (push (cons new tmpfile) delayed)
                                       (when wfnames-backup-overwrite
                                         (rename-file
                                          new (car (find-backup-file-name new))))
                                       (rename-file new tmpfile))
                                   ;; Answer is no, skip.
                                   (add-text-properties
                                    beg end `(old-name ,new))
                                   (cl-incf skipped)))
                                (;; New file exists but is not part of
                                 ;; the next files to rename, make a
                                 ;; temp file of NEW and delay renaming
                                 ;; to next turn.
                                 (and (file-exists-p new)
                                      (not (assoc new delayed)))
                                 ;; Maybe ask.
                                 (if (or (null wfnames-interactive-rename)
                                         (y-or-n-p
                                          (format "File `%s' exists, overwrite? "
                                                  new)))
                                     (let ((tmpfile (make-temp-name new)))
                                       (push (cons new tmpfile) delayed)
                                       (when wfnames-backup-overwrite
                                         (rename-file
                                          new (car (find-backup-file-name new))))
                                       (rename-file new tmpfile))
                                   ;; Answer is no, skip.
                                   (add-text-properties
                                    beg end `(old-name ,new))
                                   (cl-incf skipped)))
                                (t ; Now really rename files.
                                 (when wfnames-create-parent-directories
                                   ;; Check if base directory of new exists.
                                   (let ((basedir (file-name-directory
                                                   (directory-file-name  new))))
                                     (unless (file-directory-p basedir)
                                       (mkdir basedir 'parents))))
                                 (rename-file
                                  old (if (file-directory-p new)
                                          (file-name-as-directory new)
                                        new))
                                 (add-text-properties beg end `(old-name ,new))
                                 (let* ((assoc (assoc new delayed))
                                        (tmp   (cdr assoc)))
                                   ;; The temp file was created in
                                   ;; clause 2, delete it.
                                   (when (and tmp (file-exists-p tmp))
                                     (if (file-directory-p tmp)
                                         (delete-directory tmp t)
                                       (delete-file tmp)))
                                   (setq delayed
                                         (delete assoc delayed)))
                                 (cl-incf renamed))))
                        (forward-line 1)))
                    (when delayed (commit)))))
      (commit)
      (message "* Renamed %s file(s), Skipped %s file(s)" renamed skipped)
      (kill-buffer wfnames-buffer))))

(defun wfnames-revert-changes ()
  (interactive)
  (with-current-buffer wfnames-buffer
    (cl-loop for o in (overlays-in (point-min) (point-max))
             when (overlay-get o 'hff-changed)
             do (delete-overlay o))
    (goto-char (point-min))
    (save-excursion
      (while (not (eobp))
        (let ((old (get-text-property (point) 'old-name))
              (new (buffer-substring-no-properties
                    (point-at-bol) (point-at-eol))))
          (unless (string= old new)
            (delete-region (point-at-bol) (point-at-eol))
            (insert (propertize
                     old 'old-name old 'face 'helm-ff-file)))
          (forward-line 1))))))

(provide 'wfnames)

;;; wfnames.el ends here
