;;; wfnames.el -- Edit marked files. -*- lexical-binding:t -*-
;;

;;; Description:
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

(defface wfnames-modified '((t :background "DarkOrange"))
  "Face used when filename is modified.")

(defface wfnames-modified-exists '((t :background "LightBlue"))
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

(defun wfnames-after-change-hook (beg _end _leng-before)
  (with-current-buffer wfnames-buffer
    (save-excursion
      (save-match-data
        (goto-char beg)
        (let* ((bol (point-at-bol))
               (eol (point-at-eol))
               (old (get-text-property bol 'old-name))
               (new (buffer-substring-no-properties bol eol))
               ov face)
          (setq face `(:background ,(if (file-exists-p new)
                                        "DarkOrange" "LightBlue")))
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
                   (overlay-put ov 'evaporate t))))))))

(cl-defun wfnames-setup-buffer (files
                                &optional (display-fn #'switch-to-buffer))
  (with-current-buffer (get-buffer-create wfnames-buffer)
    (save-excursion
      (cl-loop for file in files
               do (insert (propertize
                           file 'old-name file 'face 'helm-ff-file)
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
                                      (member new
                                              wfnames-old-files)
                                      (not (assoc new delayed)))
                                 ;; Maybe ask
                                 (if (or (null wfnames-interactive-rename)
                                         (y-or-n-p (format "File `%s' exists, overwrite? "
                                                           new)))
                                     (let ((tmpfile (make-temp-name old)))
                                       (push (cons new tmpfile) delayed)
                                       (rename-file old tmpfile)
                                       (add-text-properties
                                        beg end `(old-name ,tmpfile)))
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
                                         (y-or-n-p (format "File `%s' exists, overwrite? "
                                                           new)))
                                     (let ((tmpfile (make-temp-name new)))
                                       (push (cons new tmpfile) delayed)
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
