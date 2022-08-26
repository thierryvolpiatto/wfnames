;;; helm-edit-marked.el -- Edit marked files. -*- lexical-binding:t -*-
;;

;;; Code:

(require 'helm)

(defvar helm-ff-edit-buffer "*Edit hff marked*")
(defvar helm-ff--edit-marked-old-files nil)
(defvar helm-ff-edit-marked-create-parent-directories nil)

;; TODO:
;; - Handle backing up and asking when overwriting
;; (defvar helm-ff-edit-marked-interactive-rename nil)
;; (defvar helm-ff-edit-marked-make-backup nil)
;; - Create parent directories
;; (defvar helm-ff-edit-marked-create-parent-directories nil)

(defvar helm-ff-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") #'helm-ff-edit-marked-commit-buffer)
    (define-key map (kbd "C-c C-k") #'helm-ff-edit-marked-revert-changes)
    map))

(define-derived-mode helm-ff-edit-mode text-mode
  "helm-ff-edit-mode"
  "Edit HFF marked files.

Special commands:
\\{helm-ff-edit-mode-map}
"
  (add-hook 'after-change-functions #'helm-ff-edit-marked-after-change-hook nil t))

(defun helm-ff-edit-marked-after-change-hook (beg _end _leng-before)
  (with-current-buffer helm-ff-edit-buffer
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

(defun helm-ff-edit-marked-files (_candidate)
  (let ((marked (helm-marked-candidates)))
    (with-current-buffer (get-buffer-create helm-ff-edit-buffer)
      (save-excursion
        (cl-loop for file in marked
                 do (insert (propertize
                             file 'old-name file 'face 'helm-ff-file)
                            "\n")))
      (helm-ff-edit-mode)
      (set (make-local-variable 'helm-ff--edit-marked-old-files) marked))
    (switch-to-buffer helm-ff-edit-buffer)))

(defun helm-ff-edit-marked-commit-buffer ()
  (interactive)
  (let ((renamed 0) delayed)
    (cl-labels ((commit ()
                  (with-current-buffer helm-ff-edit-buffer
                    (goto-char (point-min))
                    (while (not (eobp))
                      (let* ((beg (point-at-bol))
                             (end (point-at-eol))
                             (old (get-text-property (point) 'old-name))
                             (new (buffer-substring-no-properties beg end)))
                        (unless (string= old new) ; not modified.
                          (cond (;; New file exists and is one of the
                                 ;; next files to rename, make a temp
                                 ;; file of OLD and assign this temp
                                 ;; file to OLD, then delay renaming
                                 ;; to next turn.
                                 (and (file-exists-p new)
                                      (member new
                                              helm-ff--edit-marked-old-files)
                                      (not (assoc new delayed)))
                                 (let ((tmpfile (make-temp-name old)))
                                   (push (cons new tmpfile) delayed)
                                   (rename-file old tmpfile)
                                   (add-text-properties
                                    beg end `(old-name ,tmpfile))))
                                ;; New file exists but is not part of
                                ;; the next files to rename, make a
                                ;; temp file of NEW and delay renaming
                                ;; to next turn.
                                ((and (file-exists-p new)
                                      (not (assoc new delayed)))
                                 (let ((tmpfile (make-temp-name new)))
                                   (push (cons new tmpfile) delayed)
                                   (rename-file new tmpfile)))
                                ;; Now really rename files.
                                (t
                                 (when helm-ff-edit-marked-create-parent-directories
                                   ;; Check if base directory of new exists.
                                   (let ((basedir (helm-basedir new 'parent)))
                                     (unless (file-directory-p basedir)
                                       (mkdir basedir 'parents))))
                                 (rename-file old new)
                                 (add-text-properties beg end `(old-name ,new))
                                 (let* ((assoc (assoc new delayed))
                                        (tmp   (cdr assoc)))
                                   ;; The temp file was created in
                                   ;; clause 2, delete it.
                                   (when (and tmp (file-exists-p tmp))
                                     (delete-file tmp))
                                   (setq delayed
                                         (delete assoc delayed)))
                                 (cl-incf renamed))))
                        (forward-line 1)))
                    (when delayed (commit)))))
      (commit)
      (message "* Renamed %s file(s) " renamed)
      (kill-buffer helm-ff-edit-buffer))))

(defun helm-ff-edit-marked-revert-changes ()
  (interactive)
  (with-current-buffer helm-ff-edit-buffer
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

(provide 'helm-edit-marked)

;;; helm-edit-marked.el ends here
