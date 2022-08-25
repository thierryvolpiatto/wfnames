;;; helm-edit-marked.el -- Edit marked files. -*- lexical-binding:t -*-
;;

;;; Code:

(require 'helm)

(defvar helm-ff-edit-buffer "*Edit hff marked*")

;; TODO Handle backing up and asking when overwriting
;; (defvar helm-ff-edit-marked-interactive-rename nil)
;; (defvar helm-ff-edit-marked-make-backup nil)
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

(defun helm-ff-edit-marked-after-change-hook (beg end _leng-before)
  (with-current-buffer helm-ff-edit-buffer
    (save-excursion
      (save-match-data
        (goto-char beg)
        (let* ((bol (point-at-bol))
               (eol (point-at-eol))
               (old (get-text-property bol 'old-name))
              (new  (buffer-substring-no-properties bol eol))
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
  (let ((marked (helm-marked-candidates :with-wildcard t)))
    (with-current-buffer (get-buffer-create helm-ff-edit-buffer)
      (save-excursion
        (cl-loop for file in marked
                 do (insert (propertize
                             file 'old-name file 'face 'helm-ff-file)
                            "\n")))
      (helm-ff-edit-mode)
    (switch-to-buffer helm-ff-edit-buffer)))

(defun helm-ff-edit-marked-commit-buffer ()
  (interactive)
  (let ((renamed 0) delayed)
    (cl-labels ((commit ()
                  (with-current-buffer helm-ff-edit-buffer
                    (goto-char (point-min))
                    (while (not (eobp))
                      (let ((old (get-text-property (point) 'old-name))
                            (new (buffer-substring-no-properties
                                  (point-at-bol) (point-at-eol))))
                        (unless (string= old new) ; not modified.
                          (if (and (file-exists-p new)
                                   (not (assoc new delayed)))
                              (let ((tmpfile (make-temp-name new)))
                                (push (cons new tmpfile) delayed)
                                (rename-file new tmpfile)
                                (delete-region (point-at-bol) (point-at-eol))
                                (insert (propertize new 'old-name tmpfile)))
                            (rename-file old new)
                            (add-text-properties
                             (point-at-bol) (point-at-eol) `(old-name ,new))
                            (setq delayed
                                  (delete (assoc new delayed) delayed))
                            (cl-incf renamed))))
                      (forward-line 1))
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
