;;; Edit marked files
;;
(defvar helm-ff-edit-buffer "*edit hff marked*")
(defvar helm-ff--edit-marked-old-files nil)

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
    (let ((old (get-text-property (point-at-bol) 'old-name))
          (new (buffer-substring-no-properties
                (point-at-bol) (point-at-eol)))
          ov)
      (cond ((string= old new)
             (cl-loop for o in (overlays-in (point-at-bol) (point-at-eol))
                      when (overlay-get o 'hff-changed)
                      do (delete-overlay o)))
            (t
             (setq ov (make-overlay (point-at-bol) (point-at-eol)))
             (overlay-put ov 'face '(:background "LightBlue"))
             (overlay-put ov 'hff-changed t)
             (setq ov (make-overlay beg end))
             (overlay-put ov 'face 'font-lock-variable-name-face)
             (overlay-put ov 'hff-changed t))))))

(defun helm-ff-edit-marked-files (_candidate)
  (let ((marked (helm-marked-candidates :with-wildcard t)))
    (with-current-buffer (get-buffer-create helm-ff-edit-buffer)
      (save-excursion
        (cl-loop for file in marked
                 do (insert (propertize
                             file 'old-name file 'face 'helm-ff-file)
                            "\n")))
      (helm-ff-edit-mode)
      (set (make-local-variable 'helm-ff--edit-marked-old-files) marked))
    (switch-to-buffer helm-ff-edit-buffer)))

;; /home/thierry/tmp/test001.txt
;; /home/thierry/tmp/test002.txt
;; /home/thierry/tmp/test003.txt
;; /home/thierry/tmp/test004.txt
;; /home/thierry/tmp/test005.txt


;; /home/thierry/tmp/test001.txt => /home/thierry/tmp/test005.txt
;; /home/thierry/tmp/test002.txt => /home/thierry/tmp/test004.txt
;; /home/thierry/tmp/test004.txt => /home/thierry/tmp/test002.txt

(setq helm-find-files-actions
      (helm-append-at-nth
       helm-find-files-actions
       '(("Edit filenames" . helm-ff-edit-marked-files)) 2))

;; (setq helm-source-find-files nil)

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
