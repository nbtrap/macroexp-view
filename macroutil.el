;;;; -*- lexical-binding: t -*-
(defun macroutil--macroexpand-sexp-at-point (all inline)
  (condition-case err
      (let* ((obj (save-excursion
                    (read (current-buffer))))
             (macroexpanded-obj
              (funcall (if all 'macroexpand-all 'macroexpand) obj))
             (beg (save-excursion
                    (forward-sexp)
                    (forward-sexp -1)
                    (point)))
             (end (save-excursion
                    (forward-sexp)
                    (point))))
        (when (> (point) beg)
          ;; Point is not at a sexp boundary.  Make sure we only treat what
          ;; follows point as the sexp to be expanded.
          (setq beg (point)))
        (cond
         (inline
           (save-excursion
             (let ((inhibit-read-only
                    (if (string= (buffer-name (current-buffer))
                                 "*Macro expansion*")
                        t
                      inhibit-read-only)))
               (delete-region beg end)
              (goto-char beg)
              (pp macroexpanded-obj (current-buffer))
              (when (bolp)
                (delete-char -1)))))
         (t
          (let* ((buf (get-buffer-create "*Macro expansion*"))
                 (already-there (eq buf (current-buffer))))
            (when (not already-there)
              (pop-to-buffer buf))
            (use-local-map (copy-keymap (or (current-local-map)
                                            (make-sparse-keymap))))
            (view-mode 0)
            (widen)
            (erase-buffer)
            (pp macroexpanded-obj (current-buffer))
            (goto-char (point-min))
            (emacs-lisp-mode)
            (view-mode)
            ;; Undo should work even though the buffer is otherwise read-only.
            (local-set-key [remap undo]
             (lambda ()
               (interactive)
               (let ((inhibit-read-only t))
                 (call-interactively 'undo))))))))
    (error
     (message "Can't do macro expansion: %s (%s)"
              (error-message-string err) (car err)))))

(defun macroutil-macroexpand-sexp-at-point (&optional all)
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all nil))

(defun macroutil-macroexpand-sexp-at-point-inline (&optional all)
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all t))

(define-minor-mode macroutil-minor-mode "" nil nil
  (list (cons (kbd "C-c m") 'macroutil-macroexpand-sexp-at-point)
        (cons (kbd "C-c M-m") 'macroutil-macroexpand-sexp-at-point-inline)))

(provide 'macroutil)
