;;;; -*- lexical-binding: t -*-

(defun macroutil--macroexpand-sexp-at-point (all inline)
  ;; Do the heavy lifting of the macroexpansion.
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
             ;; Inhibit read-only if we're in the special expansion buffer.
             (let ((inhibit-read-only
                    (if (string= (buffer-name (current-buffer))
                                 "*Macro expansion*")
                        t
                      inhibit-read-only)))
               (delete-region beg end)
              (goto-char beg)
              (pp macroexpanded-obj (current-buffer))
              ;; `pp' sometimes prints a line-feed at the end of its output.
              (when (bolp)
                (delete-char -1)))))
         (t
          (let* ((buf (get-buffer-create "*Macro expansion*"))
                 (already-there (eq buf (current-buffer))))
            (when (not already-there)
              (pop-to-buffer buf))
            (view-mode)
            (let ((inhibit-read-only t))
              (widen)
              (erase-buffer)
              (pp macroexpanded-obj (current-buffer))
              (goto-char (point-min))
              (emacs-lisp-mode)
              ;; Undo should work even though the buffer is otherwise read-only.
              (use-local-map (copy-keymap (or (current-local-map)
                                              (make-sparse-keymap))))
              (local-set-key [remap undo]
                             (lambda ()
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively 'undo)))))))))
    (error
     (message "Can't do macro expansion: %s (%s)"
              (error-message-string err) (car err)))))

(defun macroutil-macroexpand-sexp-at-point (&optional all)
  "Macroexpand the S-expression at point, displaying the result in a \
different buffer.
Prefix argument ALL means expand all subforms too, as with
`macroexpand-all'."
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all nil))

(defun macroutil-macroexpand-sexp-at-point-inline (&optional all)
  "Replace the S-expression at point with a macroexpanded version thereof.
Prefix argument ALL means expand all subforms too, as with `macroexpand-all'"
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all t))

(define-minor-mode macroutil-minor-mode nil nil nil
  (list (cons (kbd "C-c m") 'macroutil-macroexpand-sexp-at-point)
        (cons (kbd "C-c M-m") 'macroutil-macroexpand-sexp-at-point-inline)))

(provide 'macroutil)
