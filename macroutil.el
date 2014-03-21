;;;; -*- lexical-binding: t -*-

(defconst macroutil-macroexp-buffer-name "*Macro expansion*")

(defun macroutil--macroexpand-sexp-at-point (all inline)
  ;; Do the heavy lifting of the macroexpansion.
  (condition-case err
      (let* ((obj (save-excursion
                    (read (current-buffer))))
             (macroexpanded-obj
              (funcall (if all #'macroexpand-all #'macroexpand) obj))
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
           (unless (eq obj macroexpanded-obj)
            (save-excursion
              ;; Inhibit read-only if we're in the special expansion buffer.
              (let ((inhibit-read-only
                     (if (string= (buffer-name (current-buffer))
                                  macroutil-macroexp-buffer-name)
                         t
                       inhibit-read-only)))
                (delete-region beg end)
                (goto-char beg)
                (pp macroexpanded-obj (current-buffer))
                ;; `pp' sometimes prints a line-feed at the end of its output.
                (when (bolp)
                  (delete-char -1))))))
         (t
          (let* ((buf (get-buffer-create macroutil-macroexp-buffer-name)))
            (unless (eq buf (current-buffer))
              (pop-to-buffer buf))
            (emacs-lisp-mode)
            (view-mode)
            (let ((inhibit-read-only t))
              (widen)
              (erase-buffer)
              (pp macroexpanded-obj (current-buffer))
              (goto-char (point-min))
              ;; Undo should work even though the buffer is otherwise read-only.
              (use-local-map (copy-keymap (or (current-local-map)
                                              (make-sparse-keymap))))
              (local-set-key [remap undo]
                             (lambda ()
                               "Same as `undo', but inhibit read-only."
                               (interactive)
                               (let ((inhibit-read-only t))
                                 (call-interactively #'undo))))))))
        ;; Indent the new sexp.  We can inhibit read-only indiscriminately at
        ;; this point, since we would have already failed if we weren't supposed
        ;; to be writing in the buffer.
        (unless (eq obj macroexpanded-obj)
          (let ((inhibit-read-only t))
            (indent-region (point) (save-excursion (forward-sexp) (point))))))
    (error
     (message "Can't do macro expansion: %s (%s)"
              (error-message-string err) (car err)))))

(defun macroutil-macroexpand-sexp-at-point (&optional all)
  "Macroexpand the S-expression at point, displaying the result in a \
different buffer.
Prefix argument ALL means expand all subforms too, as with `macroexpand-all'."
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all nil))

(defun macroutil-macroexpand-sexp-at-point-inline (&optional all)
  "Replace the S-expression at point with a macroexpanded version thereof.
Prefix argument ALL means expand all subforms too, as with `macroexpand-all'."
  (interactive "P")
  (macroutil--macroexpand-sexp-at-point all t))

(define-minor-mode macroutil-minor-mode nil nil nil
  (list (cons (kbd "C-c m") 'macroutil-macroexpand-sexp-at-point)
        (cons (kbd "C-c M-m") 'macroutil-macroexpand-sexp-at-point-inline)))

(provide 'macroutil)
