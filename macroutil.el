(defun macroutil-macroexpand-sexp-at-point (&optional all)
  "Show the macro expansion of the form after point in another \
window.
When ALL is non-nil, call `macroexpand-all' instead of just
`macroexpand'."
  (interactive "P")
  (condition-case err
      (save-excursion
        (let* ((obj (read (current-buffer)))
               (buf (get-buffer-create "*Macro expansion*"))
               (already-there (eq (current-buffer) buf)))
          ;; When we're already in the macro expansion buffer, we expand the
          ;; macro in place.
          (when (not already-there)
            (pop-to-buffer buf))
          (view-mode 0)
          (widen)
          (if (not already-there)
              (erase-buffer)
            (delete-region (point) (progn (backward-sexp) (point))))
          (pp (if all (macroexpand-all obj) (macroexpand obj))
              (current-buffer))
          (when already-there
            (backward-char)
            (when (looking-at-p "$")
              (delete-char 1))
            (backward-sexp)
            (indent-region (point) (progn (forward-sexp) (point))))
          (emacs-lisp-mode)
          (view-mode)
          (when (not already-there)
            (goto-char (point-min)))))
    (error
     (message "Can't do macro expansion: %S" err))))

(provide 'macroutil)
