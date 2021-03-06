;;; macroexp-view.el --- Convenience commands for quickly and easily viewing macro expansions -*- lexical-binding: t -*-

;; Copyright (C) 2014 Nathan Trapuzzano

;; Author: Nathan Trapuzzano <nbtrap@nbtrap.com>
;; Version: 1.0
;; Keywords: convenience lisp maint tools

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `mv-macroexpand-sexp-at-point' and `mv-macroexpand-sexp-at-point-inline'
;; perform macro expansion of the S-expression at point; the former displays the
;; expansion in a popup buffer, while the latter displays it inline, replacing
;; the unexpanded form.  With a prefix argument, all subforms get expanded as
;; well.

;; Additionally, a minor mode, `macroexp-view-minor-mode', is provided for
;; binding the above commands to `C-c m' and `C-c M-m', respectively.  Add the
;; minor mode command to `emacs-lisp-mode-hook' for the easiest setup, as in:
;;
;;   (require 'macroexp-view)
;;   (add-hook 'emacs-lisp-mode-hook 'macroexp-view-minor-mode)

;;; Code:

;;;###autoload
(defconst mv-macroexp-buffer-name "*macroexp-view*")

(defun mv--macroexpand-sexp-at-point (all inline)
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
                                  mv-macroexp-buffer-name)
                         t
                       inhibit-read-only)))
                (delete-region beg end)
                (goto-char beg)
                (pp macroexpanded-obj (current-buffer))
                ;; `pp' sometimes prints a line-feed at the end of its output.
                (when (bolp)
                  (delete-char -1))))))
         (t
          (let* ((buf (get-buffer-create mv-macroexp-buffer-name)))
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
            (indent-region (point) (save-excursion (forward-sexp) (point)))))
        t)
    (error
     (message "Can't do macro expansion: %s (%s)"
              (error-message-string err) (car err))
     nil)))

;;;###autoload
(defun mv-macroexpand-sexp-at-point (&optional all)
  "Macroexpand the S-expression at point, displaying the result in a different buffer.
Prefix argument ALL means expand all subforms too, as with
`macroexpand-all'.  Return t if expansion succeeds, nil
otherwise."
  (interactive "P")
  (mv--macroexpand-sexp-at-point all nil))

;;;###autoload
(defun mv-macroexpand-sexp-at-point-inline (&optional all)
  "Replace the S-expression at point with a macroexpanded version thereof.
Prefix argument ALL means expand all subforms too, as with
`macroexpand-all'.  Return t if expansion succeeds, nil
otherwise."
  (interactive "P")
  (mv--macroexpand-sexp-at-point all t))

;;;###autoload
(define-minor-mode macroexp-view-minor-mode nil nil nil
  (list (cons (kbd "C-c m") 'mv-macroexpand-sexp-at-point)
        (cons (kbd "C-c M-m") 'mv-macroexpand-sexp-at-point-inline)))

(provide 'macroexp-view)

;;; macroexp-view.el ends here
