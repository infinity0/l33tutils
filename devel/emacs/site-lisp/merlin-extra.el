;;;; merlin-extras
;;;; enhancements to merlin, an OCaml IDE plugin
;
;;; Dependencies:
;
; You need to install the "company" elisp package. On Debian you can do this
; by running `aptitude install elpa-company`.
;
;;; Behaviours:
;
; - Automatically show the type of the expression at point.
;   Small types go in minibuffer, large types go in a new side window "below".
;   The side window auto-closes when the point moves to a non-expression.
;
;;; Usage and configuration:
;
;; First, setup up merlin as per their instructions. Then:
;; Add this file to your load-path, then in init.el:
;
;(autoload 'merlin-auto-type-enclosing "merlin-extra" nil t)
;(add-hook 'tuareg-mode-hook 'merlin-auto-type-enclosing)
;

;;; enable e.g. with (add-hook 'tuareg-mode-hook 'merlin-auto-type-enclosing)

(defun merlin-auto-type-enclosing ()
  "Automatically run type-enclosing after every command"
  (add-hook 'post-command-hook 'merlin-type-enclosing nil t))

; see also http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
(add-to-list 'display-buffer-alist
  '((lambda (buffer _) (equal buffer merlin-type-buffer-name))
    (display-buffer-reuse-window
     display-buffer-below-selected)
    (window-height    . 0.25  )))

; from http://www.emacswiki.org/emacs/misc-cmds.el
(defun merlin--kill-type ()
  (setq buffer  (get-buffer merlin-type-buffer-name))
  (if (buffer-live-p buffer)            ; Kill live buffer only.
    (let ((wins  (get-buffer-window-list buffer nil t))) ; On all frames.
      (when (kill-buffer buffer)      ; Only delete windows if buffer killed.
        (dolist (win  wins)           ; (User might keep buffer if modified.)
          (when (window-live-p win)
            ;; Ignore error, in particular,
            ;; "Attempt to delete the sole visible or iconified frame".
          (condition-case nil (delete-window win) (error nil))))))))

;; monkey-patch merlin; ideally we would push these upstream

; already present in merlin HEAD
(defun merlin--count-lines (text)
  (let ((count 0)
        (pos   0))
    (while (and (<= count 8)
                (string-match "\n" text pos))
           (setq pos (match-end 0))
           (setq count (1+ count)))
    count))

(with-eval-after-load "merlin"
  (fset 'merlin--old-type-display (symbol-function 'merlin--type-display))
  (defun merlin--type-display (bounds type &optional quiet)
    "Display the type TYPE of the expression occuring at BOUNDS.
If QUIET is non nil, then an overlay and the merlin types can be used."
    (merlin--old-type-display bounds type quiet)
    (if (not type)
        (merlin--kill-type)
      (if (<= (merlin--count-lines type) 8)
          (merlin--kill-type)))))

(with-eval-after-load "merlin"
  (fset 'merlin--old-type-enclosing-query (symbol-function 'merlin--type-enclosing-query))
  (defun merlin--type-enclosing-query ()
    "Get the enclosings around point from merlin and sets MERLIN-ENCLOSING-TYPES."
    (if (merlin--old-type-enclosing-query)
        t
      (progn (merlin--kill-type) nil))))
