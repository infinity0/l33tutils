;;;; hideshow-extra
;;;; enhancements to hideshow, a code folding mode
;
;;; Dependencies:
;
; This depends on the "hideshow" elisp packages, which comes bundled with
; modern versions of emacs.
;
;;; Keybindings:
;
; s-\           - Toggle current block.
; s-|           - Toggle current block and all blocks below it.
; C-s-\         - Toggle all top-level blocks.
; C-s-|         - Toggle all blocks at all levels.
;
;;; Usage and configuration:
;
;; First, setup up hideshow as per their instructions. Then:
;; Add this file to your load-path, then in init.el:
;
;(with-eval-after-load "hideshow" (load "hideshow-extra") (hs-set-keys "\\" "|"))
;
;; or change "\\" and "|" to keys of your choosing.

(eval-when-compile (require 'hideshow))

; NOTE: this file is a piece of shit because HideShow code is a piece of shit

; necessary for hs-toggle-hiding-rec
(setq hs-allow-nesting t)

(defun hs-set-keys (key &optional shiftkey)
  (unless shiftkey (setq shiftkey (concat "S-" key)))
  (global-set-key (kbd (concat "s-" key)) 'hs-toggle-hiding)
  (global-set-key (kbd (concat "s-" shiftkey)) 'hs-toggle-hiding-rec)
  (global-set-key (kbd (concat "C-s-" key)) 'hs-toggle-hiding-all)
  (global-set-key (kbd (concat "C-s-" shiftkey)) 'hs-toggle-hiding-all-rec))

;;; block-rec

; TODO: this could be way more efficient
(defun hs--hide-block-rec-helper (arg minp maxp)
  (unless (< arg 1)
    (hs-hide-level-recursive arg minp maxp)
    (hs--hide-block-rec-helper (1- arg) minp maxp)))

(defun hs-hide-block-rec (&optional end)
  "Select a block and hide it and all blocks below it.  With prefix arg,
reposition at END. Upon completion, point is repositioned and the normal
hook `hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs--hide-block-rec-helper 24 (point-min) (point-max))
  (hs-hide-block end))

(defun hs-show-block-rec (&optional end)
  "Select a block and show it and all blocks below it.
With prefix arg, reposition at END.  Upon completion, point is
repositioned and the normal hook `hs-show-hook' is run.
See documentation for functions `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (let ((old hs-allow-nesting))
    (setq hs-allow-nesting nil)
    (hs-show-block) ; for some reason we do have to do this twice :/
    (hs-show-block end)
    (setq hs-allow-nesting old)))

(defun hs-toggle-hiding-rec ()
  "Toggle hiding/showing of a block and all blocks below it.
See `hs-hide-block-rec' and `hs-show-block-rec'."
  (interactive)
  (hs-life-goes-on
   (if (hs-already-hidden-p)
       (hs-show-block-rec)
     (hs-hide-block-rec))))

;;; topall

(defun hs-show-all-nonrec ()
  "Show all top-level blocks then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (hs-discard-overlays (point-min) (point-max))
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

; this redefines hs-hide-block-at-point to make hs-toggle-hiding-all work properly
; the only change is to use (hs-discard-overlays p q) instead of (delete-overlay ov)
(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block if on block beginning.
Optional arg END means reposition at end.
Optional arg COMMENT-REG is a list of the form (BEGIN END) and
specifies the limits of the comment, or nil if the block is not
a comment.

The block beginning is adjusted by `hs-adjust-block-beginning'
and then further adjusted to be at the end of the line."
  (if comment-reg
      (hs-hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (when (hs-looking-at-block-start-p)
      (let ((mdata (match-data t))
            (header-end (match-end 0))
            p q ov)
	;; `p' is the point at the end of the block beginning, which
	;; may need to be adjusted
	(save-excursion
	  (if hs-adjust-block-beginning
	      (goto-char (funcall hs-adjust-block-beginning
				  header-end))
	    (goto-char header-end))
	  (setq p (line-end-position)))
	;; `q' is the point at the end of the block
	(hs-forward-sexp mdata 1)
	(setq q (if (looking-back hs-block-end-regexp)
		    (match-beginning 0)
		  (point)))
        (when (and (< p q) (> (count-lines p q) 1))
          (cond ((and hs-allow-nesting (setq ov (hs-overlay-at p)))
                 (hs-discard-overlays p q))
                ((not hs-allow-nesting)
                 (hs-discard-overlays p q)))
          (hs-make-overlay p q 'code (- header-end p)))
        (goto-char (if end q (min p header-end)))))))

(defun hs-toggle-hiding-all ()
  "Toggle hiding/showing of all top-level blocks.
See `hs-hide-all' and `hs-show-all-nonrec'."
  (interactive)
  (hs-life-goes-on
    ; lol hacks mothafucka
    (elp-instrument-function 'hs-make-overlay)
    (elp-instrument-function 'hs-discard-overlays)
    (hs-hide-all)
    ; if we created the same number of overlays as we deleted, then they were
    ; all already hidden and we want to show them all
    (let ((cc0 (aref (get 'hs-make-overlay elp-timer-info-property) 0))
          (cc1 (aref (get 'hs-discard-overlays elp-timer-info-property) 0)))
      (if (= cc0 cc1) (hs-show-all-nonrec)))
    (elp-restore-function 'hs-make-overlay)
    (elp-restore-function 'hs-discard-overlays)))

;;; topall-rec

(defun hs-hide-block-at-point-rec (&optional end comment-reg)
  (if comment-reg
      (hs-hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (hs-hide-block-rec end)))

; this is a patched version of "hs-hide-block-at-point" that redirects to
; "hs-hide-block-at-point-rec" but only on the top-most call, with subsequent
; recursive calls being directed to the original "hs-hide-block-at-point".
; yes this is a very very dirty hack
(fset 'hs--old-hide-block-at-point (symbol-function 'hs-hide-block-at-point))
(setq hs--guarded-hide-block-at-point nil)
(defun hs--guarded-hide-block-at-point (&optional end comment-reg)
  (if hs--guarded-hide-block-at-point (hs--old-hide-block-at-point end comment-reg)
    (setq hs--guarded-hide-block-at-point t)
    (hs-hide-block-at-point-rec end comment-reg)
    (setq hs--guarded-hide-block-at-point nil)))

(defun hs-hide-all-rec ()
  "Hide all top level blocks recursively, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the comments."
  (interactive)
  (fset 'hs-hide-block-at-point (symbol-function 'hs--guarded-hide-block-at-point))
  (hs-hide-all)
  (fset 'hs-hide-block-at-point (symbol-function 'hs--old-hide-block-at-point)))

(defun hs--all-overlays ()
  (-filter (lambda (ov) (overlay-get ov 'hs)) (overlays-in (point-min) (point-max))))

(defun hs-toggle-hiding-all-rec ()
  "Toggle hiding/showing of all blocks at all levels.
See `hs-hide-all-rec' and `hs-show-all'."
  (interactive)
  (hs-life-goes-on
   (if (hs--all-overlays)
       (hs-show-all)
     (hs-hide-all-rec))))
