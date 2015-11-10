;;;; fspeedbar
;;;; fixed speedbar in the same frame, similar to sr-speedbar but simpler
;
;;; Keybindings:
;
; Super-s       - toggle speedbar display
; u             - toggle between previous and current speedbar display mode
; C-scroll-up,  - cycle through display modes listed in `fspeedbar-scroll-mode-alist`,
; C-scroll-down   which defaults to the ones in `speedbar-initial-expansion-mode-alist`
;
;;; Usage and configuration:
;
;; Add this file to your load-path, then in init.el:
;
;(autoload 'speedbar-mode "speedbar" nil t)
;(autoload 'fspeedbar-toggle "fspeedbar" nil t)
;(global-set-key (kbd "s-s") 'fspeedbar-toggle)
;
;; Optional: don't scroll through "Quick Buffers"
;(setq fspeedbar-scroll-mode-alist '("files" "buffers"))
;

;;; basic functionality

(defconst fspeedbar-buffer-name "*SPEEDBAR*"
  "The buffer name of fspeedbar.")

(defun fspeedbar-open ()
  "Open the fixed speedbar window."
  (interactive)
  (setq speedbar-buffer (get-buffer-create fspeedbar-buffer-name)
    speedbar-frame (selected-frame)
    dframe-attached-frame (selected-frame)
    speedbar-select-frame-method 'attached
    speedbar-last-selected-file nil)
  (set-buffer speedbar-buffer)
  (buffer-disable-undo speedbar-buffer)
  (speedbar-mode)
  (speedbar-reconfigure-keymaps)
  (speedbar-update-contents)
  (speedbar-set-timer 1)
  (display-buffer fspeedbar-buffer-name)
  (set-window-dedicated-p (get-buffer-window fspeedbar-buffer-name) t))

(defun fspeedbar-close ()
  "Close the fixed speedbar window."
  (interactive)
  (delete-window (get-buffer-window fspeedbar-buffer-name))
  (kill-buffer fspeedbar-buffer-name))

; see also http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
(add-to-list 'display-buffer-alist
  '((lambda (buffer _) (equal buffer fspeedbar-buffer-name))
    (display-buffer-reuse-window
     display-buffer-in-side-window)
    (side            . left   )
    (window-width    . 0.2    )))

(defun fspeedbar-toggle ()
  "Toggle the fixed speedbar window."
  (interactive)
  (if (get-buffer-window fspeedbar-buffer-name)
      (fspeedbar-close)
    (fspeedbar-open)))

;;; load speedbar targets in the previously-selected-window,
;;; instead of randomly jumping around different windows.

; https://stackoverflow.com/questions/7937395/select-the-previously-selected-window-in-emacs
(defun mru-windows ()
  (let* ((buflist (buffer-list (selected-frame)))      ; get buffer list in this frames ordered
      (buflist (delq (current-buffer) buflist))     ; if there are multiple windows showing same buffer.
      (winlist (mapcar 'get-buffer-window buflist)) ; buf->win
      (winlist (delq nil winlist))                  ; remove non displayed windows
      (winlist (delq (selected-window) winlist)))
    (delete-dups winlist)))

(add-to-list 'display-buffer-base-action
  '((lambda (buffer alist)
    (unless
      (or (cdr (assq 'inhibit-same-window alist))
        (not (equal (get-buffer-window fspeedbar-buffer-name) (selected-window))))
      (window--display-buffer buffer (car (mru-windows)) 'reuse alist)))))

;;; toggle between old and new modes

; monkey-patch speedbar; ideally we would push these upstream
(with-eval-after-load "speedbar"
  (defun speedbar-change-initial-expansion-list (new-default)
    "Change speedbar's default expansion list to NEW-DEFAULT."
    (interactive
     (list
      (completing-read (format "Speedbar Mode (default %s): "
              speedbar-previously-used-expansion-list-name)
           speedbar-initial-expansion-mode-alist
           nil t "" nil
           speedbar-previously-used-expansion-list-name)))
    (unless (equal speedbar-initial-expansion-list-name new-default)
      (setq
        speedbar-previously-used-expansion-list-name speedbar-initial-expansion-list-name
        speedbar-initial-expansion-list-name new-default))
    (speedbar-refresh)
    (speedbar-reconfigure-keymaps)))

; monkey-patch speedbar; ideally we would push these upstream
(with-eval-after-load "speedbar"
  (setq speedbar-previously-used-expansion-list-name "buffers"))

(with-eval-after-load "speedbar"
  (define-key speedbar-key-map "u"
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list
       speedbar-previously-used-expansion-list-name))))

;;;; scroll between modes

(defcustom fspeedbar-scroll-mode-alist nil
  "List of modes to scroll through")

(with-eval-after-load "speedbar"
  (unless fspeedbar-scroll-mode-alist
    (setq fspeedbar-scroll-mode-alist
      (mapcar 'car speedbar-initial-expansion-mode-alist))))

(defun list-cycle-next (elt list)
  (let ((tail (member elt list)))
    (if tail
      (let ((next (cdr tail)))
        (if next (car next) (car list))))))

(defun list-cycle-prev (elt list)
  (let ((pos (position-if (lambda (x) (equal x elt)) list)))
    (if pos
      (if (equal pos 0) (car (last list)) (nth (- pos 1) list)))))

(with-eval-after-load "speedbar"
  (define-key speedbar-key-map (kbd "<C-mouse-5>")
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list
       (list-cycle-next speedbar-initial-expansion-list-name fspeedbar-scroll-mode-alist)))))

(with-eval-after-load "speedbar"
  (define-key speedbar-key-map (kbd "<C-mouse-4>")
    (lambda () (interactive)
      (speedbar-change-initial-expansion-list
       (list-cycle-prev speedbar-initial-expansion-list-name fspeedbar-scroll-mode-alist)))))
