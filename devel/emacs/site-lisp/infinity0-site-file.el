;;;; Greedy init.el snippet that includes everything else in this directory.
;
; TL;DR installation:
;
; # apt-get install opam elpa-company tuareg-mode ocp-indent
; # opam install merlin # or tuareg-mode ocp-indent here instead of via apt-get
; $ git submodule update --init # or git clone --recursive $URL/OF/THIS/REPO && cd $PATH/TO/THIS/FILE
; $ test -f infinity0-site-file.el && echo >> ~/.emacs.d/init.el "(add-to-list 'load-path \"$PWD\")"
; $ test -f infinity0-site-file.el && echo >> ~/.emacs.d/init.el "(load \"infinity0-site-file\")"
;
; grep this file for "(kbd" to see the extra enabled keymaps; RTFS for docs. :)

;;;; sys init

; set paths from opam
(setq opam-share (substring (shell-command-to-string "opam config var share 2>/dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;;;; package init, upstream code snippets only

;;; fspeedbar
(autoload 'speedbar-mode "speedbar" nil t)
(autoload 'fspeedbar-toggle "fspeedbar" nil t)
(autoload 'fspeedbar-sess-load "fspeedbar" nil t)
(add-to-list 'desktop-buffer-mode-handlers '(speedbar-mode . fspeedbar-sess-load))
(global-set-key (kbd "s-s") 'fspeedbar-toggle)

;;; nameses
(require 'desktop)
(require 'ido)
(require 'nameses)
(global-set-key (kbd "<f9>")     'nameses-load)
(global-set-key (kbd "C-<f9>")   'nameses-prev)
(global-set-key (kbd "C-S-<f9>") 'nameses-save)

;;; undo-tree
(require 'undo-tree)

;;; ocaml, tuareg
(load "tuareg-site-file")
;; tuareg with ocp-indent
(autoload 'ocp-setup-indent "ocp-indent" nil t)
(add-hook 'tuareg-mode-hook 'ocp-setup-indent t)

;;; ocaml, merlin
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(setq merlin-command 'opam)
;; merlin-extra
(with-eval-after-load "merlin" (load "merlin-extra"))
(global-set-key (kbd "C-c M-t") 'merlin-toggle-auto-type)
;; merlin with company-mode completion
(autoload 'company-mode "company" nil t)
(add-hook 'merlin-mode-hook 'company-mode)
(with-eval-after-load "company" (add-to-list 'company-backends 'merlin-company-backend))
;; merlin with iedit-mode refactoring
(autoload 'iedit-mode "iedit" nil t)
(autoload 'merlin-iedit-occurrences "merlin-iedit" nil t)
(with-eval-after-load "merlin"
  (define-key merlin-mode-map (kbd "C-c C-e")
    ; edits all occurrences of a given identifier. sometimes the behaviour is
    ; slightly buggy and doesn't catch everything, file a bug to merlin if so.
    (lambda () (interactive)
      (if iedit-mode (iedit-mode) (merlin-iedit-occurrences)))))

;;;; session management

(server-start)

;; Save all tempfiles in $TMPDIR/emacs$UID/ instead of next to every fucking opened file
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(setq create-lockfiles nil)
