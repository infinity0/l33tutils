;;;; Greedy init.el snippet that includes everything else in this directory.
;
; TL;DR installation:
;
; # apt-get install elpa-company  opam tuareg-mode ocp-indent  cargo rust-src elpa-rust-mode dash-el s-el  haskell-mode ghc-mod
; $ opam install merlin; cargo install racer
; $ git submodule update --init # or git clone --recursive $URL/OF/THIS/REPO && cd $PATH/TO/THIS/FILE
; $ test -f infinity0-site-file.el && cat >> ~/.profile infinity0-login-profile
; $ test -f infinity0-site-file.el && cat >> ~/.emacs.d/init.el <<EOF
; > (add-to-list 'load-path "$PWD")
; > (load "infinity0-site-file")
; > EOF
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
(setq speedbar-show-unknown-files t)

;;; nameses
(require 'desktop)
(require 'ido)
(require 'nameses)
(global-set-key (kbd "<f9>")     'nameses-load)
(global-set-key (kbd "C-<f9>")   'nameses-prev)
(global-set-key (kbd "C-S-<f9>") 'nameses-save)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; company-mode completion
; uncomment the following line if you see errors about "void-function block" or
; "Symbol's function definition is void: block"; see rust-lang/rust-mode#130
;(require 'cl)
(unless (fboundp 'company-mode)
  (with-demoted-errors "infinity0-site-file warning: %S" (load "company-autoloads")))
(with-eval-after-load "company"
  ; not yet in Debian package, just copy it from upstream git
  (unless (fboundp 'company-indent-or-complete-common)
    (defun company-indent-or-complete-common ()
      "Indent the current line or region, or complete the common part."
      (interactive)
      (cond
       ((use-region-p)
        (indent-region (region-beginning) (region-end)))
       ((let ((old-point (point))
              (old-tick (buffer-chars-modified-tick))
              (tab-always-indent t))
          (call-interactively #'indent-for-tab-command)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (company-complete-common))))))))
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
; uncomment for "in-your-face" behaviour
;(setq company-minimum-prefix-length 0)
;(setq company-idle-delay 0)
; arguably looks a bit nicer
;(setq company-tooltip-align-annotations t)

;;; code folding
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
(with-eval-after-load "hideshowvis" (hideshowvis-symbols))
(with-eval-after-load "hideshow" (load "hideshow-extra") (hs-set-keys "\\" "|"))
(dolist (hook (list
    'emacs-lisp-mode-hook
    'rust-mode-hook
    'tuareg-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;;; haskell, haskell-mode
; Debian packages "haskell-mode" and "ghc-mod" already do the below
; but you can try uncommenting it if you're using something else.
;(autoload 'ghc-init "ghc" nil t)
;(autoload 'ghc-debug "ghc" nil t)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(autoload 'company-ghc "company-ghc" nil t)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'company-mode)
(with-eval-after-load "ghc"
  (with-eval-after-load "company"
    (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))))
; "dabbrev-code" is a hacky semi-fix for https://github.com/iquiw/company-ghc/issues/31

;;; rust, rust-mode
(unless (fboundp 'rust-mode)
  (with-demoted-errors "infinity0-site-file warning: %S" (load "rust-mode-autoloads")))
;; rust-mode with racer/company completion
(autoload 'racer-mode "racer" nil t)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)

;;; ocaml, tuareg
(unless (fboundp 'tuareg-mode)
  (with-demoted-errors "infinity0-site-file warning: %S" (load "tuareg-site-file")))
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
(add-hook 'merlin-mode-hook 'company-mode)
(with-eval-after-load "merlin"
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'merlin-company-backend)))
;; merlin with iedit-mode refactoring
(autoload 'iedit-mode "iedit" nil t)
(autoload 'merlin-iedit-occurrences "merlin-iedit" nil t)
(with-eval-after-load "merlin"
  (define-key merlin-mode-map (kbd "C-c C-e")
    ; edits all occurrences of a given identifier. sometimes the behaviour is
    ; slightly buggy and doesn't catch everything, file a bug to merlin if so.
    (lambda () (interactive)
      (if (bound-and-true-p iedit-mode) (iedit-mode -1) (merlin-iedit-occurrences)))))


;;;; development UI

(global-linum-mode t)
(global-whitespace-mode t)

;; show matching parens (and other syntax delimiters like if-else, begin-end)
(setq show-paren-delay 0) ; needs to be set before the mode is switched on
(show-paren-mode t)

;; show full filename in mode line
(setq-default mode-line-buffer-identification
  (list 'buffer-file-name
    (propertized-buffer-identification "%12f")
    (propertized-buffer-identification "%12b")))

;; show current (line,column) instead of just (line)
(column-number-mode)

;; go to line[,column]
(global-set-key (kbd "s-g")
  (lambda () (interactive)
  (let ((a (split-string (read-string "Go to line[, column]: ") "," t)))
    (goto-line (or (string-to-number (car a)) 'line))
    (move-to-column (or (string-to-number (car (cdr a))) 0)))))

;; extra hotkeys for resizing windows
(global-set-key (kbd "C-s--") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-=") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-_") 'shrink-window)
(global-set-key (kbd "C-s-+") 'enlarge-window)


;;;; session management

(server-start)

;; save all tempfiles in $TMPDIR/emacs$UID/ instead of next to every fucking opened file
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(setq create-lockfiles nil)
