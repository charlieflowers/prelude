;; Load up evil using packages!

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(require 'clojure-mode)
(require 'cider)

(add-hook 'after-init-hook 'global-company-mode)

;; These next line gives me TAB back in Coffeescript mode!
(define-key evil-motion-state-map "\t" nil)

(define-key emacs-lisp-mode-map (kbd "C-x C-a") 'pp-macroexpand-last-sexp)
(define-key evil-normal-state-map (kbd "C-x s") 'prelude-ido-goto-symbol) ; this blocks Cx s so I can use spc spc
(define-key evil-normal-state-map (kbd "C-x C-s") 'prelude-ido-goto-symbol) ; this blocks Cx Cs so I can use spc spc

;; This beautiful stuff right here is from metasandwich.com. Deals with dashes and camel humps!
(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))

(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

;; This is from cofi's evil config ... it colors the "evil state" indicator, to make it stand out more.
(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

;; Certain modes need to start up in Emacs mode (especially the freaking repl! ... or does it?) And this makes that happen (taken from cofi evil)
(loop for (mode . state) in '((inferior-emacs-lisp-mode      . emacs)
                              (pylookup-mode                 . emacs)
                              (cider-repl-mode               . emacs)
                              (stacktrace-mode               . emacs)
                              (comint-mode                   . emacs)
                              (ebib-entry-mode               . emacs)
                              (ebib-index-mode               . emacs)
                              (ebib-log-mode                 . emacs)
                              (gtags-select-mode             . emacs)
                              (shell-mode                    . emacs)
                              (term-mode                     . emacs)
                              (bc-menu-mode                  . emacs)
                              (magit-branch-manager-mode-map . emacs)
                              (semantic-symref-results-mode  . emacs)
                              (rdictcc-buffer-mode           . emacs))
      do (evil-set-initial-state mode state))

;; These are key combos I have chosen personally.
;; (define-key evil-normal-state-map "g1" 'delete-other-windows) ;; Like emacs C-x 1 (I hope)
;; (define-key evil-normal-state-map "gb" 'ido-switch-buffer)    ;; Hopefully same as C-x b
;; (define-key evil-normal-state-map "gf" 'prelude-recentf-ido-find-file)
;; (define-key evil-normal-state-map "gz" 'prelude-indent-defun)
;; (define-key evil-normal-state-map "gc" 'prelude-ido-goto-symbol)
;; (define-key evil-normal-state-map "g4" 'prelude-duplicate-current-line-or-region)
;; (define-key evil-normal-state-map (kbd "g.") 'prelude-duplicate-and-comment-current-line-or-region)
;; (define-key evil-normal-state-map "g-" 'goto-last-change)
;; (define-key evil-normal-state-map "g=" 'goto-last-change-reverse)
;; (define-key evil-normal-state-map (kbd "g p") 'projectile-find-file)
;; (define-key evil-normal-state-map (kbd "g s") 'projectile-grep)
;; (define-key evil-normal-state-map (kbd "g a") 'evil-ace-jump-char-mode)
;; (define-key evil-normal-state-map (kbd "g m") 'magit-status)

; Now, transitioning to space as my leader (of course, g remains available too!)
(define-key evil-normal-state-map (kbd "SPC 1") 'delete-other-windows) ;; Like emacs C-x 1 (I hope)
(define-key evil-normal-state-map (kbd "SPC b") 'ido-switch-buffer)    ;; Hopefully same as C-x b
(define-key evil-normal-state-map (kbd "SPC f") 'prelude-recentf-ido-find-file)
(define-key evil-normal-state-map (kbd "SPC z") 'prelude-indent-defun)
(define-key evil-normal-state-map (kbd "SPC c") 'prelude-ido-goto-symbol)
(define-key evil-normal-state-map (kbd "SPC d") 'evil-goto-definition)
(define-key evil-normal-state-map (kbd "SPC 4") 'prelude-duplicate-current-line-or-region)
(define-key evil-normal-state-map (kbd "SPC .") 'prelude-duplicate-and-comment-current-line-or-region)
(define-key evil-normal-state-map (kbd "SPC -") 'goto-last-change)
(define-key evil-normal-state-map (kbd "SPC =") 'goto-last-change-reverse)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC s") 'projectile-grep)
(define-key evil-normal-state-map (kbd "SPC a") 'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC m") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC SPC") 'save-buffer)

; This is an attempt to get the "n" key back in grep mode.
;; (define-key ag-mode-map (kbd "n") 'evil-search-next)

; space space saves
(defun my-save ()
  (if (buffer-file-name)
      (evil-save (buffer-file-name) 42)))

; save the buffer when i exit insert mode
(add-hook 'evil-insert-state-exit-hook 'save-buffer)

;; todo, move the clojure and cider stuff to its own file
; enable eldoc in clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

; Some cider key mappings
(define-key cider-repl-mode-map (kbd "<home>") nil)
(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-,") 'complete-symbol)
(define-key cider-mode-map (kbd "C-c C-q") 'nrepl-close)
(define-key cider-mode-map (kbd "C-c C-Q") 'cider-quit)

;; Some clojure related key mappings
(define-key evil-normal-state-map (kbd "go") 'ace-window)

;; Specify cider history file
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")
;; To make the REPL history wrap around when its end is reached:
(setq cider-repl-wrap-history t)
;; To adjust the maximum number of items kept in the REPL history:
(setq cider-repl-history-size 2200) ; the default is 500

;; don't show the annoying stacktrace mode error buffer on error
(setq cider-show-error-buffer nil)

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer nil)

;; activate smartparens strict mode in clojure and cider repl
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

;; log communication with nrepl
(setq nrepl-log-messages t)

;; when switching buffers, don't show the nrepl-connection and nrepl-server buffers unless i press spc after cmd
;; (setq nrepl-hide-special-buffers t)

;; in the cider stacktrace error buffer, show all stacktrace frames
(setq cider-stacktrace-default-filters nil)

;; show the port number the repl is on in the repl buffer name
(setq nrepl-buffer-name-show-port t)

;; Prevent C-c C-k from prompting to save the file corresponding to the buffer being loaded, if it's modified:
(setq cider-prompt-save-file-on-load nil)

;; turn on rainbow-delimiters in clojure and cider
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; This makes sure no matter how many times i hit ESC, emacs won't blow away my window configuration
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(provide 'charlie-evil)
