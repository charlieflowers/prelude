;; Load up evil using packages!

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(define-key emacs-lisp-mode-map (kbd "C-x C-a") 'pp-macroexpand-last-sexp)

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
(define-key evil-normal-state-map "g1" 'delete-other-windows) ;; Like emacs C-x 1 (I hope)
(define-key evil-normal-state-map "gb" 'ido-switch-buffer)    ;; Hopefully same as C-x b
