(setq prelude-guru nil)
(global-linum-mode t)

; Show the full path of file I'm editing in the status bar.
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
(global-set-key "\C-cz" 'show-file-name)

(setq tab-width 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2))

(setq evil-want-C-i-jump nil)

; Makes gr set up for search and replace.
;; (define-key evil-normal-state-map (kbd "g r") (lambda () (evil-ex "%s/")))

;; (define-key evil-normal-state-map (kbd "g p") 'projectile-find-file)
;; (define-key evil-normal-state-map (kbd "g s") 'projectile-grep)
;; (define-key evil-normal-state-map (kbd "g a") 'evil-ace-jump-char-mode)

;; (add-to-list 'load-path "/path/to/ack-and-a-half")
;; (require 'ack-and-a-half)
;; ;; Create shorter aliases
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
