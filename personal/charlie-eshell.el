;; These are charlie's customizations to eshell

(require 'evil)


(defun macroexpand-point ()
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))




(defmacro charlie-magic (original-fn)
  `(defun ,(intern (concat "charlie-insert-first-" (symbol-name original-fn))) (&optional arg)
     (interactive "p")
     (evil-insert-state)
     (,symbol-name arg)))







(defun make-go-insert-state-first-fn (original-symbol &optional arg)
  (let (new-symbol-name) (concat "charlie-insert-first-" (symbol-name original-symbol))
       (fset (intern new-symbol-name) (lambda (&optional arg) (
                                                             (evil-insert-state)
                                                             (funcall original-symbol arg))))))










(defun charlie-do-eshell-command (&optional arg)
  (interactive "p")
  (evil-change-state 'insert)
  (eshell-send-input arg))

(defun charlie-go-to-insert-mode-first (original-symbol &optional arg)
  (interactive "p")

  (evil-change-state 'insert)
  (funcall action-fn arg))

(evil-define-key 'normal eshell-mode-map
  (kbd "<up>") 'eshell-previous-input)

(evil-define-key 'normal eshell-mode-map
  (kbd "C-<up>") 'evil-previous-line)

(evil-define-key 'normal eshell-mode-map
  (kbd "<down>") 'eshell-next-input)

(evil-define-key 'normal eshell-mode-map
  (kbd "C-<down>") 'evil-next-line)

(evil-define-key 'normal eshell-mode-map
  (kbd "RET") 'charlie-do-eshell-command)

(evil-define-key 'insert eshell-mode-map
  (kbd "<up>") 'eshell-previous-input)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-<up>") 'evil-previous-line)

(evil-define-key 'insert eshell-mode-map
  (kbd "<down>") 'eshell-next-input)

(evil-define-key 'insert eshell-mode-map
  (kbd "C-<down>") 'evil-next-line)

(provide 'charlie-eshell)
