;; Load up evil using packages!

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

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
