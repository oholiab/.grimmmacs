(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-list
      '(
	evil
	helm
	))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package evil :ensure t)
(use-package evil-leader :ensure t)
(use-package helm :ensure t)
(use-package lispy :ensure t)
(use-package cyberpunk-theme :ensure t)

(require 'helm-config)
(helm-descbinds-mode)
(require 'evil-leader)
(require 'evil)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
;; File tree
(evil-leader/set-key "f f" 'helm-find-files)
;; Mode tree
(evil-leader/set-key "m e r" 'eval-region)
(evil-leader/set-key "m e b" 'eval-buffer)
;; Window movement
(evil-leader/set-key "w h" 'evil-window-left)
(evil-leader/set-key "w j" 'evil-window-down)
(evil-leader/set-key "w k" 'evil-window-up)
(evil-leader/set-key "w l" 'evil-window-right)
(evil-leader/set-key "w p" 'evil-window-prev)
(evil-leader/set-key "w n" 'evil-window-next)
;; Buffer movement
(evil-leader/set-key "b p" 'previous-buffer)
(evil-leader/set-key "b n" 'next-buffer)
(evil-leader/set-key "b b" 'helm-buffers-list)

(helm-mode 1)

;; NFI, probably needs changing
(require 'lispy)
(defun enable-lispy-mode () (lispy-mode 1))

(add-hook 'emacs-lisp-mode-hook       #'enable-lispy-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-lispy-mode)
(add-hook 'ielm-mode-hook             #'enable-lispy-mode)
(add-hook 'lisp-mode-hook             #'enable-lispy-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-lispy-mode)
(add-hook 'scheme-mode-hook           #'enable-lispy-mode)

(evil-mode t)
(menu-bar-mode -1)

;; Fuck you
(setq custom-file "/tmp/null")
(load custom-file 'noerror)
