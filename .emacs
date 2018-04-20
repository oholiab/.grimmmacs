(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Pretty
(use-package cyberpunk-theme :ensure t)

(use-package evil :ensure t)
(use-package evil-leader :ensure t)
(use-package helm :ensure t)
(use-package lispy :ensure t)
(use-package evil-lispy :ensure t)
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package which-key :ensure t)
(use-package markdown-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package cider :ensure t)
(use-package projectile :ensure t)
(projectile-mode)
(use-package helm-projectile :ensure t)
(use-package org :ensure org-plus-contrib :pin org)
(use-package flymake :ensure t)
(use-package flymake-cursor :ensure t)
;; For elpy:
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; pip install yapf
(use-package elpy :ensure t)
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))

(if (version< emacs-version "25")
    (progn
      (defalias 'outline-show-all 'show-all)))

(if (file-exists-p "~/.grimmmacs/jira.el")
    (load "~/.grimmmacs/jira.el"))

(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun re-source-ssh-auth ()
  "Takes the contents of `~/.ssh_socket` and exports as SSH_AUTH_SOCK (for reattaching to ssh agent whilst in mosh)"
  (setenv "SSH_AUTH_SOCK" (replace-regexp-in-string "\n$" "" (get-file-contents "~/.ssh_socket"))))

(require 'which-key)
(require 'helm-config)
(require 'evil-leader)
(require 'evil-magit)
(require 'evil-lispy)
(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(add-hook 'clojure-mode-hook #'evil-lispy-mode)
(require 'evil)
(setq evil-want-C-i-jump nil)
(setq evil-esc-delay 0)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(setq which-key-enable-extended-define-key 1)
(setq which-key-idle-delay 0.001)
(which-key-mode)

;; Prettify
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹  " . nil)))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎  " . nil)))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤  " . nil)))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣  " . nil)))

(defun run-or-raise-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun run-or-raise-ielm-buffer ()
  "Create or visit a `ielm' buffer."
  (interactive)
  (if (not (get-buffer "*ielm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ielm))
    (switch-to-buffer-other-window "*ielm*")))

;; Meta stuff
(which-key-declare-prefixes "<SPC> <SPC>" "meta")
(evil-leader/set-key "<SPC> t" 'run-or-raise-term-buffer)
(evil-leader/set-key "<SPC> r" 'run-or-raise-ielm-buffer)

;; Help tree
(which-key-declare-prefixes "<SPC> h" "help")
(evil-leader/set-key "h" help-map)

;; File tree
(which-key-declare-prefixes "<SPC> f" "file")
(evil-leader/set-key "f f" 'helm-find-files)

;; Mode tree
(which-key-declare-prefixes "<SPC> m" "mode")
(which-key-declare-prefixes "<SPC> m e" "evaluate")
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m e r" 'eval-region)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m e b" 'eval-buffer)
(evil-leader/set-key-for-mode 'clojure-mode "m e r" 'cider-eval-region)
(evil-leader/set-key-for-mode 'clojure-mode "m e b" 'cider-eval-buffer)
(which-key-declare-prefixes "<SPC> m c" "compile")
(evil-leader/set-key-for-mode 'rust-mode "m c c" 'rust-compile)
(which-key-declare-prefixes-for-mode 'python-mode "<SPC> m f" "find")
(evil-leader/set-key-for-mode 'python-mode "m f g" 'elpy-goto-definition)
(evil-leader/set-key-for-mode 'python-mode "m f r" 'elpy-rgrep-symbol)
(which-key-declare-prefixes-for-mode 'python-mode "<SPC> m t" "test")
(evil-leader/set-key-for-mode 'python-mode "m t t" 'elpy-test)
(which-key-declare-prefixes "<SPC> m s" "repl")
(evil-leader/set-key-for-mode 'clojure-mode "m s i" 'cider-jack-in)
(evil-leader/set-key-for-mode 'clojure-mode "m s s" 'cider-repl)
(which-key-declare-prefixes "<SPC> m c" "config")
(evil-leader/set-key-for-mode 'python-mode "m c" 'elpy-config)
(which-key-declare-prefixes "<SPC> m v" "venv")
(evil-leader/set-key-for-mode 'python-mode "m v a" 'pyvenv-activate)
(evil-leader/set-key-for-mode 'python-mode "m v d" 'pyvenv-deactivate)

;; Window movement
(which-key-declare-prefixes "<SPC> w" "window")
(evil-leader/set-key "w h" 'evil-window-left)
(evil-leader/set-key "w j" 'evil-window-down)
(evil-leader/set-key "w k" 'evil-window-up)
(evil-leader/set-key "w l" 'evil-window-right)
(evil-leader/set-key "w p" 'evil-window-prev)
(evil-leader/set-key "w n" 'evil-window-next)
(evil-leader/set-key "w -" 'evil-window-split)
(evil-leader/set-key "w |" 'evil-window-vsplit)
(evil-leader/set-key "w d" 'evil-window-delete)

;; Buffer movement
(which-key-declare-prefixes "<SPC> b" "buffer")
(evil-leader/set-key "b p" 'previous-buffer)
(evil-leader/set-key "b n" 'next-buffer)
(evil-leader/set-key "b b" 'helm-buffers-list)
(evil-leader/set-key "b d" 'kill-this-buffer)
(evil-leader/set-key "b D" 'kill-buffer-and-window)

;; Git
(which-key-declare-prefixes "<SPC> g" "git")
(evil-leader/set-key "g s" 'magit-status)
(evil-leader/set-key "g b" 'magit-blame)
(evil-leader/set-key "g d" 'magit-diff)
(evil-leader/set-key "g p" 'magit-push)
(evil-leader/set-key "g r" 'magit-remote-config-popup)

;; LISP - why doesn't this work???
(which-key-declare-prefixes "<SPC> k" "lisp")
(evil-leader/set-key "k" lispy-mode-map)
(helm-mode 1)

(defun run-make ()
  (interactive)
  (shell-command-to-string "make"))

(defun reload-ff ()
  (interactive)
  (shell-command-to-string "osascript ~/reloadff.scpt"))

;; Command execution
(which-key-declare-prefixes "<SPC> x" "execute")
(evil-leader/set-key "x m" 'run-make)
(evil-leader/set-key "x r" 'reload-ff)
;; ORG
(which-key-declare-prefixes "<SPC> o" "org")
(evil-leader/set-key "o l" 'org-store-link)
(evil-leader/set-key "o a" 'org-agenda)
(evil-leader/set-key "o c" 'org-capture)
(evil-leader/set-key "o b" 'org-iswitchb)
(which-key-declare-prefixes-for-mode 'org-mode "<SPC> m t" "todo")
(evil-leader/set-key-for-mode 'org-mode "m t t" 'org-todo)
(evil-leader/set-key-for-mode 'org-mode "m t i" 'org-insert-todo-heading)
(which-key-declare-prefixes-for-mode 'org-mode "<SPC> m l" "link")
(evil-leader/set-key-for-mode 'org-mode "m l o" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "m l i" 'org-insert-link)
;; Projectile
(which-key-declare-prefixes "<SPC> p" "projectile")
(evil-leader/set-key "p p" 'projectile-switch-project)
(evil-leader/set-key "p f" 'projectile-find-file)

;; Quit
(which-key-declare-prefixes "<SPC> q" "quit")
(evil-leader/set-key "q S" 'save-buffers-kill-terminal)


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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (sh . t)
   (python . t)))

(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "blue" :weight bold))
        ("DEFERRED" . (:foreground "gray" :weight bold))))

(require 'flymake)
(require 'flymake-cursor)

(defun flymake-pylint-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
		             (local-file (file-relative-name
			                          temp-file
						                       (file-name-directory buffer-file-name))))
								          (list "pyflakes" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))

; Load flymake on non-temp buffers
(add-hook 'python-mode-hook (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))
(setq elpy-test-pytest-runner-command '("python" "-m" "pytest"))
