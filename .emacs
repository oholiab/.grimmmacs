(require 'package)

;; Stop emacs from saving and modifying its own nonsense
(setq custom-file "/tmp/null")
(load custom-file 'noerror)

;; Ditch the menu bar and speed things up

(menu-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      inhibit-startup-hooks t)

;; Set default widths
(setq-default fill-column 72
	            tab-width 2)

;; Let me drop back into files at the same position
(save-place-mode t)

;; Only create a new output buffer for shell commands if one is needed
(setq-default async-shell-command-display-buffer nil
	            async-shell-command-buffer 'new-buffer)

;; Jump straight to help buffers
(setq help-window-select t)

;; Something daft to do with emacs not respecting $PATH
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Encoding nonsense to make pasting UTF-8 work
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Gather autosaves and backups in a sensible place
(setq temporary-file-directory "~/.emacstmp")
(if (not (file-directory-p temporary-file-directory))
    (mkdir temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory t)))

;; Load additional ELISP files in the configuration directory
(seq-map
 'load-file
 (seq-filter
  (lambda (x) (string= (file-name-extension x) "el"))
  ;; TODO: Make this portable
  (directory-files "~/.grimmmacs" t)))

;; Some lisp help
(setq show-paren-delay 0
      show-paren-when-point-inside-paren t)
(show-paren-mode t)

(setq eldoc-idle-delay 0.1)

;; Set up package system
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Pretty
(use-package cyberpunk-theme :ensure t)
(use-package highlight-parentheses :ensure t)

;; Input
(use-package evil :ensure t)
(use-package evil-leader :ensure t)
(use-package helm :ensure t)
(use-package lispy :ensure t)
(use-package which-key :ensure t)
(use-package auto-complete :ensure t)

;; Git
(use-package magit :ensure t)
(use-package evil-magit :ensure t)

;; I don't use either of these ever and I probably should!
(use-package projectile :ensure t)
(use-package helm-projectile :ensure t)
(use-package helm-describe-modes :ensure t)
(projectile-mode)

;; Languages
(use-package markdown-mode :ensure t)
(use-package rust-mode :ensure t)
(use-package cider
  :ensure t
  :config (add-hook 'clojure-mode-hook #'lispy-mode)
          (add-hook 'cider-repl-mode #'lispy-mode))
(use-package flymake :ensure t)
(use-package flymake-cursor :ensure t)
(use-package terraform-mode :ensure t)
(use-package puppet-mode :ensure t)
;; For elpy:
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; pip install yapf
(use-package elpy
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))
;; Appears to be broken (use-package virtualenvwrapper :ensure t)
(use-package go-mode
  :ensure t
  :config (add-hook 'go-mode-hook (lambda () (setq compile-command "go build ."))))
(use-package yaml-mode :ensure t)
(use-package fennel-mode :ensure t
  :config (add-hook 'fennel-mode-hook #'lispy-mode))

;; org-mode
(use-package org :ensure org-plus-contrib :pin org)
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme))))

;; org-mode hack for low emacs versions
(if (version< emacs-version "25")
    (progn
      (defalias 'outline-show-all 'show-all)))

;; Turn a bunch of the packages on
(require 'which-key)
(require 'helm-config)
(require 'evil-leader)
(require 'evil-magit)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'clojure-mode-hook #'lispy-mode)
(require 'evil)
(setq evil-want-C-i-jump nil)
(setq evil-esc-delay 0)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(setq which-key-enable-extended-define-key 1)
(setq which-key-idle-delay 0.001)
(which-key-mode)
(require 'highlight-parentheses)
(highlight-parentheses-mode)

;; Prettify
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹  " . nil)))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎  " . nil)))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤  " . nil)))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣  " . nil)))
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    `(("(\\(fn\\)[\[[:space:]]"
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "λ"))))
				      ("(\\(partial\\)[\[[:space:]]"
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "Ƥ"))))
				      ("(\\(comp\\)[\[[:space:]]"
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "∘"))))
				      ("\\(#\\)("
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "ƒ"))))
				      ("\\(#\\){"
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "∈"))))))))
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    `(("(\\(lambda\\) "
				       (0 (progn (compose-region (match-beginning 1)
								 (match-end 1) "λ"))))))))

;; Custom functions
(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun re-source-ssh-auth ()
  "Takes the contents of `~/.ssh_socket` and exports as SSH_AUTH_SOCK (for reattaching to ssh agent whilst in mosh)"
  (setenv "SSH_AUTH_SOCK" (replace-regexp-in-string "\n$" "" (get-file-contents "~/.ssh_socket"))))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions"
  nil)

(comment (defun auto-compile ()
	   (interactive)
	   (if (member major-mode '(go-mode))
	       (setq compile-command "go build ."))
	   (compile compile-command)))

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

(defun run-make ()
  (interactive)
  (shell-command-to-string "make"))

(defun reload-ff ()
  (interactive)
  (shell-command-to-string "osascript ~/reloadff.scpt"))

(defun describe-foo-at-point ()
          "Show the documentation of the Elisp function and variable near point.
	This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call
	"
	  (interactive)
	  (let (sym)
	    ;; sigh, function-at-point is too clever.  we want only the first half.
	    (cond ((setq sym (ignore-errors
                               (with-syntax-table emacs-lisp-mode-syntax-table
                                 (save-excursion
                                   (or (not (zerop (skip-syntax-backward "_w")))
                                       (eq (char-syntax (char-after (point))) ?w)
                                       (eq (char-syntax (char-after (point))) ?_)
                                       (forward-sexp -1))
                                   (skip-chars-forward "`'")
        	                   (let ((obj (read (current-buffer))))
                                     (and (symbolp obj) (fboundp obj) obj))))))
                   (describe-function sym))
                  ((setq sym (variable-at-point)) (describe-variable sym))
                  ;; now let it operate fully -- i.e. also check the
                  ;; surrounding sexp for a function call.
                  ((setq sym (function-at-point)) (describe-function sym)))))

(defun enter-jira-link (ticket)
  "expand an org-mode jira link for an interactively entered ticket"
  (interactive "sTicket #: ")
  (let* ((ticket-parts (split-string ticket "/"))
	 (ticket-no (nth (- (length ticket-parts) 1) ticket-parts)))
    (insert (concat "[[" jira-url "/browse/" ticket-no "][" ticket-no "]]"))))


;; Key bindings
;; Compile
(which-key-declare-prefixes "<SPC> c" "compile")
(evil-leader/set-key "c c" 'compile)

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

;; Smerge
(which-key-declare-prefixes "<SPC> s" "smerge")
(evil-leader/set-key-for-mode 'smerge-mode "s " smerge-mode-map)

;; Mode tree
(which-key-declare-prefixes "<SPC> m" "mode")
(which-key-declare-prefixes "<SPC> m e" "evaluate")
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m e r" 'eval-region)
(evil-leader/set-key-for-mode 'emacs-lisp-mode "m e b" 'eval-buffer)
(evil-leader/set-key-for-mode 'racket-mode "m e r" 'racket-send-region)
(evil-leader/set-key-for-mode 'racket-mode "m e f" 'racket-eval-defun-at-point)
(evil-leader/set-key-for-mode 'racket-mode "m e b" 'racket-run-and-switch-to-repl)
(evil-leader/set-key-for-mode 'clojure-mode "m e r" 'cider-eval-region)
(evil-leader/set-key-for-mode 'clojure-mode "m e b" 'cider-eval-buffer)
(evil-leader/set-key-for-mode 'clojure-mode "m e f" 'cider-eval-defun-at-point)
(evil-leader/set-key-for-mode 'python-mode "m e b" 'elpy-shell-send-buffer-and-go)
(which-key-declare-prefixes "<SPC> m c" "compile")
(evil-leader/set-key-for-mode 'rust-mode "m c c" 'rust-compile)
(which-key-declare-prefixes-for-mode 'python-mode "<SPC> m f" "find")
(evil-leader/set-key-for-mode 'python-mode "m f g" 'elpy-goto-definition)
(evil-leader/set-key-for-mode 'python-mode "m f r" 'elpy-rgrep-symbol)
(which-key-declare-prefixes-for-mode 'python-mode "<SPC> m t" "test")
(evil-leader/set-key-for-mode 'python-mode "m t t" 'elpy-test)
(which-key-declare-prefixes "<SPC> m s" "repl")
(evil-leader/set-key-for-mode 'racket-mode "m s i" 'racket-repl)
(evil-leader/set-key-for-mode 'clojure-mode "m s i" 'cider-jack-in)
(evil-leader/set-key-for-mode 'clojure-mode "m s s" 'cider-repl)
(evil-leader/set-key-for-mode 'python-mode "m s i" 'elpy-shell-switch-to-shell)
(evil-leader/set-key-for-mode 'python-mode "m s s" 'elpy-shell-switch-to-shell)
(which-key-declare-prefixes "<SPC> m c" "config")
(evil-leader/set-key-for-mode 'python-mode "m c" 'elpy-config)
(which-key-declare-prefixes "<SPC> m v" "venv")
(evil-leader/set-key-for-mode 'python-mode "m v a" 'pyvenv-activate)
(evil-leader/set-key-for-mode 'python-mode "m v d" 'pyvenv-deactivate)

;; Window movement
(which-key-declare-prefixes "<SPC> w" "window")
(evil-leader/set-key "w h" 'evil-window-left)
(evil-leader/set-key "w H" 'evil-window-move-far-left)
(evil-leader/set-key "w j" 'evil-window-down)
(evil-leader/set-key "w J" 'evil-window-move-very-bottom)
(evil-leader/set-key "w k" 'evil-window-up)
(evil-leader/set-key "w K" 'evil-window-move-very-top)
(evil-leader/set-key "w l" 'evil-window-right)
(evil-leader/set-key "w L" 'evil-window-move-far-right)
(evil-leader/set-key "w p" 'evil-window-prev)
(evil-leader/set-key "w n" 'evil-window-next)
(evil-leader/set-key "w -" 'evil-window-split)
(evil-leader/set-key "w |" 'evil-window-vsplit)
(evil-leader/set-key "w d" 'evil-window-delete)
(evil-leader/set-key "w N" 'evil-window-new)

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
(evil-leader/set-key-for-mode 'org-mode "m t i" 'org-insert-todo-heading-respect-content)
(which-key-declare-prefixes-for-mode 'org-mode "<SPC> m l" "link")
(evil-leader/set-key-for-mode 'org-mode "m l o" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "m l i" 'org-insert-link)
(evil-leader/set-key-for-mode 'org-mode "m l j" 'enter-jira-link)

;; Projectile
(which-key-declare-prefixes "<SPC> p" "projectile")
(evil-leader/set-key "p p" 'projectile-switch-project)
(evil-leader/set-key "p f" 'projectile-find-file)

;; Toggles
(which-key-declare-prefixes "<SPC> t" "toggle")
(evil-leader/set-key "t w" 'toggle-word-wrap)
(evil-leader/set-key "t t" 'toggle-truncate-lines)
(evil-leader/set-key "t n" 'linum-mode)
(evil-leader/set-key-for-mode 'org-mode "t l" 'org-toggle-link-display)

;; Quit
(which-key-declare-prefixes "<SPC> q" "quit")
(evil-leader/set-key "q S" 'save-buffers-kill-terminal)
(evil-leader/set-key "q q" 'kill-emacs)

(add-hook 'emacs-lisp-mode-hook       #'lispy-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispy-mode)
(add-hook 'ielm-mode-hook             #'lispy-mode)
(add-hook 'lisp-mode-hook             #'lispy-mode)
(add-hook 'lisp-interaction-mode-hook #'lispy-mode)
(add-hook 'scheme-mode-hook           #'lispy-mode)

(evil-mode t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (org . t)
   (shell . t)
   (python . t)))

(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "blue" :weight bold))
        ("DEFERRED" . (:foreground "gray" :weight bold))))

;; Bunch of python nonsense
(require 'flymake)
(require 'flymake-cursor)

(defun flymake-pylint-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	(local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
  (list "pyflakes" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))

; Load flymake on non-temp buffers
(add-hook 'python-mode-hook (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))
(setq elpy-test-pytest-runner-command '("python" "-m" "pytest"))
(setq elpy-test-runner 'elpy-test-pytest-runner)
(auto-complete-mode)
(show-paren-mode 1)

;; More org magic
(add-hook 'org-mode-hook (lambda ()
			   (if (string= (buffer-substring 1 9) "#+REVEAL")
			       (progn (require 'ox-reveal)
				      (evil-leader/set-key-for-mode
					'org-mode "c c" 'org-reveal-export-to-html-and-browse))
			     (message "Not reveal mode"))))

(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook (lambda ()
			   (toggle-truncate-lines)
			   (toggle-word-wrap)))

(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)

