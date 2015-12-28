;;;; .emacs

;;; some to-dos
;; * appropriately color tab bar

;;; package setup
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ; ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;; theme
;; baseline
(load-theme 'wombat t)

;; hide menu bar
(menu-bar-mode -1)

;;; evil mode
;; leader key (must be set up before evil, otherwise leader won't be enabled
;; in initial buffers
(require-package 'evil-leader)
(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require-package 'evil)
(require 'evil)
;; enable
(evil-mode 1)

;; mode-dependent cursor highlighting (doesn't seem to work in terminal)
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; make esc quit everything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; tabs
(require-package 'elscreen)
(require-package 'evil-tabs)
(global-evil-tabs-mode t)

;;; projectile / project management
(require-package 'projectile)
(require 'projectile)

;; enable globally (on a trial basis)
(projectile-global-mode)

;; TODO
;; at some point, should probably integrate this with helm
(evil-leader/set-key-for-mode 'projectile-mode
  "a" 'projectile-ag)

;;; markdown
(require-package 'markdown-mode)
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-css-path "markdown.css")

;;; ruby.
(require-package 'flymake-ruby)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;; editing cucumber
(require-package 'feature-mode)
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; req ruby_parser ~>2.0
;; also, seems broken
(evil-leader/set-key-for-mode 'feature-mode "d" 'feature-goto-step-definition)

;;; editing haskell
(require-package 'haskell-mode)
(require 'haskell-mode)

(eval-after-load 'haskell-mode ; should probably rebind this w/ leader key
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(org-agenda-files (quote ("~/org/ai.org"))))

(evil-leader/set-key-for-mode 'haskell-mode "e" 'ghc-display-errors)

;; interactive mode
(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

     (evil-leader/set-key-for-mode 'haskell-mode "l" 'haskell-process-load-or-reload)
     (evil-leader/set-key-for-mode 'haskell-mode "z" 'haskell-interactive-switch)))

;; backend functionality
(require-package 'ghc)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; yaml
(require-package 'yaml-mode)
(require 'yaml-mode)


;;; org-mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "ON HOLD" "DONE")))


;; bindings
(evil-leader/set-key-for-mode 'org-mode "a" 'org-agenda)
(evil-leader/set-key-for-mode 'org-mode "c" 'org-cycle)
(evil-leader/set-key-for-mode 'org-mode "r" 'org-archive-subtree-default-with-confirmation)
(evil-leader/set-key-for-mode 'org-mode "t" 'org-todo)

;;; helm
(require-package 'helm)
(require 'helm)
(require 'helm-config)

(helm-mode 1)

(evil-leader/set-key "h" 'helm-command-prefix)

;;; powerline
; not working wholly properly at present
;(require-package 'powerline)
;(require-package 'powerline-evil)
;(require 'powerline-evil)
;(powerline-evil-vim-color-theme)
;(display-time-mode t)

;;; general
;; spaces instead of tabs
(setq-default tab-width 4 indent-tabs-mode nil)

;; auto-indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; match the indentation rules on the file being edited
(require-package 'dtrt-indent)
(dtrt-indent-mode 1)

;; line numbering
(global-linum-mode t)
(setq linum-format "%4d \u2502 ") ; this will probaby fail someday

;; prevent backup files from cluttering
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; utf-8 
(prefer-coding-system 'utf-8)

;;; column limit mark
(require-package 'fill-column-indicator)
(setq fci-rule-character ?\u2503)
(setq fci-rule-character-color "#424242") ; wombat mode specific

;; per-mode configuration
(add-hook 'python-mode-hook (lambda ()
                              (fci-mode)
                              (set-fill-column 80)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (fci-mode)
                                  (set-fill-column 80)))

(add-hook 'feature-mode-hook (lambda ()
                               (fci-mode)
                               (set-fill-column 80)
                               (setq show-trailing-whitespace t)))

(add-hook 'ruby-mode-hook (lambda ()
                            (fci-mode)
                            (set-fill-column 80)
                            (setq show-trailing-whitespace t)))

(add-hook 'haskell-mode-hook (lambda ()
                               (fci-mode)
                               (set-fill-column 80)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
