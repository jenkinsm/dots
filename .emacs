;;;; .emacs

;;; package setup
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
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

;;; editing cucumber
(require-package 'feature-mode)
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;;; editing haskell
(require-package 'haskell-mode)
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(eval-after-load 'haskell-mode ; should probably rebind this w/ leader key
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables '(haskell-tags-on-save t))

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
                               (set-fill-column 80)))

(add-hook 'ruby-mode-hook (lambda ()
                            (fci-mode)
                            (set-fill-column 80)))
