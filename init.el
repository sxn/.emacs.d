;;; init.el --- main config entry point -*- no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; Misc. builtin options
(setq
 user-full-name "Sorin Muntean" user-mail-address "me@sorinmuntean.ro"

 ;;; Don't try to load `default.el`
 inhibit-default-init t
 ;;; Don't show startup message
 inhibit-startup-message t


 ;; EMACS' default GC threshold is <1MB. Give it 16MB instead.
 gc-cons-threshold 16777216
 garbage-collection-messages nil

 ;; Don't interact with git when editing symlinked files.
 vc-follow-symlinks nil

 ;;; auto-compile
 load-prefer-newer t)


(set-language-environment "utf-8")

(setq-default

 custom-file (locate-user-emacs-file "custom.el")
 ;;; Don't use tabs by default
 indent-tabs-mode nil
 ;;; If for some reason you DO use tabs (Go), use two spaces for them
 tab-width 2
 ;;; Don't wrap long lines
 truncate-lines t)



;;; Paths
(setq default-directory (expand-file-name "~/"))

(defconst local-temp-dir (expand-file-name (locate-user-emacs-file "temp"))
  "The folder in which temp files should be stored.")



;;; Vendored packages
(add-to-list 'load-path (locate-user-emacs-file "vendor/dash"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/packed"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/auto-compile"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/use-package"))



;;; auto-compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)



;;; Configure packaging
;;; use-package
(require 'use-package)
(with-no-warnings
  (require 'cl)
  (require 'package))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize nil)

;; Refresh packages
(unless package-archive-contents
  (package-refresh-contents))



;;; UI
;; Font, fullscreen
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Fira Mono for Powerline-16"))
  (add-to-list 'default-frame-alist '(top . 5))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(width . 188))
  (add-to-list 'default-frame-alist '(height . 50))
  ;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; Transparency
  (add-to-list 'default-frame-alist '(alpha 95 95))
  (set-frame-parameter (selected-frame) 'alpha '(85 85)))

;; Use y and n as yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove GUI elements.
(dolist (mode '(blink-cursor-mode
                menu-bar-mode
                tool-bar-mode
                tooltip-mode
                scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Disable all bells
(setq
 ring-bell-function (lambda ())
 visible-bell nil)

;; Show current (row, col) in modeline.
(column-number-mode +1)
(line-number-mode +1)



;;; Theme stuff
;; Disable current theme before loading the new one
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(if (not (display-graphic-p))
    (load-theme 'wombat t)

  (require 'server)
  (unless (server-running-p)
    (server-start))

  (use-package color-theme-sanityinc-tomorrow
    :ensure t
    :config (load-theme 'sanityinc-tomorrow-eighties t)))



;;; Configure built-in packages
;; autorevert
(use-package autorevert
  :config
  ;; Revert files that update on disk automatically. Ignores dirty
  ;; buffers.
  (global-auto-revert-mode))

;; compile
(use-package compile
  :init
  (setq compilation-scroll-output t))

;; electric
(use-package electric
  :config
  (electric-indent-mode +1)
  (electric-pair-mode +1))

;; ffap
(use-package ffap
  :commands ffap-other-window)

;; time
(use-package time
  :init
  (setq display-time-default-load-average nil)
  (add-hook 'after-init-hook #'display-time-mode))

;; paren
(use-package paren
  :config
  (show-paren-mode +1))

;; recentf
(use-package recentf
  :init
  (setq recentf-save-file (locate-user-emacs-file "recentf")
        recentf-max-saved-items 100
        recentf-max-menu-items 10
        recentf-auto-cleanup 60)
  :config
  (progn
    (add-to-list 'recentf-exclude "/.virtualenvs/")
    (add-to-list 'recentf-exclude "/elpa/")
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (add-to-list 'recentf-exclude "MERGE_MSG\\'")
    (add-to-list 'recentf-exclude "TAGS\\'")
    (add-to-list 'recentf-exclude ".el.gz")

    (recentf-mode +1)))

;; savehist
(use-package savehist
  :init
  (setq savehist-file (locate-user-emacs-file "savehist")
        savehist-additional-variables '(search ring regexp-search-ring))
  :config
  (savehist-mode +1))

;; saveplace
(use-package saveplace
  :init
  (save-place-mode +1))

;; files
(use-package files
  :init
  (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
        backup-directory-alist         `((".*" . ,local-temp-dir))
        backup-by-copying t))

;; hl-line
(use-package hl-line
  :config
  (progn
    (define-global-minor-mode sm-global-hl-line-mode global-hl-line-mode
      (lambda ()
        ;; XXX: You can't turn off global-hl-line-mode on a per-buffer
        ;; basis so we can just build up our own version that doesn't
        ;; activate for a given list of modes.
        (when (not (memq major-mode (list 'eww-mode
                                          'term-mode
                                          'org-agenda-mode)))
          (hl-line-mode +1))))

    (sm-global-hl-line-mode)))

;; smex
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-;" . smex))
  :ensure t
  :init
  (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config
  (smex-initialize))

;; ido
(use-package ido
  :init
  (setq ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10
        ido-ignore-extensions t)
  :config
  (progn
    (use-package ido-ubiquitous
      :ensure t
      :config
      (ido-ubiquitous-mode +1))

    (use-package ido-vertical-mode
      :ensure t
      :init
      (setq ido-vertical-show-count t)
      :config
      (progn
        (ido-vertical-mode +1)))

    (ido-mode +1)
    (ido-everywhere +1)))

;; uniquify
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))



;;; Configure external packages
;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  :init
  (setq evil-search-module #'evil-search
        evil-magic 'very-magic)
  :config
  (progn
    ;; Dependencies
    (use-package undo-tree
      :ensure t
      :diminish undo-tree-mode
      :commands global-undo-tree-mode
      :init
      (add-hook 'after-init-hook #'global-undo-tree-mode)
      :config
      (with-no-warnings
        (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diffs t
              undo-tree-history-directory-alist `((".*" . ,local-temp-dir))
              undo-tree-auto-save-history t)))

    (evil-mode +1)
    
    (bind-keys :map evil-normal-state-map
               :prefix "\\"
               :prefix-map evil-leader-prefix-map
               ("b" . ido-switch-buffer)
               ("\\" . evil-ex-nohighlight))))

;; projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

;; ag
(use-package ag
  :ensure t
  
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

;; it's magit!
(use-package magit
  :ensure t
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
         ("MERGE_MSG\\'"      . git-commit-mode))
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-completing-read-function #'magit-ido-completing-read))

;; neotree
(use-package neotree
  :ensure t
  :init
  (progn
    (defun sm-neotree-project-dir ()
      "Open NeoTree using the projectile root."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name))
          (progn (message "Could not find projectile project root.")
                 (neotree))))))

  :config
  (progn
    (setq neo-smart-open t
          projectile-switch-project-action 'neotree-projectile-action)
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))
  :bind ("C-c n" . sm-neotree-project-dir))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :init
  (progn
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))
    (setq web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-markup-indent-offset 2

          web-mode-style-padding 2
          web-mode-script-padding 2

          web-mode-enable-auto-closing t
          web-mode-enable-auto-expanding t
          web-mode-enable-auto-pairing t
          web-mode-enable-current-element-highlight t

          web-mode-engines-alist '(("django" . "\\.html\\'")))))

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :init
    (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))

;; json mode
(use-package json-mode
  :mode (("\.json\\'" . json-mode)
         ("\.eslintrc\\'" . json-mode))


  :ensure t
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2))

;; js2
(use-package js2-mode
  :mode ("\.js\\'" . js2-mode)
  :ensure t
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning t
        js2-global-externs '("module" "require" "describe" "it" "process" "__dirname")))

;; rjxs-mode
(use-package rjsx-mode
  :ensure t
  :mode (("components?\\/.*\\.js\\'" . rjsx-mode))
  :bind ("C-c C-d" . rjsx-delete-creates-full-tag))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :init
  (use-package ivy
    :ensure t)
  :config (setq dumb-jump-selector 'ivy)
  :bind ("C-c ." . dumb-jump-go-other-window))

;; flycheck!
(use-package flycheck
  :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-temp-prefix ".flycheck")
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (flycheck-add-mode 'javascript-eslint 'rjsx-mode)))


;; Haskell
(use-package intero
  :load-path "vendor/intero/elisp"
  :init
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook #'intero-mode)
  :bind ("C-c ." . intero-goto-definition))


;; Python
(use-package company
  :diminish company-mode
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0.25)

    (add-hook 'after-init-hook #'global-company-mode))
  :config
  (bind-key "C-c C-y" #'company-yasnippet))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :diminish yas-minor-mode
  :ensure t
  :config
  (yas-reload-all))

(use-package pyvenv
  :ensure t)
(use-package python
  :mode ("\\.py\\'"   . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (use-package elpy
      :commands (elpy-enable)
      :ensure t
      :init
      (with-eval-after-load 'python (elpy-enable))
      :config
      (progn
        (bind-keys :map python-mode-map
                   ("C-c v" . pyvenv-workon)
                   ("C-c ." . elpy-goto-definition)
                   ("C-c ," . pop-tag-mark))

        (custom-set-variables
         '(elpy-modules
           (quote
            (elpy-module-company
             elpy-module-eldoc
             elpy-module-pyvenv
             elpy-module-sane-defaults))))))))



;; Markdown
(use-package flymd
  :ensure t
  :init
  (progn
    (defun sm-flymd-browser-function (url)
      (let ((process-environment (browse-url-process-environment)))
        (apply 'start-process
               (concat "firefox " url)
               nil
               "/usr/bin/open"
               (list "-a" "firefox" url)))))
  :config
  (setq flymd-browser-open-function 'sm-flymd-browser-function))

(use-package markdown-mode+
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))



(provide 'init)

;;; init.el ends here
