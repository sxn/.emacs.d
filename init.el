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
 ;;; Mac port
 mac-option-modifier 'meta
 mac-command-modifier 'hyper
 mac-mouse-wheel-smooth-scroll t
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
(add-to-list 'load-path (locate-user-emacs-file "vendor/prettier"))
(add-to-list 'load-path (locate-user-emacs-file "vendor/flow-for-emacs"))

;;; auto-compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;; Configure packaging
;;; use-package
(require 'use-package)
(with-no-warnings (require 'cl) (require 'package))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize nil)

;; Refresh packages
(unless package-archive-contents (package-refresh-contents))

;;; UI
;; Font, fullscreen
(when (window-system)
  (add-to-list 'default-frame-alist '(font . "Operator Mono-16"))
  (add-to-list 'default-frame-alist '(top . 5))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(width . 188))
  (add-to-list 'default-frame-alist '(height . 50))
  ;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(97 . 97))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

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

;;; Theme
;; Disable current theme before loading the new one
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(if (not (display-graphic-p))
    (load-theme 'wombat t)
  (require 'server)
  (unless (server-running-p)
    (server-start))
  (use-package color-theme-sanityinc-solarized
    :ensure t
    :config (load-theme 'sanityinc-solarized-dark t)))

;;; Configure built-in packages
;; autorevert
(use-package autorevert
  :config
  ;; Revert files that update on disk automatically. Ignores dirty
  ;; buffers.
  (global-auto-revert-mode))

;; compile
(use-package compile
  :init (setq compilation-scroll-output t))

;; electric
(use-package electric
  :config (progn
            (electric-indent-mode +1)
            (electric-pair-mode +1)))

;; ffap
(use-package ffap
  :commands ffap-other-window)

;; time
(use-package time
  :init (progn
          (setq display-time-default-load-average nil)
          (add-hook 'after-init-hook #'display-time-mode)))

;; paren
(use-package paren
  :config (show-paren-mode +1))

;; recentf
(use-package recentf
  :init (setq recentf-save-file (locate-user-emacs-file "recentf")
              recentf-max-saved-items 100
              recentf-max-menu-items 10
              recentf-auto-cleanup 60)
  :config (progn
            (add-to-list 'recentf-exclude "/.virtualenvs/")
            (add-to-list 'recentf-exclude "/elpa/")
            (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
            (add-to-list 'recentf-exclude "MERGE_MSG\\'")
            (add-to-list 'recentf-exclude "TAGS\\'")
            (add-to-list 'recentf-exclude ".el.gz")

            (recentf-mode +1)))

;; savehist
(use-package savehist
  :init (setq savehist-file (locate-user-emacs-file "savehist")
              savehist-additional-variables '(search ring regexp-search-ring))
  :config (savehist-mode +1))

;; saveplace
(use-package saveplace
  :init (save-place-mode +1))

;; files
(use-package files
  :init (setq auto-save-file-name-transforms `((".*" ,(concat local-temp-dir "/\\1") t))
              backup-directory-alist         `((".*" . ,local-temp-dir))
              backup-by-copying t
              create-lockfiles nil))

;; hl-line
(use-package hl-line
  :config (progn
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

;; uniquify
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; smex
(use-package smex
  :ensure t
  :init (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-;" . smex)))

;; ido
(use-package ido
  :init (setq ido-create-new-buffer 'always
              ido-use-filename-at-point 'guess
              ido-use-virtual-buffers t
              ido-handle-duplicate-virtual-buffers 2
              ido-max-prospects 10
              ido-ignore-extensions t)
  :config (progn
            (use-package ido-completing-read+
              :ensure t
              :config (ido-ubiquitous-mode +1))
            (use-package ido-vertical-mode
              :ensure t
              :init (setq ido-vertical-show-count t)
              :config (ido-vertical-mode +1))
            (ido-mode +1)
            (ido-everywhere +1)))

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :commands exec-path-from-shell-initialize
    :ensure t
    :init (add-hook 'after-init-hook #'exec-path-from-shell-initialize)))


;; EVIL
(use-package evil
  :load-path "vendor/evil"
  :pin manual
  :preface
  :init (setq evil-search-module #'evil-search
              evil-magic 'very-magic)
  :config (progn
            ;; Dependencies
            (use-package undo-tree
              :ensure t
              :diminish undo-tree-mode
              :commands global-undo-tree-mode
              :init (add-hook 'after-init-hook #'global-undo-tree-mode)
              :config (with-no-warnings
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

;; Project
(use-package projectile
  :ensure t
  :init (progn
          (setq projectile-enable-caching nil)
          (setq projectile-project-root-files-bottom-up '(".projectile" ".gitignore" ".git"))
          (setq projectile-switch-project-action 'projectile-commander)
          (add-hook 'after-init-hook #'projectile-global-mode))
  :config (progn
            (projectile-register-project-type 'angular2 '("protractor.conf.js")
                                              :compile "npm run build"
                                              :test "npm run test"
                                              :run "npm run start"
                                              :test-suffix ".spec")
            (projectile-register-project-type 'react '("package.json" "src" "src/component" "src/actions")
                                              :compile "yarn run build"
                                              :test "yarn run test"
                                              :run "yarn run start"
                                              :test-suffix "spec")))

;; Search - The silver searcher
(use-package ag
  :ensure t
  :init (setq ag-highlight-search t
              ag-reuse-buffers t))

;; File tree
(use-package neotree
  :ensure t
  :init (progn
          (use-package all-the-icons
            :ensure t
            :config (add-to-list 'all-the-icons-icon-alist
                                 '("\\.ts"
                                   all-the-icons-fileicon "typescript-alt"
                                   :height 1.0
                                   :face all-the-icons-red))
            (add-to-list 'all-the-icons-icon-alist
                         '("\\.tsx$"
                           all-the-icons-alltheicon "tsx-alt"
                           :height 1.0
                           :face all-the-icons-blue)))
          (defun sm-neotree-project-dir ()
            "Open NeoTree using the projectile root."
            (interactive)
            (let ((project-dir (projectile-project-root))
                  (file-name (buffer-file-name)))
              (if project-dir
                  (progn
                    (neotree-dir project-dir)
                    (neotree-find file-name))
                (progn
                  (message "Could not find projectile project root.")
                  (neotree))))))
  :config (progn
            (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
            (add-hook 'neotree-mode-hook
                      (lambda ()
                        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))
  :bind ("C-c n" . sm-neotree-project-dir))


;; Programming languages
;;; JavaScript & family
(use-package web-mode
  :ensure t
  :init (progn
          (use-package sass-mode
            :ensure t
            :mode ("\\.sass\\'" . sass-mode))
          (defadvice web-mode-highlight-part (around tweak-jsx activate)
            (if (equal web-mode-content-type "jsx")
                (let ((web-mode-enable-part-face nil))
                  ad-do-it)
              ad-do-it))
          (setq web-mode-code-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-style-padding 2
                web-mode-script-padding 2
                web-mode-enable-auto-closing t
                web-mode-enable-auto-expanding t
                web-mode-enable-auto-pairing nil
                web-mode-enable-current-element-highlight t
                web-mode-engines-alist '(("django" . "\\.html\\'"))
                web-mode-engines-alist '(("ctemplate" . "\\.component.html\\'")))

          (add-hook 'web-mode-hook
                    (lambda()
                      (local-unset-key (kbd "C-c C-h"))))
          )
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.php\\'" . web-mode)))

(use-package prettier-js
  :init (setq prettier-js-args '(
                                 "--single-quote" "true"
                                 "--print-width" "100"
                                 "--bracket-spacing" "true"
                                 ))
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'scss-mode-hook 'prettier-js-mode)
  (add-hook 'rxjs-mode-hook 'prettier-js-mode))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :ensure t
  :init (progn
          (add-to-list 'projectile-other-file-alist '("module.ts" . ("component.ts" "component.html" "component.sass component.css component.less")))
          (add-to-list 'projectile-other-file-alist '("component.html" . ("component.ts" "component.sass" "component.css" "component.less" "module.ts" )))
          (add-to-list 'projectile-other-file-alist '("component.ts" . ("component.html" "component.sass" "component.css" "component.less" "module.ts" )))
          (add-to-list 'projectile-other-file-alist '("component.sass" . ("component.html" "component.ts" "module.ts" )))
          (add-to-list 'projectile-other-file-alist '("tsx" "scss" ))
          (add-to-list 'projectile-other-file-alist '("scss" "tsx" )))
  :config (progn
            (defun sm-setup-tide ()
              (interactive)
              (tide-setup)
              (eldoc-mode +1)
              (tide-hl-identifier-mode +1)
              (setq-local company-tooltip-align-annotations t)
              (setq-local flycheck-check-syntax-automatically '(save new-line idle-change mode-enabled)))
            (use-package tide
              :ensure t
              :config (bind-keys :map tide-mode-map
                                 ("C-c d" . tide-documentation-at-point)
                                 ("C-c ." . tide-jump-to-definition)
                                 ("C-c ," . tide-jump-back)))
            (add-hook 'typescript-mode-hook #'sm-setup-tide)
            (setq typescript-indent-level 2)))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :bind ("C-c C-d" . rjsx-delete-creates-full-tag)
  :config (progn
            (use-package tern
              :ensure t
              :config (progn
                        (bind-keys :map tern-mode-keymap
                                   ("C-c ." . tern-find-definition)
                                   ("C-c ," . tern-pop-find-definition))))
            (use-package company-tern
              :ensure t
              :config (add-to-list 'company-backends #'company-tern))
            (setq mode-require-final-newline t
                  js-indent-level 2)
            (add-hook 'rjsx-mode-hook 'tern-mode)))

;;; Python
(use-package python
  :mode ("\\.py\\'"   . python-mode)
  :interpreter ("python" . python-mode)
  :config (progn
            (use-package pyvenv
              :ensure t
              :config (define-key python-mode-map (kbd "C-c v") 'pyvenv-workon))

            (use-package yapfify
              :ensure t
              :config (add-hook 'python-mode-hook 'yapf-mode))

            (use-package elpy
              :commands (elpy-enable)
              :ensure t
              :init (with-eval-after-load 'python (elpy-enable))
              :config (progn
                        (bind-keys :map python-mode-map
                                   ("C-c ." . elpy-goto-definition)
                                   ("C-c ," . pop-tag-mark))

                        (custom-set-variables
                         '(elpy-modules
                           (quote
                            (elpy-module-company
                             elpy-module-eldoc
                             elpy-module-pyvenv
                             elpy-module-sane-defaults))))))))

;;; Golang
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init (setq gofmt-command "goimports")
  :config (progn
            (use-package go-eldoc :ensure t)
            (use-package company-go
              :ensure t
              :config (add-to-list 'company-backends #'company-go))
            (bind-keys :map go-mode-map
                       ("C-c ." . godef-jump)
                       ("C-c C-p r" . go-play-region)
                       ("C-c C-p b" . go-play-buffer))
            (add-hook 'go-mode-hook #'go-eldoc-setup)
            (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
            (add-hook 'go-mode-hook #'yas-minor-mode)
            (add-hook 'before-save-hook #'gofmt-before-save)))


;; Misc languages
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")
(use-package json-mode
  :ensure t
  :config (setq json-reformat:indent-width 2
                js-indent-level 2)
  :mode (("\\.json\\'" . json-mode)
         ("\.eslintrc\\'" . json-mode)))
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))


;; Source control
(use-package magit
  :ensure t
  :init (setq magit-completing-read-function #'magit-ido-completing-read)
  :commands (magit-status git-commit-mode)
  :mode (("COMMIT_EDITMSG\\'" . git-commit-mode)
         ("MERGE_MSG\\'"      . git-commit-mode))
  :config (bind-keys :map magit-blame-mode-map
                     ("C-c q" . magit-blame-quit))
  :bind (("C-c m" . magit-status)
         ("C-c C-m b" . magit-blame)))


;; Code snippets
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :bind ("C-c C-y" . yas-insert-snippet))


;; Completion
(use-package company
  :diminish company-mode
  :ensure t
  :init (progn
          (setq company-idle-delay 0.15)
          (add-hook 'after-init-hook #'global-company-mode)))


;; Syntax checking
(use-package flycheck
  :commands flycheck-mode
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode)
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq-default flycheck-disabled-checkers '(sass javascript-jshint)))
  :config (flycheck-add-mode 'javascript-eslint 'rjsx-mode))


;; Productivity
(use-package flymd
  :ensure t
  :init
  (progn
    (defun sm-flymd-browser-function (url)
      (let ((process-environment (browse-url-process-environment)))
        (apply 'start-process (concat "firefox " url)
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
(use-package org
  :ensure t
  :commands (org-agenda org-capture)
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :preface (progn
             (defvar sm-org-dir (expand-file-name "~/OneDrive/Documents/Personal/org"))
             (defvar sm-org-main-file (expand-file-name (concat sm-org-dir "/Sorin.org")))
             (defvar sm-org-notes-file (expand-file-name (concat sm-org-dir "/refile.org")))
             (defvar sm-org-journal-file (expand-file-name (concat sm-org-dir "/journal.org")))
             (defvar sm-org-agenda-files-path sm-org-dir)
             ;; Remove empty LOGBOOK drawers on clock out
             (defun sm-remove-empty-drawer-on-clock-out ()
               (interactive)
               (save-excursion
                 (beginning-of-line 0)
                 (org-remove-empty-drawer-at "LOGBOOK" (point))))
             :config (progn
                       (setq
                        ;;; Completion
                        org-outline-path-complete-in-steps nil
                        ;;; Todos
                        ;; Log when todos are marked as done
                        org-log-done 'time
                        ;;; Custom capture templates
                        org-capture-templates
                        (quote (("t" "todo" entry (file sm-org-notes-file)
                                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                ("n" "note" entry (file sm-org-notes-file)
                                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                                ("j" "Journal" entry (file+datetree sm-org-journal-file)
                                 "* %?\n%U\n" :clock-in t :clock-resume t)
                                ("m" "Meeting" entry (file sm-org-notes-file)
                                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                ("h" "Habit" entry (file sm-org-notes-file)
                                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
                        ;; Refile up to the 2nd level of any file
                        org-refile-targets '((nil :maxlevel . 4)
                                             (org-agenda-files :maxlevel . 4))
                        ;;; Agenda
                        ;; Set path to agenda files.
                        org-agenda-files (list sm-org-agenda-files-path)
                        org-todo-keywords
                        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
                        org-todo-keyword-faces
                        (quote (("TODO" :foreground "red" :weight bold)
                                ("NEXT" :foreground "orange" :weight bold)
                                ("DONE" :foreground "forest green" :weight bold)
                                ("WAITING" :foreground "orange" :weight bold)
                                ("HOLD" :foreground "magenta" :weight bold)
                                ("CANCELLED" :foreground "pink" :weight bold)
                                ("MEETING" :foreground "yellow" :weight bold)
                                ("PHONE" :foreground "brown" :weight bold))))
                       ;; Text editing?
                       (progn
                         (add-hook 'org-mode-hook #'auto-fill-mode)
                         (add-hook 'org-clock-out-hook #'sm-remove-empty-drawer-on-clock-out)))))

(provide 'init)

;;; init.el ends here
