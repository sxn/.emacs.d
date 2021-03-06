;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sorin Muntean"
      user-mail-address "me@sorinmuntean.ro")

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'auto-mode-alist '("_partials/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("_partials/**/.*\\.php\\'" . web-mode))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

(setq doom-font (font-spec :family "Dank Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Dank Mono")
      ;; There are two ways to load a theme. Both assume the theme is installed and
      ;; available. You can either set `doom-theme' or manually load a theme with the
      ;; `load-theme' function. This is the default:
      doom-theme 'doom-solarized-light
      ;; If you use `org' and don't want your org files in the default location below,
      ;; change `org-directory'. It must be set before org loads!
      org-directory "~/org/"
      ;; This determines the style of line numbers in effect. If set to `nil', line
      ;; numbers are disabled. For relative line numbers, set this to `relative'.
      display-line-numbers-type nil

      evil-move-cursor-back nil

      ;; disable hl-line globally because it is exceptionally slow
      global-hl-line-modes nil

      ;; DOOM-EMACS' default GC threshold is 16MB. Give it 512MB instead.
      gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.7
      garbage-collection-messages nil

      lsp-enable-file-watchers t
      lsp-file-watch-threshold 30000

      ;; by default, which-key triggers slower than I want
      which-key-idle-delay 0.3)

;; Emacs Mac Port binds `<swipe-left>` and `<swipe-right>' to
;; `mac-previous-buffer' and `mac-next-buffer'. Let's remove that silliness.
(unbind-key "<swipe-left>")
(unbind-key "<swipe-right>")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pollen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! pollen-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; racket ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! racket-mode
  (add-hook! racket-mode #'racket-xp-mode)

  (setq
   racket-smart-open-bracket-mode   nil
   racket-repl-buffer-name-function #'racket-repl-buffer-name-project
   racket-show-functions            '(racket-show-echo-area)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scribble ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! scribble-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; email ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-load-path! "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")
(after! mu4e

  (defun sm-make-mu4e-matcher (mailbox-name addresses)
    (lexical-let ((addresses addresses)
                  (prefix (concat "/" mailbox-name "/")))
      (lambda (msg)
        (when msg
          (-any-p (lambda (addr)
                    (mu4e-message-contact-field-matches msg '(:from :to :cc) addr))
                  addresses)))))

  (setq
   mu4e-get-mail-command    "mbsync personal"
   mu4e-update-interval     300
   mu4e-date-format         "%y/%m/%d"
   mu4e-headers-date-format "%Y/%m/%d %I:%M"
   ;; mu4e-headers-skip-duplicates t
   mu4e-headers-results-limit 500
   mu4e-attachment-dir        "~/Mail/.attachments"
   mu4e-view-use-gnus         nil

   sendmail-program                 "msmtp"
   message-send-mail-function       #'message-send-mail-with-sendmail
   message-sendmail-extra-arguments '("--read-envelope-from" "--read-recipients")
   message-sendmail-f-is-evil       t

   gnus-blocked-images ".*"

   mu4e-headers-fields
   '((:flags      . 4)
     (:human-date . 20)
     (:from       . 25)
     (:subject))

   mu4e-view-actions '(("Thread"          . mu4e-action-show-thread)
                       ("View in Browser" . mu4e-action-view-in-browser))

   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy 'ask-if-none

   mu4e-bookmarks `((:name "All Inboxes" :key ?i :query ,(string-join '("maildir:/sorinmuntean.ro/inbox" "maildir:/kigroup.de/inbox") " or "))
                    (:name "Unread Messages" :key ?u :query ,(string-join '("flag:unread" "flag:trashed" "maildir:/sorinmuntean.ro/junk" "maildir:/kigroup.de/junk") " AND NOT "))
                    (:name "Messages Today" :key ?t :query "date:today..now")
                    (:name "Messages Last 7 Days" :key ?w :query "date:7d..now")
                    (:name "Messages Last 30 Days" :key ?m :query "date:30d..now"))

   mu4e-contexts (list
                  (make-mu4e-context
                   :name "sorinmuntean.ro"
                   :match-func (sm-make-mu4e-matcher "sorinmuntean.ro" '("me@sorinmuntean.ro" "payment@sorinmuntean.ro" "contact@sorinmuntean.ro" "hello@sorinmuntean.ro"))
                   :vars '((user-mail-address           . "me@sorinmuntean.ro")
                           (mu4e-refile-folder          . "/sorinmuntean.ro/archive")
                           (mu4e-sent-folder            . "/sorinmuntean.ro/sent")
                           (mu4e-drafts-folder          . "/sorinmuntean.ro/drafts")
                           (mu4e-trash-folder           . "/sorinmuntean.ro/trash")
                           (mu4e-sent-messages-behavior . sent)))

                  (make-mu4e-context
                   :name "digital-loop.com"
                   :match-func (sm-make-mu4e-matcher "digital-loop.com" '("s.muntean@digital-loop.com"))
                   :vars '((user-mail-address           . "s.muntean@digital-loop.com")
                           (mu4e-refile-folder          . "/digital-loop.com/archive")
                           (mu4e-sent-folder            . "/digital-loop.com/sent")
                           (mu4e-drafts-folder          . "/digital-loop.com/drafts")
                           (mu4e-trash-folder           . "/digital-loop.com/trash")
                           (mu4e-sent-messages-behavior . sent)))

                  (make-mu4e-context
                   :name "kigroup.de"
                   :match-func (sm-make-mu4e-matcher "kigroup.de" '("s.muntean@kigroup.de"))
                   :vars '((user-mail-address           . "s.muntean@kigroup.de")
                           (mu4e-refile-folder          . "/kigroup.de/archive")
                           (mu4e-sent-folder            . "/kigroup.de/sent")
                           (mu4e-drafts-folder          . "/kigroup.de/drafts")
                           (mu4e-trash-folder           . "/kigroup.de/trash")
                           (mu4e-sent-messages-behavior . sent))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-docsets! 'racket-mode "Racket")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I can't say I dislike evil-snipe, but unfortunately I can't get _any_ of ;;;;;
;; its commands to work... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! evil-snipe (evil-snipe-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git/magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! magit
  (setq git-commit-summary-max-length 72))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! ivy
  ;; sometimes I want to create a file whose name is a subset of an already
  ;; existing one. For example if I've got `.env.example' and want to create
  ;; `.env'.
  (setq ivy-use-selectable-prompt t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sm-save-and-link-clipboard-image ()
  (interactive)

  (setq folder (read-directory-name "Directory:" "static/"))

  (setq filename
        (concat
         (make-temp-name
          (concat folder
                  (format-time-string "%Y%m%d_%H%M%S_")))
         ".png"))

  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))

  (call-process "pngpaste" nil nil nil filename)

  (if (file-exists-p filename)
      (insert (concat "![" filename "](" filename ")"))
    (message "Couldn't save image")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! paredit
  :hook ((emacs-lisp-mode lisp-mode racket-mode scheme-mode) . paredit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; php ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! php
  (setq-hook! 'php-mode-hook +format-with :none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq projectile-project-search-path
      '("~/Sandbox/"
        "~/Playground/"
        "~/Playground/racket/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! tramp
  (setq tramp-default-method "ssh")
  (setq tramp-syntax 'simplified)

  ;; Open files in a Docker container: /docker:my_container:/
  ;; Open files in a Docker container on a remote host: /ssh:host|docker:my_container:/
  ;;
  ;; Taken from https://willschenk.com/articles/2020/tramp_tricks/.
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)

  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
      a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it)))
