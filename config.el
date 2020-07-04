;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sorin Muntean"
      user-mail-address "me@sorinmuntean.ro")

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
(setq doom-font (font-spec :family "Dank Mono" :size 16)
      ;; There are two ways to load a theme. Both assume the theme is installed and
      ;; available. You can either set `doom-theme' or manually load a theme with the
      ;; `load-theme' function. This is the default:
      doom-theme 'doom-one-light
      ;; If you use `org' and don't want your org files in the default location below,
      ;; change `org-directory'. It must be set before org loads!
      org-directory "~/org/"
      ;; This determines the style of line numbers in effect. If set to `nil', line
      ;; numbers are disabled. For relative line numbers, set this to `relative'.
      display-line-numbers-type nil

      ;; DOOM-EMACS' default GC threshold is 16MB. Give it 512MB instead.
      gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.7
      garbage-collection-messages nil

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
;; racket ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! racket-mode
  (add-hook! racket-mode #'racket-xp-mode #'parinfer-mode)

  (setq
    racket-smart-open-bracket-mode   nil
    racket-repl-buffer-name-function #'racket-repl-buffer-name-project
    racket-show-functions            '(racket-show-echo-area)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pollen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! pollen-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list '+format-on-save-enabled-modes 'mhtml-mode t)

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
;;; projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq projectile-project-search-path
      '("~/Sandbox/"
        "~/Playground/"
        "~/Playground/racket/"))