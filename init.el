(tool-bar-mode -1)

(setq
 user-full-name "Sorin Muntean"
 user-mail-address "me@sorinmuntean.ro"

 ;; Don't attempt to load packages.
 package-enable-at-startup nil

 ;; Ensure custom values are saved to an ignored file.
 custom-file (locate-user-emacs-file "custom.el")

 ;; EMACS' default GC threshold is <1MB. Give it 32MB instead.
 gc-cons-threshold (* 32 1024 1024)
 gc-cons-percentage 0.9
 garbage-collection-messages nil

 ;; Prefer source over bytecode if bytecode is outdated.
 load-prefer-newer t

 ;; Don't warn on redefinition
 ad-redefinition-action 'accept

 ;; Don't attempt to load `default.el'
 inhibit-default-init t

 ;; Mac port
 mac-option-modifier 'meta
 mac-command-modifier 'hyper
 mac-mouse-wheel-smooth-scroll nil)

(setq-default
 ;; Never use tabs.
 indent-tabs-mode nil

 ;; When using tabs (Go), they should be 2 spaces long.
 tab-width 2

 ;; Don't wrap long lines.
 truncate-lines t)


(add-to-list 'default-frame-alist '(font . "-*-Dank Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
