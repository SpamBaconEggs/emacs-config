
;; use ecb or use speedbar
(setq use-ecb t)

;;To use it's tagging system, you need to install exuberant-ctags.
;;In ubuntu, do this:
;;sudo apt-get install exuberant-ctags
(require 'ctags)

;;You also seem to need GNU Global
;;sudo apt-get install global
(require 'gtags)

;;Once you've loaded a project, generate tags for it by running this:
;;M-x projectile-tags

(require 'projectile)
(projectile-global-mode)
;; (global-set-key (kbd "M-.") 'projectile-find-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<speedbar
;;Speedbar. Should be lighter weight than ECB, maybe.
;;see http://www.emacswiki.org/emacs/SpeedBar
(if (not use-ecb)
    (progn
      ;;(require 'speedbar)
      ;;(speedbar 1)
      ;;Speedbar in the same window (not sure if it depeds on speedbar)
      (require 'sr-speedbar)
      (setq speedbar-show-unknown-files t) ; show all files
      ;; (setq speedbar-use-images nil) ; use text for buttons
      (setq sr-speedbar-right-side nil) ; put on left side
      )
  )
;;</speedbar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ecb>
;; ecb - emacs code browser (provides a UI layer on top of cedet and ede)
;; ecb gives you side bars for navigating directories, files and symbols
(if use-ecb
    (progn
      (require 'ecb)

      ;; Following the ecb README from the source directory
      ;;(require 'ecb-autoloads) ;; Switched off if you want to speed-up startup

      (setq stack-trace-on-error nil) ;;don’t popup Backtrace window
      (setq ecb-tip-of-the-day nil)
      ;;(setq ecb-auto-activate t)
      ;;(setq ecb-layout-name "left6")
      ;;(setq ecb-options-version "2.40")
      ;;(setq ecb-primary-secondary-mouse-buttons (quote mouse-1–mouse-2))
      ;;(setq ecb-source-path (quote ("~/")))
      )
  )
;;</ecb>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;todo - try helm and/or helm-projectile - impressive demo here:
;;https://github.com/emacs-helm/helm
;;http://tuhdo.github.io/helm-intro.html
;;todo - try ido-flx

;; To make the most of helm-projectile, install the following tools as well:
;; http://blog.newrelic.com/2015/01/28/grep-ack-ag/
;; ack-grep http://beyondgrep.com/install/
;; ag https://github.com/ggreer/the_silver_searcher

;;https://github.com/emacs-helm/helm/wiki
(if t
    (progn
      (message "enabling helm")
      (require 'helm)
      (require 'helm-config)
      ;; As per http://tuhdo.github.io/helm-intro.html :
      ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
      ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
      ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
      (global-set-key (kbd "C-c h") 'helm-command-prefix)
      (global-unset-key (kbd "C-x c"))

      (define-key helm-map
        (kbd "<tab>")
        'helm-execute-persistent-action) ; rebind tab to run persistent action
      (define-key helm-map
        (kbd "C-i")
        'helm-execute-persistent-action) ; make TAB works in terminal
      (define-key helm-map
        (kbd "C-z")
        'helm-select-action) ; list actions using C-z
      ;; Helm's M-x implementation. Shows help on function if you hit TAB,
      ;; and shows keybindings automatically if any are registered.
      (global-set-key (kbd "M-x") 'helm-M-x)
      ;; Make M-x fuzzy match, like idoflex
      (setq helm-M-x-fuzzy-match t)
      ;; Nicer, navigable kill-ring
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)

      (require 'helm-projectile)

      ;;https://github.com/syohex/emacs-helm-gtags
      ;;helm-gtags isn't working properly yet - needs
      ;;more work
      (require 'helm-gtags)
      (custom-set-variables
        '(helm-gtags-path-style 'relative)
        '(helm-gtags-ignore-case t)
        '(helm-gtags-auto-update t))
      ;;; Enable helm-gtags-mode
      (add-hook 'c-mode-hook 'helm-gtags-mode)
      (add-hook 'c++-mode-hook 'helm-gtags-mode)
      (add-hook 'asm-mode-hook 'helm-gtags-mode)

      (require 'helm-ack)
      (require 'helm-ag)
      (custom-set-variables
       ;; ignore cscope and GNU GLOBAL tags files, and make it work
       ;; with case sensitivity elegantly
       '(helm-ag-command-option " --smart-case --ignore '*cscope*' --ignore '*GT*' --ignore '*.html' "))
      (helm-projectile-on)
      (global-set-key (kbd "M-.") 'helm-gtags-find-tag)
      (global-set-key (kbd "<f1>") 'helm-gtags-dwim)
      ;;helm-mode enables helm completion in all Emacs commands using
      ;;completing-read, read-file-name, completion-at-point and
      ;;completing-read-multiple

      ;; https://github.com/emacs-helm/helm/blob/master/helm-imenu.el
      (require 'helm-imenu)

      (helm-mode 1)

      ;; enable helm-completion for projectile (but hopefully not
      ;; all of emacs
      (setq projectile-completion-system 'helm)
      )
  )



