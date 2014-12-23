
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

(global-set-key (kbd "<f1>") 'projectile-find-tag)

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
;;todo - try ido-flx

;;https://github.com/emacs-helm/helm/wiki
(if nil
    (progn
      (message "enabling helm")
      (require 'helm)
      (require 'helm-config)
      (require 'helm-projectile)
      ;;helm-mode enables helm completion in all Emacs commands using
      ;;completing-read, read-file-name, completion-at-point and
      ;;completing-read-multiple
      (helm-mode 1))
  )
