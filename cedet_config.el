;; RMF's file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up package manager bits, as per http://www.logilab.org/173886

;; this is intended for manually installed libraries
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; load the package system and add some repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install a hook running post-init.el *after* initialization took place
(add-hook 'after-init-hook (lambda () (load "post-init.el")))

;; Do here basic initialization, (require) non-ELPA packages, etc.

;; disable automatic loading of packages after init.el is done
(setq package-enable-at-startup nil)
;; and force it to happen now
(package-initialize)
;; NOW you can (require) your ELPA packages and configure them as normal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido - nice quick buffer option completion
(require 'ido)
;;(setq ido-enable-flex-matching t)
(ido-mode t)
(setq ido-everywhere t)
;; ido-ubiquitous - Does what you were really hoping for when you did
;;(require 'ido-ubiquitous)
;;(setq ido-ubiquitous-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing/viewing of markdown files
;; use git-hub-flavoured markdown instead of standard
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; standard markdown flavour is below
;;(autoload 'markdown-mode "markdown-mode"
;;  "Major mode for editing Markdown files" t)
;;; Markdown mode, choosing the github-flavoured-mode, as that gives
;;; syntax highlighting. Woot!
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
;; see http://stackoverflow.com/a/18443287
(setq markdown-command "markdown2 -x fenced-code-blocks -x tables")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xscope.el for source code browsing, jumping to definitions, etc.
;; https://github.com/dkogan/xcscope.el
(add-to-list 'load-path
             "~/.emacsecb.d/xcscope")
(require 'xcscope)
(cscope-setup)
;; keybinding for file finding
;; (global-set-key (kbd "C-o") 'cscope-find-this-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elpy for Python goodies
;; See: https://github.com/jorgenschaefer/elpy/wiki/Installation

;; First to do this on command line: pip install elpy jedi
(require 'elpy)
(package-initialize)
(elpy-enable)
;; Rope (the default code completer for elpy) is abominably slow.
;; Use Jedi instead. (WIP)
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path
             "~/.emacsecb.d/cedet-bzr/trunk")
;;(load-file "/home/fanner/EmacsIDE/cedet-bzr/trunk/cedet-devel-load.el")
(require 'cedet-devel-load)
(require 'cedet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Personal preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set-specifier top-toolbar-visible-p nil)
;;(setq top-toolbar-visible-p nil)
;;Line numbers
(require 'linum)
(setq linum-mode t)

;;Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;;Use 4 spaces in c mode
(setq c-basic-offset 4)
;;Use 4 spaces for tabs
(setq default-tab-width 4)
;;Automatically indent after newlines so you don't have to hit TAB
(require 'cc-mode)
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))
;;Guess the default offset in files automatically
(add-to-list 'load-path
             "~/.emacsecb.d/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)


;;Highlight whitespace issues
(require 'whitespace)
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
;; Cleanup whitespace issues
(setq whitespace-action '(cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Autocompletion - basic setup
(add-to-list 'ac-dictionary-directories "~/.emacsecb.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
;;Seems to hook up to semantic automatically, so C/C++ autocompletion
;;works at this point.
(require 'ac-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;directory and module browsing using ecb and cedet
;;Following http://altom.ro/blog/emacs-python-ide-recipe

;; cedet - coding environment for emacs
;;(add-to-list 'load-path
;;             "/home/fanner/EmacsIDE/cedet-bzr/trunkecb")
(require 'cedet-cscope)

;; ede - project management which is part of cedet
(global-ede-mode 1)
;; lets ede use cscope for finding source files in projects
(setq ede-locate-setup-options
      '(ede-locate-cscope
        ede-locate-base))
;; katana 2.0 project
(ede-cpp-root-project "katana2.0" :file "/workspace/fanner/katana2.0/SConstruct"
     )

;; katana 1.6 project
(ede-cpp-root-project "katana1.6" :file "/workspace/fanner/katana1.6/SConstruct"
     )

;; tputils project
(ede-cpp-root-project "tputils" :file "/workspace/fanner/tputils/README"
     )

;; emacs ecb project
(ede-cpp-root-project ".emacsecb" :file "/home/fanner/.emacsecb.d/init.el"
     )

;; sprint planner project
(ede-cpp-root-project "sprint_planner" :file "/workspace/fanner/katana_sprint_planner/KatanaSprintPlanner.py"
     )

;; katana qa resources
(ede-cpp-root-project "qa_resources" :file "/workspace/Katana/QA_Resources/finder.sh"
     )

;; test harness for foundry/katana
(ede-cpp-root-project "test_harness" :file "/workspace/Katana/TestHarness/finder.sh"
     )

;; web sandbox
(ede-cpp-root-project "web" :file "/var/www/finder.sh"
     )

(defun my-find-file-in-project (filesubstring)
  "EDE's file finding is a bit pants. It doesn't make use of previously
   hashed/searched results correctly, and then doesn't give you the option
   to choose between multiple similarly named files (just picks the first).
   So this is an attempt to do a bit better by using cedet-cscope and ido
   directly."
  (interactive "sFind file: ")
  (require 'cedet-cscope)
  (require 'ido)
  (let ((default-directory (cscope-search-directory-hierarchy default-directory)))
    (let ((filelist (cedet-cscope-expand-filename filesubstring)))
      (setq filename (ido-completing-read "Select file: " filelist))
      (find-file filename)
      )
    )
)

(global-set-key (kbd "C-o") 'my-find-file-in-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ecb - emacs code browser (uses cedet)
(add-to-list 'load-path
             "~/.emacsecb.d/ecb")
(require 'ecb)

;; ggtags - for GNU Global tags and Exuberant C Tags (depending on how GNU GLOBAL was compiled)
(add-to-list 'load-path "~/.emacsecb.d/ggtags")
(require 'ggtags)
;;   enable ggtags for c modes
(add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))
;;(require 'ctags)
;; Bind cedet navigation window to right-mouse button
;;(global-cedet-m3-minor-mode 1)
;; Following the ecb README from the source directory
;;(require 'ecb-autoloads) ;; Switched off if you want to speed-up startup

(setq stack-trace-on-error nil) ;;don’t popup Backtrace window
(setq ecb-tip-of-the-day nil)
(setq ecb-auto-activate t)
;;(setq ecb-layout-name "left6")
;;(setq ecb-options-version "2.40")
;;(setq ecb-primary-secondary-mouse-buttons (quote mouse-1–mouse-2))
;;(setq ecb-source-path (quote ("~/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C/C++ Source Browsing, Completion, Etc. (ECB, using CEDET, using Semantic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Following http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;and http://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2/12716443#12716443

;;Program to parse and build tags for browsing (non-LISP, so fast)
(mapc (lambda (MODE) (add-to-list 'semantic-default-submodes MODE))
      '(global-semantic-mru-bookmark-mode
        global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-stickyfunc-mode
;;        global-cedet-m3-minor-mode ;; not available anymore in 2.0 - the devs are not sure why it was not ported
        global-semantic-highlight-func-mode
        global-semanticdb-minor-mode))

(semantic-mode 1)
(require 'semantic/ia)

;;(require 'etags)
;;(require 'gtags)
(require 'semantic/db)
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/lex)
(require 'semantic/lex-spp)
(require 'semantic/debug)
;; if you want to enable support for gnu global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
;; as per http://www.randomsample.de/cedetdocs/cedet/CScope.html
(semanticdb-enable-cscope-databases)

;;(semantic-add-system-include
;;  "/usr/include/QtCore" 'c++-mode)
;;(add-to-list 'semantic-lex-c-preprocessor-symbol-map
;;  '("Q_CORE_EXPORT" . ""))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;;(semantic-load-enable-primary-exuberent-ctags-support)

;;(add-to-list global-semanticdb-minor-mode semantic-default-submodes)
;; if you want to enable support for gnu global
;;(when (cedet-gnu-global-version-check t)
;;  (semanticdb-enable-gnu-global-databases 'c-mode)
;;  (semanticdb-enable-gnu-global-databases 'c++-mode)
;;  )

;;Get CEDET's configuration for work with Qt4:
;;(setq qt4-base-dir "/usr/include/qt4")
;;(semantic-add-system-include qt4-base-dir 'c++-mode)
;;(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
;;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
;;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
;;(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

;; cedet-m3 - for a right mouse context menu for code jumps,
;; doesn't seem to ship with newer cedet anymore. So I downloaded
;; an old copy from github.
;;(add-to-list 'load-path
;;             "/home/fanner/EmacsIDE/cedet-extra")
;;(require 'cedet-m3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;code completion using auto-complete and ac-python
;;Following http://utkarshsengar.com/2011/06/emacs-python/
;;Largely based on getting EVERYTHING via emacs-for-python.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load-file "~/EmacsIDE/emacs-for-python/epy-init.el")
;; This is needed to stop  garbage instead of a nicely coloured
;; prompt, as per http://ipython.org/ipython-doc/rel-1.0.0/config/editors.html
;; (setq ansi-color-for-comint-mode t)
;; (epy-setup-ipython)

;;Disable some autocompletion bits introduced by emacs-for-python
;;which annoyingly inserts mathching quotes and brackets
(setq skeleton-pair nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Colour themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacsecb.d/color-theme-6.6.0")
(require 'color-theme)

;;dark colour scheme using solarized
;;Following http://utkarshsengar.com/2011/06/emacs-python/
;;(eval-after-load "color-theme"
;;  '(progn
;;     (color-theme-initialize)
;;   (color-theme-hober)))
(add-to-list 'load-path "~/.emacsecb.d/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(eval-after-load "color-theme-solarized"
  '(progn (color-theme-solarized-dark)))

;; Ryan's colour theme
;;(add-to-list 'load-path "~/.emacsecb.d/ryan-color-theme")
;;(load-library "ryan-color-theme")
;;(color-theme-initialize)
;;(global-font-lock-mode 1)
;;(color-theme-ryan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Mode hooks for using different jump-to-definition and jump-to-file
;;settings for various code modes
;; TODO - look at semantic vs cscope or semantic + cscope
;; Jump to definition

;; Get rid of newline binding
;; (global-unset-key (kbd "C-j"))
;; Use cscope by default for all programming modes
(eval-after-load 'prog-mode-hook (lambda ()
                                   (local-set-key (kbd "C-j") 'cscope-find-this-symbol)))

;; Specialise for Python, because elpy does a better job than cscope
;; TODO - I want C-J. At present I need to use M-. to get elpy's goto
;; definition

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Sanitise files before saving
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Keyboard shortcuts
(global-set-key "\C-g" 'goto-line)
(global-whitespace-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Editing basics
(delete-selection-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Katana development support
(require 'magit)
(setq magit-repo-dirs '("/workspace/fanner/katana2.0/Apps/Katana/"
                        "/workspace/fanner/katana2.0/Apps/Geolib3/"))

(defun katana-git-fetch ()
  (interactive)
  (call-process "/workspace/fanner/katana2.0/gitfetch.sh")
  )

(defun katana-git-merge ()
  (interactive)
  (call-process "/workspace/fanner/katana2.0/gitmerge.sh")
  )

(defun katana-review ()
  (interactive)
  (let ((review-branch-name "review_2_0")))
  (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
  (magit-fetch "origin")
  (magit-checkout review-branch-name)
  (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
  )

(defun katana-review-close ()
  (interactive)
  (setq review-branch-name "review_2_0")
  (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
  (setq line1 (car (split-string (buffer-string) "\n" t)))
  (setq branch-name (nth 1 (split-string line1)))
  (if (not (string= branch-name review-branch-name))
      (error "not on review branch")
    )
  (magit-merge "origin/KATANA_17A_BRANCH")
  (magit-checkout "KATANA_17A_BRANCH")
  (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
  )

(defun geolib-review ()
  (interactive)
  (let ((review-branch-name "review_2_0")))
  (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
  (magit-fetch "origin")
  (magit-checkout review-branch-name)
  (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
  )

(defun geolib-review-close ()
  (interactive)
  (setq review-branch-name "review_2_0")
  (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
  (setq line1 (car (split-string (buffer-string) "\n" t)))
  (setq branch-name (nth 1 (split-string line1)))
  (if (not (string= branch-name review-branch-name))
      (error "not on review branch")
    )
  (magit-merge "origin/GEOLIB3_41A_BRANCH")
  (magit-checkout "GEOLIB3_41A_BRANCH")
  (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
  )

  
(defun magit-status-geolib ()
  (interactive)
  (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3/")
  (magit-log-ranged "GEOLIB3_35A_BRANCH..origin/GEOLIB3_35A_BRANCH")
  )

(defun magit-status-katana ()
  (interactive)
  (magit-status "/workspace/fanner/katana2.0/Apps/Katana/")
  (magit-log-ranged "KATANA_17A_40A_BRANCH..origin/KATANA_17A_40A_BRANCH")
  )

(defun run-katana ()
  (interactive)
  (start-process "katana2.0" "katana-output" "/workspace/fanner/katana2.0/runkatana.sh")
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GUD/GDB for Katana 2.0
(defun debug-katana ()
  (interactive)
  (setq gdb-many-windows t)
  (gdb "gdb --command /workspace/fanner/katana2.0/Apps/Katana/docs_spi/gdb_katana --fullname /workspace/fanner/katana2.0/Apps/Katana/objects/linux-64-x86-debug-410-gcc/Dist/bin/katanaBin")
  (setq gdb-many-windows t)
  (gdb-restore-windows)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Easy window and buffer switching

;; Switch windows
;; see windmove here http://ergoemacs.org/emacs/emacs_winner_mode.html
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-up>") 'windmove-up)

;; Swap buffers
;; http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs
;; and http://www.emacswiki.org/emacs/TransposeFrame
(add-to-list 'load-path
             "~/.emacsecb.d/transpose-frame")
(require 'transpose-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Stuff emacs saves when using it's customisation menus.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-offsets-alist (quote ((statement-block-intro . +) (substatement . 0) (substatement-open . 0) (substatement-label . +) (case-label . +))))
 '(ecb-excluded-directories-regexps (quote ("^\\(CVS\\|\\.[^xX]*\\|\\.git\\|objects\\)$")))
 '(ecb-layout-name "leftright2")
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote never))
 '(ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote ((#("/" 0 1 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) #("/" 0 1 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))))))
 '(ecb-tip-of-the-day nil)
 '(ecb-vc-directory-exclude-regexps (quote ("Thirdparty")))
 '(ecb-vc-enable-support t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(python-indent-offset 4)
 '(scroll-conservatively 30)
 '(semantic-lex-spp-use-headers-flag t)
 '(solarized-contrast (quote normal))
 '(tool-bar-mode nil)
 '(whitespace-global-modes t)
 '(whitespace-style (quote (tabs trailing space-before-tab indentation tab-mark trailing space-after-tab space-mark tab-mark lines))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-space ((((class color) (background dark)) (:stipple nil :background "grey20" :foreground "aquamarine3")))))
