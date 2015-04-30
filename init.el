;; RMF's file.

;; see http://ergoemacs.org/emacs/emacs_custom_system.html
;; for tips on moving custom-set variables from .emacs files into your own
;; emacs config
;; (customize-set-variable auto-save-default nil)

;;Set to t to use CEDET & ECB. Set to 0 to use Projectile.
(setq use-cedet-ide nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<package manager>
;; Setting up package manager bits, as per http://www.logilab.org/173886

;; this is intended for manually installed libraries
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; load the package system and add some repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize and require non-ELPA packages, etc.

;; disable automatic loading of packages after init.el is done
(setq package-enable-at-startup nil)
;; and force it to happen now
(package-initialize)
;; now you can (require) your ELPA packages and configure them as normal

;;</package manager>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<navigation>

;; Go to specified line number
(global-set-key "\M-g" 'goto-line)

;;Easy window and buffer switching

;; Switch windows
;; see windmove here http://ergoemacs.org/emacs/emacs_winner_mode.html
(require 'windmove)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-up>") 'windmove-up)

;; Swap buffers
;; see http://stackoverflow.com/questions/1774832/how-to-swap-the-buffers-in-2-windows-emacs
;; and http://www.emacswiki.org/emacs/TransposeFrame
(add-to-list 'load-path
             "~/.emacs.d/extra")
(require 'transpose-frame)
;;</navigation>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<editing basics>

;;Delete selection when calling yank/pasting (default is to annoyingly
;;unselect and then yank inserts instead of overwrites)
(delete-selection-mode 1)

;;Line numbers
(require 'linum)
(setq linum-mode t)

;; From http://stackoverflow.com/a/3669681/601626
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;;</editing basics>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<misc keybindings>
;; I find it annoying that C-z will minimize emacs.
;; It's still bound to C-x C-z, so this feature is still
;; accessible.
(global-unset-key (kbd "C-z"))

;;</misc keybindings>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ido>
;; ido - nice quick buffer option completion
(if nil ;; t ;; use-cedet-ide
    (progn
      (message "loading ido - rmf")
      (require 'ido)
      (ido-mode t)
      (ido-everywhere t)
      (setq ido-use-faces nil)
      ;; enable fuzzy matching via flx
      ;; see https://github.com/bbatsov/projectile
      ;; and https://github.com/lewang/flx
      (require 'flx)
      (require 'flx-ido)
      (flx-ido-mode 1)
      ;; disable ido faces to see flx highlights.
      (setq ido-enable-flex-matching t)
      ;; disable flx's highlights
      (setq flx-ido-use-faces nil))
  )

;; ido-ubiquitous - Does what you were really hoping for when you did
;;(require 'ido-ubiquitous)
;;(setq ido-ubiquitous-mode t)
;;</ido>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<cscope tags>
;;xscope.el for source code browsing, jumping to definitions, etc.
;;see http://stackoverflow.com/a/12923994/601626
;;and https://github.com/dkogan/xcscope.el

(require 'xcscope)
(cscope-setup)

(if use-cedet-ide
    ;; keybinding for symbol finding
    (global-set-key (kbd "M-.") 'cscope-find-this-symbol)
  )
;;</cscope tags>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ggtags>
;; ggtags - for GNU Global tags and Exuberant C Tags (depending on how GNU GLOBAL was compiled)
(require 'ggtags)
;;   enable ggtags for c modes
(add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))
;;(require 'ctags)
;;</ggtags>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<gtags>
(require 'gtags)
;;</gtags>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<python>
;; Elpy for Python goodies
;; See: https://github.com/jorgenschaefer/elpy/wiki/Installation

;; First to do this on command line: pip install elpy jedi
(require 'elpy)
(package-initialize)
(elpy-enable)
(setq python-indent-offset 4)

;; Rope (the default code completer for elpy) is abominably slow.
;; Use Jedi instead. (WIP)
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)

;; Very rudimentary IPython
;; http://emacswiki.org/emacs/PythonProgrammingInEmacs - section for Emacs 24
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;</python>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<code mode preferences>
(load "~/code/emacs-config/programming_styles.el")

;;</code mode preferences>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<grep behaviour>

(require 'grep)
;; Use this to hardcode it:
(customize-set-variable
 'grep-find-ignored-directories
 (list
  ;; defaults
  "SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"
  ;; Foundry build directories
  "objects" "build" "bin" "out" "lib"))
;;</grep behaviour>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<whitespace handling>
(require 'whitespace)
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)
(customize-set-variable
 'whitespace-style (quote
                    (face tabs spaces trailing lines-tail
                     space-before-tab newline
                     indentation empty space-after-tab space-mark
                     tab-mark newline-mark)))

(global-whitespace-mode 1)
;;Use ws-trim module to cleanup whitepsace issues, as it's more flexible
;;with regard to where the cleanup will be applied. Specifically, you can
;;make it cleanup only modified lines in a buffer, which is good when
;;working on code/diffs.
(require 'ws-trim)
(global-ws-trim-mode t)
;;</whitespace handling>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<code autocompletion>
(require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.emacsecb.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
;;Autocompletion for C/C++ using clang
;;see http://truongtx.me/2013/03/06/emacs-ccpp-autocomplete-with-clang/
;;Requires you to install clang before using it (i.e. running
;; clang --version
;;on a bash shell should produce something sensible)
(require 'auto-complete-clang)
;; (require 'auto-complete-clang-async) is another module that could be checked
;;Seems to hook up to semantic automatically, so C/C++ autocompletion
;;works at this point.
(require 'ac-python)
(require 'ac-etags)
;;</code autocompletion>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ecb>
;;
;;Use ECB for both cedet and projectile-based emacs
;;configurations below. ECB provides a source code browser
;;(directories, files, tags/symbols and history) that is superior to
;;speedbar, but which some people say is less well supported :'(

(load "~/code/emacs-config/ecb_init.el")
;;</ecb>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<cedet>
;;CEDET is a collection of tools aimed at providing IDE-like
;;functionality.  ECB is essentially some UI layered on top of
;;CEDET, providing sidebar buffers for directory, file and
;;tag/symbol browsing.

(if use-cedet-ide
    (load "~/code/emacs-config/cedet_init.el")
  )
;;</cedet>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<projectile>
(if (not use-cedet-ide)
    (load "~/code/emacs-config/projectile_init.el")
  )
;;</projectile>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<git>
;;Configure magit and some home-rolled functionality to help me fetch
;;and review code from various git repositories.

(require 'magit)

;; Note to self: To stage individual hunks, you need to use TAB to
;; toggle from the Magit status buffer to the file you want to do
;; hunks for. Trying to do this from magit-log buffers does nothing
;; - you need to be in the status buffer!

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

;; TODO: The code-review hack could potentially be a nice
;; addition to magit. Something like
;; magit-review-init
;; -- makes a branch like magit_review_KATANA_17A_BRANCH
;; -- which tracks origin/KATANA_17A_BRANCH
;; Thereafter, you can do
;; magit-review (select review branch)
;; and
;; magit-review-close (select the commit you were at)

;; TODO: Adjust so it ties into projectile, so I don't have
;; to worry about paths.
;; TODO: Make it work generically on any projects
;;
(defun katana-review ()
  (interactive)
  (let ((review-branch-name "review_2_0"))
    (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
    (magit-fetch "origin")
    (magit-checkout review-branch-name)
    (magit-status "/workspace/fanner/katana2.0/Apps/Katana")))

(defun katana-review-close (commit)
;;  (interactive "sEnter last commit reviewed: ")
  (interactive (list
                (read-string (format "sEnter last commit reviewed (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (let ((review-branch-name "review_2_0")
        (line1 "")
        (branch-name ""))
    (magit-checkout review-branch-name)
    (magit-status "/workspace/fanner/katana2.0/Apps/Katana")
    (setq line1 (car (split-string (buffer-string) "\n" t)))
    (setq branch-name (nth 1 (split-string line1)))
    (if (not (string= branch-name review-branch-name))
        (error "not on review branch")
      )
    (magit-merge commit)
    (magit-checkout "KATANA_17A_BRANCH")
    (magit-status "/workspace/fanner/katana2.0/Apps/Katana")))

(defun geolib-review ()
  (interactive)
  (let ((review-branch-name "review_2_0"))
    (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
    (magit-fetch "origin")
    (magit-checkout review-branch-name)
    (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
    ))

(defun geolib-review-close (commit)
;;  (interactive "sEnter last commit reviewed: ")
  (interactive (list
                (read-string (format "sEnter last commit reviewed (%s): " (thing-at-point 'word))
                             nil nil (thing-at-point 'word))))
  (let ((review-branch-name "review_2_0")
        (line1 "")
        (branch-name ""))
    (magit-checkout review-branch-name)
    (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")
    (setq line1 (car (split-string (buffer-string) "\n" t)))
    (setq branch-name (nth 1 (split-string line1)))
    (if (not (string= branch-name review-branch-name))
        (error "not on review branch")
      )
    (magit-merge commit)
    (magit-checkout "GEOLIB3_41A_BRANCH")
    (magit-status "/workspace/fanner/katana2.0/Apps/Geolib3")))

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
;;<p4>
;;Configure perforce bits
(require 'p4)
;;todo: copy list of client names from .emacs into here.
;; </p4>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<gud gdb>
;;GUD/GDB for Katana 2.0
(defun debug-katana ()
  (interactive)
  (setq gdb-many-windows t)
  (gdb "gdb --command /workspace/fanner/katana2.0/Apps/Katana/docs_spi/gdb_katana --fullname /workspace/fanner/katana2.0/Apps/Katana/objects/linux-64-x86-debug-410-gcc/Dist/bin/katanaBin")
  (setq gdb-many-windows t)
  (gdb-restore-windows)
  )
;;</gud gdb>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<markdown mode>
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
;;</markdown mode>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<colour theme>
(require 'color-theme)
(require 'color-theme-solarized)
(setq color-theme-is-global t)
;; see https://github.com/sellout/emacs-color-theme-solarized/blob/master/README.md
;; see http://stackoverflow.com/a/17038372
(add-hook 'after-init-hook (lambda ()
                             (progn
                                (color-theme-initialize)
                                (color-theme-solarized-dark))))
;;</colour theme>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "done loading my init.el")
