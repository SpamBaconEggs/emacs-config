;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<cedet ede>
;;cedet is massive. It provides various tools useful for building up an
;;IDE. E.g. EDE, for setting up directories and files that are part projects
;;(used heavily by ECB), semantic (for getting tags/symbols from code,
;;jumping to definitions - although mileage here is very variable).
;;This is one approach to get emacs to be more like a fully-fledged
;;modern IDE. The other is projectile (haven't added it to the config yet).
;;See for example,
;;http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;and http://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2/12716443#12716443
;;and http://altom.ro/blog/emacs-python-ide-recipe


;; lets ede use cscope for finding source files in projects
(setq ede-locate-setup-options
      '(ede-locate-cscope
        ede-locate-base))


;;See the notes in cedet-cscope-tips.md on getting case-insensitive
;;cscope searching to work.
(defun my-find-file-in-project (filesubstring)
  "EDE's file finding is a bit pants. It doesn't make use of previously
   hashed/searched results correctly, and then doesn't give you the option
   to choose between multiple similarly named files (just picks the first).
   So this is an attempt to do a bit better by using cedet-cscope and ido
   directly."
  ;;todo - make this independent of cedet, so we only rely on xcscope?
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

;;Using cscope for searching for symbols works reasonably well on my
;;C/C++ projects.  Semantic has been less useful, as it often doesn't
;;find the tags (perhaps I didn't set it up correctly).

;;Following http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;and http://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2/12716443#12716443

;;Program to parse and build tags for browsing (non-LISP, so fast)
(mapc (lambda (MODE) (add-to-list 'semantic-default-submodes MODE))
      '(global-semantic-mru-bookmark-mode
        global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        ;; name of current tag will be shown in top line of buffer
        global-semantic-stickyfunc-mode
        ;; activates CEDET's context menu that is bound to right mouse button
        ;; doesn't work - no such function: global-cedet-m3-minor-mode
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
(require 'semantic/symref)
;; if you want to enable support for gnu global
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
;; as per http://www.randomsample.de/cedetdocs/cedet/CScope.html
;;(semanticdb-enable-cscope-databases)
;; does not work - no such function: (semanticdb-enable-cscope-databases)

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
;;</cedet ede>
