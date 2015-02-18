;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<global code mode preferences>

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
(require 'dtrt-indent)
(dtrt-indent-mode 1)
;;</global code mode preferences>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<style definitions>

;; Generated from ConstraintResolver.cpp using
;; M-x c-set-style linux
;; M-x c-guess-buffer
;; M-x c-guess-view
(defconst katana-c-style
  '((c-offsets-alist
     (access-label . 0)      ; Guessed value
     (arglist-cont . 0)      ; Guessed value
     (arglist-intro . +)     ; Guessed value
     (block-close . 0)       ; Guessed value
     (catch-clause . 0)      ; Guessed value
     (class-close . 0)       ; Guessed value
     (class-open . 0)        ; Guessed value
     (defun-block-intro . +) ; Guessed value
     (defun-close . 0)       ; Guessed value
     (defun-open . 0)        ; Guessed value
     (else-clause . 0)       ; Guessed value
     (inclass . +)           ; Guessed value
     (inline-close . 0)      ; Guessed value
     (innamespace . 0)       ; Guessed value
     (member-init-intro . +) ; Guessed value
     (namespace-close . 0)   ; Guessed value
     (namespace-open . 0)    ; Guessed value
     (statement . 0)             ; Guessed value
     (statement-block-intro . +) ; Guessed value
     (statement-cont . +)        ; Guessed value
     (substatement-open . 0) ; Guessed value
     (topmost-intro . 0)     ; Guessed value
     (topmost-intro-cont . +) ; Guessed value
     (arglist-close . c-lineup-close-paren)
     (arglist-cont-nonempty . c-lineup-arglist)
     (c . c-lineup-C-comments)
     (comment-intro . c-lineup-comment)
     (cpp-macro . -1000)
     (inher-cont . c-lineup-multi-inher)
     (string . -1000)))
  "C Programming Style of Katana Team")

(c-add-style "katana" katana-c-style)

(defun my-c-mode-common-hook ()
       ;; my customizations for all of c-mode and related modes
       (c-set-style "katana")
       )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
