Enable case-insensitive matching for cscope
===========================================

To get case-insensitive matching to work via cedet's built-in
cscope, you need to download the emacs lisp source files, and
then patch 'cedet-cscope.el', and then byte-compile-file it.

    # bash
    sudo apt-get install emacs24-el
    cd /usr/share/emacs/24.3/lisp/cedet/
    emacs cedet-cscope.el


Then apply this patch to the cedet-cscope.el:

    ;; elisp

    (defun cedet-cscope-call (flags)
      "Call CScope with the list of FLAGS."
      (let ((b (get-buffer-create "*CEDET CScope*"))
    	  (cd default-directory)
    	  )
        (with-current-buffer b
          (setq default-directory cd)
          (erase-buffer))
    +    ;; make all matches case-insensitive
    +    (add-to-list 'flags "-C")
        (apply 'call-process cedet-cscope-command
    	   nil b nil
    	   flags)
        b))


Then save and do `M-x byte-compile-file`.
