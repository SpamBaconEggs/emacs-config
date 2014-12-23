;;I'm using ECB as a speedbar-on-steroids in both my cedet-flavoured
;;setup and my projectile-flavoured setup.  The main reason for doing
;;so is that it supports project-based browsing better than speedbar,
;;which knows nothing of projects, and always just shows the child
;;directory tree. It also doesn't show tags/symbols out of the box,
;;which ECB will do.

(require 'cedet)
(require 'cedet-cscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ede>
;; ede - project management which is part of cedet
;;
;; ecb uses ede's project facilities to populate its sidebars with
;; relevant directory structures.

(global-ede-mode 1)

;; emacs config project
(if (file-exists-p "/home/fanner/code/emacs-config/init.el")
    (ede-cpp-root-project "katana2.0" :file "/home/fanner/code/emacs-config/init.el"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )

;; katana 2.0 project
(if (file-exists-p "/workspace/fanner/katana2.0/SConstruct")
    (ede-cpp-root-project "katana2.0" :file "/workspace/fanner/katana2.0/SConstruct"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )


;; katana 1.6 project
(if (file-exists-p "/workspace/fanner/katana1.6/SConstruct")
    (ede-cpp-root-project "katana2.0" :file "/workspace/fanner/katana1.6/SConstruct"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )

;; tputils project
(if (file-exists-p "/workspace/fanner/tputils/README")
    (ede-cpp-root-project "katana2.0" :file "/workspace/fanner/tputils/README"
                          )
  (message "skipping over an ede project because project dir does not exist")
  (warn "not on review branch")
  )

;; emacs ecb project
(if (file-exists-p "/home/fanner/.emacsecb.d/init.el")
    (ede-cpp-root-project "katana2.0" :file "/home/fanner/.emacsecb.d/init.el"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )


;; sprint planner project
(if (file-exists-p "/workspace/fanner/katana_sprint_planner/KatanaSprintPlanner.py")
    (ede-cpp-root-project "katana2.0" :file "/workspace/fanner/katana_sprint_planner/KatanaSprintPlanner.py"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )

;; katana qa resources
(if (file-exists-p "/workspace/Katana/QA_Resources/finder.sh")
    (ede-cpp-root-project "katana2.0" :file "/workspace/Katana/QA_Resources/finder.sh"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )

;; test harness for foundry/katana
(if (file-exists-p "/workspace/Katana/TestHarness/finder.sh")
    (ede-cpp-root-project "test_harness" :file "/workspace/Katana/TestHarness/finder.sh"
                          )
  (message "skipping over an ede project because project dir does not exist")
  )

;;</ede>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;<ecb>
;; ecb - emacs code browser (provides a UI layer on top of cedet and ede)
;; ecb gives you side bars for navigating directories, files and symbols
(require 'ecb)

;; Following the ecb README from the source directory
;; Switched off if you want to speed-up startup
;; For now, you need to run M-x ecb-activate when you want ECB to be
;; active during a session.
;;(require 'ecb-autoloads)

(setq stack-trace-on-error nil) ;;don’t popup Backtrace window
(setq ecb-tip-of-the-day nil)
;;(setq ecb-auto-activate t)
;;(setq ecb-layout-name "left6")
;;(setq ecb-options-version "2.40")
;;(setq ecb-primary-secondary-mouse-buttons (quote mouse-1–mouse-2))
;;(setq ecb-source-path (quote ("~/")))
;;</ecb>

