
(chgbg 'r)
(setq frame-title-format "Dibrawi – %b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation:

(setq omake-command
      (if (eq system-type 'darwin) 
          "/Users/seb/usr/godi311/bin//omake --project"
        "omake --project"))

(set-mk-command omake-command)
(setq compile-command omake-command)
(setq compilation-read-command nil)

;; Remove usual call to eshell:
(seb-funkey "mk" save-all-and-mk
            (progn (save-some-buffers) (compile omake-command)))
