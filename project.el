(defwrk project-euler-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/tools/parts.clj"
  "~/projects/ProjectEuler/src/problem196.clj"
  "~/projects/ProjectEuler/src/problem178.clj"
  "~/projects/ProjectEuler/src/problem211.clj"
  "~/projects/ProjectEuler/src/problem086.clj"
)

(defwrk project-euler-stuck "stuck problems"
  "~/projects/ProjectEuler/src/tools.clj"
  ;"~/projects/ProjectEuler/src/problem282.clj"
  "~/projects/ProjectEuler/src/problem216.clj"
  "~/projects/ProjectEuler/src/problem191.clj"
  "~/projects/ProjectEuler/src/problem145.clj"
  "~/projects/ProjectEuler/src/problem061.clj"
  "~/projects/ProjectEuler/src/problem066.clj"
  "~/projects/ProjectEuler/src/problem068.clj"
  "~/projects/ProjectEuler/src/problem077.clj"
)

(defun project-euler nil
  "Start Project Euler"
  (interactive)
  (setq default-directory "~/projects/ProjectEuler/")
  (load "~/projects/gists/421306/clojure-font-lock-setup.el")
;;  (load "~/projects/gists/407276/colored-clojure-slime-repl.el")
  (project-euler-current)
  (add-hook 'slime-connected-hook 'slime-redirect-inferior-output)
;  (swank-clojure-project default-directory)
  )

