(defwrk project-euler-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/problem078.clj"
)
(defwrk project-euler-stuck "stuck problems"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/problem179.clj"
  "~/projects/ProjectEuler/src/problem023.clj"
  "~/projects/ProjectEuler/src/problem216.clj"
  "~/projects/ProjectEuler/src/problem191.clj"
  ;"~/projects/ProjectEuler/src/problem282.clj"
  "~/projects/ProjectEuler/src/problem145.clj"
  "~/projects/ProjectEuler/src/problem015.clj"
  "~/projects/ProjectEuler/src/problem033.clj"
  "~/projects/ProjectEuler/src/problem032.clj"
  "~/projects/ProjectEuler/src/problem038.clj"
  "~/projects/ProjectEuler/src/problem043.clj"
  "~/projects/ProjectEuler/src/problem044.clj"
  "~/projects/ProjectEuler/src/problem046.clj"
  "~/projects/ProjectEuler/src/problem050.clj"
  "~/projects/ProjectEuler/src/problem074.clj"
  "~/projects/ProjectEuler/src/problem075.clj"
)

(defun project-euler nil
  "Start Project Euler"
  (interactive)
  (setq default-directory "~/projects/ProjectEuler/")
  (load "~/projects/gists/gist-337280/clojure-font-lock-setup.el")
  (project-euler-current)
;  (swank-clojure-project default-directory)
  )

