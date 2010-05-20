(defwrk project-euler-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/tools/parts.clj"
  "~/projects/ProjectEuler/src/problem196.clj"
  "~/projects/ProjectEuler/src/problem178.clj"
  "~/projects/ProjectEuler/src/problem032.clj"
  "~/projects/ProjectEuler/src/problem214.clj"
)

(defwrk project-euler-stuck "stuck problems"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/problem216.clj"
  "~/projects/ProjectEuler/src/problem191.clj"
  ;"~/projects/ProjectEuler/src/problem282.clj"
  "~/projects/ProjectEuler/src/problem145.clj"
  "~/projects/ProjectEuler/src/problem051.clj"
  "~/projects/ProjectEuler/src/problem054.clj"
  "~/projects/ProjectEuler/src/problem060.clj"
  "~/projects/ProjectEuler/src/problem061.clj"
  "~/projects/ProjectEuler/src/problem066.clj"
  "~/projects/ProjectEuler/src/problem068.clj"
  "~/projects/ProjectEuler/src/problem072.clj"
  "~/projects/ProjectEuler/src/problem073.clj"
  "~/projects/ProjectEuler/src/problem074.clj"
  "~/projects/ProjectEuler/src/problem075.clj"
  "~/projects/ProjectEuler/src/problem077.clj"
)

(defun project-euler nil
  "Start Project Euler"
  (interactive)
  (setq default-directory "~/projects/ProjectEuler/")
  (load "~/projects/gists/gist-337280/clojure-font-lock-setup.el")
  (project-euler-current)
  (add-hook 'slime-connected-hook 'slime-redirect-inferior-output)
;  (swank-clojure-project default-directory)
  )

