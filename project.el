(defwrk project-euler-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/problem078.clj"
  "~/projects/ProjectEuler/src/problem031.clj"
  "~/projects/ProjectEuler/src/tools/parts.clj")

(defwrk project-euler-stuck "stuck problems"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/problem216.clj"
  "~/projects/ProjectEuler/src/problem191.clj"
  ;"~/projects/ProjectEuler/src/problem282.clj"
  "~/projects/ProjectEuler/src/problem145.clj"
  "~/projects/ProjectEuler/src/problem033.clj"
  "~/projects/ProjectEuler/src/problem032.clj"
  "~/projects/ProjectEuler/src/problem038.clj"
  "~/projects/ProjectEuler/src/problem043.clj"
  "~/projects/ProjectEuler/src/problem044.clj"
  "~/projects/ProjectEuler/src/problem046.clj"
  "~/projects/ProjectEuler/src/problem050.clj"
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
  "~/projects/ProjectEuler/src/problem079.clj"
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

