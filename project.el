(defwrk clj-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/tools.clj"
  "~/projects/ProjectEuler/src/tools/parts.clj"
  "~/projects/ProjectEuler/src/problem196.clj"
  "~/projects/ProjectEuler/src/problem178.clj"
  "~/projects/ProjectEuler/src/problem211.clj"
  "~/projects/ProjectEuler/src/problem086.clj"
  "~/projects/ProjectEuler/src/problem275.clj"
  "~/projects/ProjectEuler/src/problem277.clj"
  "~/projects/ProjectEuler/src/problem284.clj"
  "~/projects/ProjectEuler/src/problem014.clj"
  "~/projects/ProjectEuler/user.clj"
  "~/projects/ProjectEuler/src/all.clj"
  "~/projects/ProjectEuler/user.clj"
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
  (clj-current))

