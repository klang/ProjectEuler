(defwrk clj-current "problems currently being worked on"
  "~/projects/ProjectEuler/project.org"
  "~/projects/ProjectEuler/src/all.clj"
  "~/projects/ProjectEuler/user.clj"
  "~/projects/ProjectEuler/src/problem110.clj"
  "~/projects/ProjectEuler/src/problem108.clj"
)

(defwrk project-euler-stuck "stuck problems"
  "~/projects/ProjectEuler/src/tools.clj"
  ;"~/projects/ProjectEuler/src/problem282.clj"
  "~/projects/ProjectEuler/src/problem216.clj"
  "~/projects/ProjectEuler/src/problem191.clj"
  "~/projects/ProjectEuler/src/problem145.clj"
  "~/projects/ProjectEuler/src/problem066.clj"
  "~/projects/ProjectEuler/src/problem068.clj"
  "~/projects/ProjectEuler/src/problem077.clj"
)
(defwrk project-euler-parked "parked problems"
  "~/projects/ProjectEuler/src/problem196.clj"
  "~/projects/ProjectEuler/src/problem178.clj"
  "~/projects/ProjectEuler/src/problem211.clj"
  "~/projects/ProjectEuler/src/problem086.clj"
  "~/projects/ProjectEuler/src/problem275.clj"
  "~/projects/ProjectEuler/src/problem277.clj"
  "~/projects/ProjectEuler/src/problem284.clj"
)

(defun project-euler nil
  "Start Project Euler"
  (interactive)
  (setq default-directory "~/projects/ProjectEuler/")
  (clj-current))

