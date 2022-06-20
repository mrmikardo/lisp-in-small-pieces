(ns user
  "Start Clerk notebook server."
  (:require [nextjournal.clerk :as clerk]))

;; Start Clerk's buit-in webserver on the default port
(clerk/serve! {:browse? true})

;; Tell Clerk to watch the given `:paths` for changes
(clerk/serve! {:watch-paths ["src"]})

(clerk/show! "src/ch_01.clj")
