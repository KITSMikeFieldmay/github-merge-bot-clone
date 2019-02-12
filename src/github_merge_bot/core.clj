(ns github-merge-bot.core
  (:require [tentacles.pulls :as pulls]
            [tentacles.core :as tentacles]
            [tentacles.issues :as issues]
            [clj-jgit.porcelain :as git])
  (:import (java.util Timer TimerTask)
           (org.eclipse.jgit.transport UsernamePasswordCredentialsProvider RefSpec)
           (org.eclipse.jgit.revwalk RevWalk)
           (org.eclipse.jgit.revwalk.filter RevFilter)
           (org.eclipse.jgit.lib ObjectId)
           (java.io FileNotFoundException)
           (org.eclipse.jgit.api Git RebaseResult$Status RebaseCommand$Operation))
  (:gen-class))

(def READY_FOR_BOT "readyForBot")
(def REJECT_LABEL "rejectedFromQueue")

;TODO add other messages
(def IN_PROGRESS_MSG "Required status check \"workflow\" is in progress.")
(def HOLD_REJECT_MESSAGES (list IN_PROGRESS_MSG))


(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn github-reviews [owner repo pr-id & [options]]
  (tentacles/api-call :get "repos/%s/%s/pulls/%s/reviews" [owner repo pr-id] options))

(defn github-create-review [owner repo pr-id & [options]]
  (tentacles/api-call :post "repos/%s/%s/pulls/%s/reviews" [owner repo pr-id] options))

(defn approved? [owner repo pull-request]
  (let [reviews (github-reviews owner repo (:number pull-request))
        reviews-by-author (vals (group-by #(-> % :user :id) reviews))
        decisive-reviews (map (fn [reviews]
                                (->> reviews (map :state) (filter #{"CHANGES_REQUESTED" "APPROVED"}) last))
                              reviews-by-author)]
    (and (< 0 (count (filter #{"APPROVED"} decisive-reviews)))
         (not-any? #{"CHANGES_REQUESTED"} decisive-reviews))))

(defn has-label? [pull-request]
  (let [labels (set (map :name (:labels pull-request)))]
    (and (contains? labels READY_FOR_BOT)
         (not (contains? labels REJECT_LABEL)))))

(defn merge-candidate [owner repo pull-requests]
  (last (filter #(has-label? %)
                (sort-by :created-at pull-requests))))

(defn procure-repo
  ([owner repo-name]
   (procure-repo owner repo-name (str "./tmp/" owner "/" repo-name)))
  ([owner repo-name directory]
   (try
     (git/load-repo directory)
     (catch FileNotFoundException _
       (println "Repo not found locally, cloning...")
       (:repo (git/git-clone-full (str "https://github.com/" owner "/" repo-name ".git")
                                  directory))))))

(defn head-up-to-date-with-base? [owner repo pull-request]
  (let [repo (procure-repo owner repo)]
    (git/git-fetch repo "origin")
    (let [rev-walk (RevWalk. (.getRepository repo))
          master (.parseCommit rev-walk (.getObjectId (.findRef (.getRepository repo) "origin/master")))
          _ (.reset rev-walk)
          base (.parseCommit rev-walk (ObjectId/fromString (:sha (:head pull-request))))
          _ (.reset rev-walk)
          merge-base (-> (doto rev-walk (.setRevFilter (RevFilter/MERGE_BASE))
                                        (.markStart [master base]))
                         (.next))]
      (= (.getName merge-base)
         (.getName master)))))


(defn reject-from-queue [owner repo pull-request credentials result-status]
  "Check if reject label should be added to remove PR form the queue and stop blocking it"
  (println (str "Checking if should reject " (:number pull-request) ", status: "result-status))
  (if (in? HOLD_REJECT_MESSAGES (str result-status))
    (println (str "IN PROGRESS waiting for " (:number pull-request)))
    (tentacles/with-defaults
     {:auth (str (:username credentials) ":" (:password credentials))}
     (git/with-credentials (:username credentials) (:password credentials)
                           (issues/add-labels owner repo (:number pull-request) {:labels [REJECT_LABEL]} {})))))


(defn update-pull-request [owner repo-name pull-request credentials]
  (println (str "Updating pull request #" (:number pull-request) " by rebasing its head branch on master..."))
  (let [^Git repo (procure-repo owner repo-name)
        head (:sha (:head pull-request))
        approved (approved? owner repo-name pull-request)]
    (git/git-fetch repo "origin")
    (git/git-checkout repo head)
    (let [rebase-result (-> repo .rebase (.setUpstream "origin/master") .call)]
      (if-not (= RebaseResult$Status/OK (.getStatus rebase-result))
        (do (println (str "Unable to rebase pull request #" (:number pull-request) ": rebase result status: " (.getStatus rebase-result)))
            (reject-from-queue owner repo-name pull-request credentials (.getStatus rebase-result))
            (-> repo .rebase (.setOperation RebaseCommand$Operation/ABORT) .call))
        (let [new-head (-> repo .getRepository (.findRef "HEAD") .getObjectId .getName)]
          (-> repo
              (.push)
              (.setRemote "origin")
              (.setRefSpecs [(RefSpec. (str "HEAD:refs/heads/" (:ref (:head pull-request))))])
              (.setForce true)
              (.setCredentialsProvider (UsernamePasswordCredentialsProvider. (:username credentials) (:password credentials)))
              (.call))
          (if approved
            (do (println (str "Re-approving pull request #" (:number pull-request) " after updating..."))
                (github-create-review owner
                                      repo-name
                                      (:number pull-request)
                                      {:commit-id new-head
                                       :body      "Automatically re-approving after updating this pull request."
                                       :event     "APPROVE"})))))))
  nil)



(defn try-merge-pull-request [owner repo pull-request credentials]
  (println (str "Trying to merge pull request #" (:number pull-request) "..."))
  (let [result (pulls/merge owner repo (:number pull-request) {})]
    (if (:merged result)
      (println (str "Successfully merged pull request #" (:number pull-request) "."))
      (reject-from-queue owner repo pull-request credentials (:message result))
      )))


(defn merge-pull-requests []
  (println "Checking pull requests...")
  (let [owner       (System/getenv "GITHUB_MERGE_BOT_OWNER")
        repo        (System/getenv "GITHUB_MERGE_BOT_REPO")
        credentials {:username (System/getenv "GITHUB_MERGE_BOT_USERNAME")
                     :password (System/getenv "GITHUB_MERGE_BOT_PASSWORD")}]
    (tentacles/with-defaults
      {:auth (str (:username credentials) ":" (:password credentials))}
      (git/with-credentials (:username credentials) (:password credentials)
                            (if-let [pr (merge-candidate owner repo (pulls/pulls owner repo))]
                              (if (head-up-to-date-with-base? owner repo pr)
                                (try-merge-pull-request owner repo pr credentials)
                                (update-pull-request owner repo pr credentials))
                              (println "No pull requests found to merge or update."))))))

(defn -main
  [& args]
  (let [timer-task (proxy [TimerTask] []
                     (run []
                       (merge-pull-requests)))]
    (.schedule (Timer.) timer-task 0 30000)))
