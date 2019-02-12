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

(def REJECT_LABEL "rejectedFromQueue")
(def IN_PROGRESS_MSG "Required status check \"workflow\" is in progress.")
(def HOLD_REJECT_MESSAGES '(IN_PROGRESS_MSG))


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
;  (println "WHOLE PR DATA:" pull-request)
  (let [labels (set (map :name (:labels pull-request)))]
    (and (contains? labels "LGTM")
         (not (contains? labels REJECT_LABEL)))))

(defn merge-candidate [owner repo pull-requests]
;  (println "MERGE CANDIDATES " pull-requests)
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
  (println (str "Checking if should reject " (:number pull-request) ", status: "result-status))
  (if (= 0 (compare (str result-status) IN_PROGRESS_MSG))
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
;
;MERGE
;
;CANDIDATES
;
;({:html_url                                                                                                                https
;  :
;  //github.com/KITSAndrzejWower/bot-test/pull/32,                                                                          :merge_commit_sha
;  71                                                                                                                       a337cadea0fcb813162bd985e718d5f800144b,
;  :patch_url                                                                                                               https
;  :
;  //github.com/KITSAndrzejWower/bot-test/pull/32.patch,
;  :labels                                                                                                                  [{:id                                                           1217087710,
;                                                                                                                             :node_id                                                      MDU6TGFiZWwxMjE3MDg3NzEw,
;                                                                                                                             :url                                                          https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/labels/LGTM, :name
;                                                                                                                             LGTM,                                                         :color
;                                                                                                                             ea9aad,                                                       :default
;                                                                                                                             false}],
;  :assignees                                                                                                               [],
;  :closed_at                                                                                                               nil,
;  :review_comment_url                                                                                                      https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/comments                                                          {/number},
;  :author_association                                                                                                      OWNER,
;  :number                                                                                                                  32,
;  :milestone                                                                                                               nil,
;  :requested_reviewers                                                                                                     [],
;  :node_id                                                                                                                 MDExOlB1bGxSZXF1ZXN0MjQ5NTg3NDE1,
;  :merged_at                                                                                                               nil,
;  :statuses_url                                                                                                            https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/statuses/5b0ff27e68183a4d4a67c9276cff06b9c7da5484,
;  :state                                                                                                                   open,
;  :issue_url                                                                                                               https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/issues/32,                                                              :title
;  brake                                                                                                                    test,
;  :commits_url                                                                                                             https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32/commits,
;  :updated_at                                                                                                              2019
;  -02-01T14:09
;  :
;  28                                                                                                                       Z,
;  :head                                                                                                                    {:label                                             KITSAndrzejWower:brake_test,
;                                                                                                                            :ref                                               brake_test,
;                                                                                                                            :sha                                               5
;                                                                                                                            b0ff27e68183a4d4a67c9276cff06b9c7da5484,           :user
;                                                                                                                            {:html_url                                                https
;                                                                                                                             :
;                                                                                                                             //github.com/KITSAndrzejWower,                           :gravatar_id,
;                                                                                                                             :followers_url                                           https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/followers,
;                                                                                                                             :subscriptions_url                                       https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/subscriptions,   :site_admin
;                                                                                                                             false,                                                   :following_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/following
;                                                                                                                             {/other_user},                                           :node_id
;                                                                                                                             MDQ6VXNlcjQ0Njc4MTY1,                                    :type
;                                                                                                                             User,                                                    :received_events_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/received_events, :login
;                                                                                                                             KITSAndrzejWower,                                        :organizations_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/orgs,
;                                                                                                                             :id                                                      44678165,
;                                                                                                                             :events_url                                              https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/events           {/privacy},
;                                                                                                                             :url                                                     https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower,
;                                                                                                                             :repos_url                                               https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/repos,           :starred_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/starred
;                                                                                                                             {/owner}                                                 {/repo},
;                                                                                                                             :gists_url                                               https
;                                                                                                                             :
;                                                                                                                             //api.github.com/users/KITSAndrzejWower/gists            {/gist_id},
;                                                                                                                             :avatar_url                                              https
;                                                                                                                             :
;                                                                                                                             //avatars3.githubusercontent.com/u/44678165?v=4}, :repo
;                                                                                                                            {:html_url                                                       https
;                                                                                                                             :
;                                                                                                                             //github.com/KITSAndrzejWower/bot-test,                         :description
;                                                                                                                             temp                                                            test
;                                                                                                                             repo                                                            for
;                                                                                                                             auto                                                            merge
;                                                                                                                             bot,                                                            :archived
;                                                                                                                             false,                                                          :open_issues_count
;                                                                                                                             1,                                                              :watchers
;                                                                                                                             1,                                                              :ssh_url
;                                                                                                                             git                                                             @github.com:KITSAndrzejWower/bot-test.git,
;                                                                                                                             :hooks_url                                                      https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/hooks,
;                                                                                                                             :archive_url                                                    https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/               {archive_format}
;                                                                                                                             {/ref},                                                         :keys_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/keys
;                                                                                                                             {/key_id},                                                      :forks_count
;                                                                                                                             0,                                                              :languages_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/languages,     :git_url
;                                                                                                                             git
;                                                                                                                             :
;                                                                                                                             //github.com/KITSAndrzejWower/bot-test.git,
;                                                                                                                             :issue_comment_url                                              https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/issues/comments{/number},
;                                                                                                                             :git_refs_url                                                   https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/git/refs
;                                                                                                                             {/sha},                                                         :clone_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //github.com/KITSAndrzejWower/bot-test.git,                     :contents_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/contents/
;                                                                                                                             {+path},                                                        :has_downloads
;                                                                                                                             true,                                                           :teams_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/teams,         :has_issues
;                                                                                                                             true,                                                           :issue_events_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/issues/events
;                                                                                                                             {/number},                                                      :license
;                                                                                                                             nil,                                                            :private
;                                                                                                                             false,                                                          :watchers_count
;                                                                                                                             1,                                                              :collaborators_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/collaborators  {/collaborator},
;                                                                                                                             :homepage                                                       nil,
;                                                                                                                             :git_commits_url                                                https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/git/commits
;                                                                                                                             {/sha},                                                         :name
;                                                                                                                             bot-test,                                                       :releases_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/releases       {/id},
;                                                                                                                             :milestones_url                                                 https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/milestones
;                                                                                                                             {/number},                                                      :svn_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //github.com/KITSAndrzejWower/bot-test,                         :node_id
;                                                                                                                             MDEwOlJlcG9zaXRvcnkxNjc5ODEyMDc=,                               :merges_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/merges,
;                                                                                                                             :compare_url                                                    https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/compare/       {base}
;                                                                                                                             ...                                                             {head},
;                                                                                                                             :stargazers_count                                               1,
;                                                                                                                             :tags_url                                                       https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/tags,
;                                                                                                                             :statuses_url                                                   https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/statuses/      {sha},
;                                                                                                                             :notifications_url                                              https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/notifications
;                                                                                                                             {?since, all, participating},                                   :open_issues
;                                                                                                                             1,                                                              :has_wiki
;                                                                                                                             true,                                                           :size
;                                                                                                                             191,                                                            :assignees_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/assignees      {/user},
;                                                                                                                             :commits_url                                                    https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/commits
;                                                                                                                             {/sha},                                                         :labels_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/labels         {/name},
;                                                                                                                             :forks_url                                                      https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/forks,
;                                                                                                                             :contributors_url                                               https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/contributors,  :updated_at
;                                                                                                                             2019                                                            -02-01T14:08
;                                                                                                                             :                                                               25
;                                                                                                                             Z,                                                              :pulls_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/pulls
;                                                                                                                             {/number},                                                      :has_pages
;                                                                                                                             false,                                                          :default_branch
;                                                                                                                             master,                                                         :language
;                                                                                                                             JavaScript,                                                     :comments_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/comments       {/number},
;                                                                                                                             :id                                                             167981207,
;                                                                                                                             :stargazers_url                                                 https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/stargazers,
;                                                                                                                             :issues_url                                                     https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/issues         {/number},
;                                                                                                                             :trees_url                                                      https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/git/trees
;                                                                                                                             {/sha},                                                         :events_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/events,        :branches_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/branches
;                                                                                                                             {/branch},                                                      :url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test,               :downloads_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/downloads,
;                                                                                                                             :forks                                                          0,
;                                                                                                                             :subscribers_url                                                https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/subscribers,   :full_name
;                                                                                                                             KITSAndrzejWower/bot-test,                                      :blobs_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/git/blobs
;                                                                                                                             {/sha},                                                         :subscription_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/subscription,  :fork
;                                                                                                                             false,                                                          :deployments_url
;                                                                                                                             https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/deployments,
;                                                                                                                             :has_projects                                                   true,
;                                                                                                                             :pushed_at                                                      2019
;                                                                                                                             -02-01T14:08
;                                                                                                                             :
;                                                                                                                             24                                                              Z,
;                                                                                                                             :owner                                                          {:html_url                                                https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //github.com/KITSAndrzejWower,                           :gravatar_id,
;                                                                                                                                                                                              :followers_url                                           https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/followers,
;                                                                                                                                                                                              :subscriptions_url                                       https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/subscriptions,   :site_admin
;                                                                                                                                                                                              false,                                                   :following_url
;                                                                                                                                                                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/following
;                                                                                                                                                                                              {/other_user},                                           :node_id
;                                                                                                                                                                                              MDQ6VXNlcjQ0Njc4MTY1,                                    :type
;                                                                                                                                                                                              User,                                                    :received_events_url
;                                                                                                                                                                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/received_events, :login
;                                                                                                                                                                                              KITSAndrzejWower,                                        :organizations_url
;                                                                                                                                                                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/orgs,
;                                                                                                                                                                                              :id                                                      44678165,
;                                                                                                                                                                                              :events_url                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/events           {/privacy},
;                                                                                                                                                                                              :url                                                     https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower,
;                                                                                                                                                                                              :repos_url                                               https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/repos,           :starred_url
;                                                                                                                                                                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/starred
;                                                                                                                                                                                              {/owner}                                                 {/repo},
;                                                                                                                                                                                              :gists_url                                               https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //api.github.com/users/KITSAndrzejWower/gists            {/gist_id},
;                                                                                                                                                                                              :avatar_url                                              https
;                                                                                                                                                                                              :
;                                                                                                                                                                                              //avatars3.githubusercontent.com/u/44678165?v=4},
;                                                                                                                             :git_tags_url                                                   https
;                                                                                                                             :
;                                                                                                                             //api.github.com/repos/KITSAndrzejWower/bot-test/git/tags       {/sha},
;                                                                                                                             :created_at                                                     2019
;                                                                                                                             -01-28T15:02
;                                                                                                                             :                                                               12
;                                                                                                                             Z,                                                              :mirror_url
;                                                                                                                             nil}},
;  :diff_url                                                                                                                https
;  :
;  //github.com/KITSAndrzejWower/bot-test/pull/32.diff,                                                                     :comments_url
;  https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/issues/32/comments,
;  :locked                                                                                                                  false,
;  :id                                                                                                                      249587415,
;  :url                                                                                                                     https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32,                                                               :base
;  {:label                                             KITSAndrzejWower:master,
;   :ref                                               master,
;   :sha                                               8
;   b64f3a71176ec0859ba4998c96c25ed3d6b0461,           :user
;   {:html_url                                                https
;    :
;    //github.com/KITSAndrzejWower,                           :gravatar_id,
;    :followers_url                                           https
;    :
;    //api.github.com/users/KITSAndrzejWower/followers,
;    :subscriptions_url                                       https
;    :
;    //api.github.com/users/KITSAndrzejWower/subscriptions,   :site_admin
;    false,                                                   :following_url
;    https
;    :
;    //api.github.com/users/KITSAndrzejWower/following
;    {/other_user},                                           :node_id
;    MDQ6VXNlcjQ0Njc4MTY1,                                    :type
;    User,                                                    :received_events_url
;    https
;    :
;    //api.github.com/users/KITSAndrzejWower/received_events, :login
;    KITSAndrzejWower,                                        :organizations_url
;    https
;    :
;    //api.github.com/users/KITSAndrzejWower/orgs,
;    :id                                                      44678165,
;    :events_url                                              https
;    :
;    //api.github.com/users/KITSAndrzejWower/events           {/privacy},
;    :url                                                     https
;    :
;    //api.github.com/users/KITSAndrzejWower,
;    :repos_url                                               https
;    :
;    //api.github.com/users/KITSAndrzejWower/repos,           :starred_url
;    https
;    :
;    //api.github.com/users/KITSAndrzejWower/starred
;    {/owner}                                                 {/repo},
;    :gists_url                                               https
;    :
;    //api.github.com/users/KITSAndrzejWower/gists            {/gist_id},
;    :avatar_url                                              https
;    :
;    //avatars3.githubusercontent.com/u/44678165?v=4}, :repo
;   {:html_url                                                       https
;    :
;    //github.com/KITSAndrzejWower/bot-test,                         :description
;    temp                                                            test
;    repo                                                            for
;    auto                                                            merge
;    bot,                                                            :archived
;    false,                                                          :open_issues_count
;    1,                                                              :watchers
;    1,                                                              :ssh_url
;    git                                                             @github.com:KITSAndrzejWower/bot-test.git,
;    :hooks_url                                                      https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/hooks,
;    :archive_url                                                    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/               {archive_format}
;    {/ref},                                                         :keys_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/keys
;    {/key_id},                                                      :forks_count
;    0,                                                              :languages_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/languages,     :git_url
;    git
;    :
;    //github.com/KITSAndrzejWower/bot-test.git,
;    :issue_comment_url                                              https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/issues/comments{/number},
;    :git_refs_url                                                   https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/git/refs
;    {/sha},                                                         :clone_url
;    https
;    :
;    //github.com/KITSAndrzejWower/bot-test.git,                     :contents_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/contents/
;    {+path},                                                        :has_downloads
;    true,                                                           :teams_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/teams,         :has_issues
;    true,                                                           :issue_events_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/issues/events
;    {/number},                                                      :license
;    nil,                                                            :private
;    false,                                                          :watchers_count
;    1,                                                              :collaborators_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/collaborators  {/collaborator},
;    :homepage                                                       nil,
;    :git_commits_url                                                https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/git/commits
;    {/sha},                                                         :name
;    bot-test,                                                       :releases_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/releases       {/id},
;    :milestones_url                                                 https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/milestones
;    {/number},                                                      :svn_url
;    https
;    :
;    //github.com/KITSAndrzejWower/bot-test,                         :node_id
;    MDEwOlJlcG9zaXRvcnkxNjc5ODEyMDc=,                               :merges_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/merges,
;    :compare_url                                                    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/compare/       {base}
;    ...                                                             {head},
;    :stargazers_count                                               1,
;    :tags_url                                                       https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/tags,
;    :statuses_url                                                   https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/statuses/      {sha},
;    :notifications_url                                              https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/notifications
;    {?since, all, participating},                                   :open_issues
;    1,                                                              :has_wiki
;    true,                                                           :size
;    191,                                                            :assignees_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/assignees      {/user},
;    :commits_url                                                    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/commits
;    {/sha},                                                         :labels_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/labels         {/name},
;    :forks_url                                                      https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/forks,
;    :contributors_url                                               https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/contributors,  :updated_at
;    2019                                                            -02-01T14:08
;    :                                                               25
;    Z,                                                              :pulls_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/pulls
;    {/number},                                                      :has_pages
;    false,                                                          :default_branch
;    master,                                                         :language
;    JavaScript,                                                     :comments_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/comments       {/number},
;    :id                                                             167981207,
;    :stargazers_url                                                 https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/stargazers,
;    :issues_url                                                     https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/issues         {/number},
;    :trees_url                                                      https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/git/trees
;    {/sha},                                                         :events_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/events,        :branches_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/branches
;    {/branch},                                                      :url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test,               :downloads_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/downloads,
;    :forks                                                          0,
;    :subscribers_url                                                https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/subscribers,   :full_name
;    KITSAndrzejWower/bot-test,                                      :blobs_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/git/blobs
;    {/sha},                                                         :subscription_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/subscription,  :fork
;    false,                                                          :deployments_url
;    https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/deployments,
;    :has_projects                                                   true,
;    :pushed_at                                                      2019
;    -02-01T14:08
;    :
;    24                                                              Z,
;    :owner                                                          {:html_url                                                https
;                                                                     :
;                                                                     //github.com/KITSAndrzejWower,                           :gravatar_id,
;                                                                     :followers_url                                           https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/followers,
;                                                                     :subscriptions_url                                       https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/subscriptions,   :site_admin
;                                                                     false,                                                   :following_url
;                                                                     https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/following
;                                                                     {/other_user},                                           :node_id
;                                                                     MDQ6VXNlcjQ0Njc4MTY1,                                    :type
;                                                                     User,                                                    :received_events_url
;                                                                     https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/received_events, :login
;                                                                     KITSAndrzejWower,                                        :organizations_url
;                                                                     https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/orgs,
;                                                                     :id                                                      44678165,
;                                                                     :events_url                                              https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/events           {/privacy},
;                                                                     :url                                                     https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower,
;                                                                     :repos_url                                               https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/repos,           :starred_url
;                                                                     https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/starred
;                                                                     {/owner}                                                 {/repo},
;                                                                     :gists_url                                               https
;                                                                     :
;                                                                     //api.github.com/users/KITSAndrzejWower/gists            {/gist_id},
;                                                                     :avatar_url                                              https
;                                                                     :
;                                                                     //avatars3.githubusercontent.com/u/44678165?v=4},
;    :git_tags_url                                                   https
;    :
;    //api.github.com/repos/KITSAndrzejWower/bot-test/git/tags       {/sha},
;    :created_at                                                     2019
;    -01-28T15:02
;    :                                                               12
;    Z,                                                              :mirror_url
;    nil}},                                                                                                                 :_links
;  {:self            {:href https : //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32},
;   :html            {:href https : //github.com/KITSAndrzejWower/bot-test/pull/32},
;   :issue           {:href https : //api.github.com/repos/KITSAndrzejWower/bot-test/issues/32},
;   :comments        {:href https
;                     :
;                     //api.github.com/repos/KITSAndrzejWower/bot-test/issues/32/comments},
;   :review_comments {:href https : //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32/comments},
;   :review_comment  {:href                                                          https
;                     :
;                     //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/comments{/number}},
;   :commits         {:href https : //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32/commits},
;   :statuses        {:href https
;                     :
;                     //api.github.com/repos/KITSAndrzejWower/bot-test/statuses/5b0ff27e68183a4d4a67c9276cff06b9c7da5484}}, :body,
;  :user                                                                                                                    {:html_url                                                https
;                                                                                                                            :
;                                                                                                                            //github.com/KITSAndrzejWower,                           :gravatar_id,
;                                                                                                                            :followers_url                                           https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/followers,
;                                                                                                                            :subscriptions_url                                       https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/subscriptions,   :site_admin
;                                                                                                                            false,                                                   :following_url
;                                                                                                                            https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/following
;                                                                                                                            {/other_user},                                           :node_id
;                                                                                                                            MDQ6VXNlcjQ0Njc4MTY1,                                    :type
;                                                                                                                            User,                                                    :received_events_url
;                                                                                                                            https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/received_events, :login
;                                                                                                                            KITSAndrzejWower,                                        :organizations_url
;                                                                                                                            https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/orgs,
;                                                                                                                            :id                                                      44678165,
;                                                                                                                            :events_url                                              https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/events           {/privacy},
;                                                                                                                            :url                                                     https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower,
;                                                                                                                            :repos_url                                               https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/repos,           :starred_url
;                                                                                                                            https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/starred
;                                                                                                                            {/owner}                                                 {/repo},
;                                                                                                                            :gists_url                                               https
;                                                                                                                            :
;                                                                                                                            //api.github.com/users/KITSAndrzejWower/gists            {/gist_id},
;                                                                                                                            :avatar_url                                              https
;                                                                                                                            :
;                                                                                                                            //avatars3.githubusercontent.com/u/44678165?v=4},
;  :review_comments_url                                                                                                     https
;  :
;  //api.github.com/repos/KITSAndrzejWower/bot-test/pulls/32/comments,
;  :requested_teams                                                                                                         [],
;  :assignee                                                                                                                nil,
;  :created_at                                                                                                              2019
;  -02-01T12:29
;  :
;  46                                                                                                                       Z})
