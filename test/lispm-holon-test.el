;;; lispm-holon-test.el --- Tests for lispm-holon  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-holon
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run with `make test', lispm-agent's lisp/ on the load path.  They need
;; no Holon: its daemon is a server in this Emacs, on the socket of a
;; fresh HOLON_HOME, and its command is test/fake-holon.

;;; Code:

(require 'ert)
(require 'lispm-holon)

(defconst lispm-holon-test-fake
  (expand-file-name "fake-holon" (file-name-directory (or load-file-name buffer-file-name)))
  "The stand-in for Holon's command.")

;;;; A daemon

(defun lispm-holon-test--answer (routes request)
  "Return the HTTP/1.0 response to REQUEST, a whole request, as ROUTES say.
ROUTES is a list of (\"METHOD PATH\" . ANSWER); ANSWER is the JSON
answered, or a function of the request's JSON returning (STATUS . JSON).
A request ROUTES do not name is not found."
  (string-match "\\`\\([A-Z]+\\) \\([^ ]+\\)" request)
  (let* ((route (concat (match-string 1 request) " " (match-string 2 request)))
         (body (substring request (+ (string-search "\r\n\r\n" request) 4)))
         (answer (cdr (assoc route routes)))
         (answer (cond ((functionp answer)
                        (funcall answer (and (not (string-empty-p body))
                                             (lispm-holon--json (decode-coding-string body 'utf-8)))))
                       (answer (cons 200 answer))
                       (t (cons 404 '(:ok :false :error "Not Found")))))
         (json (lispm-holon--utf-8 (cdr answer))))
    (format "HTTP/1.0 %d Answer\r\ncontent-type: application/json\r\ncontent-length: %d\r\n\r\n%s"
            (car answer) (length json) json)))

(defun lispm-holon-test--serve (routes)
  "Start a daemon answering ROUTES on the socket of Holon's home; return it.
See `lispm-holon-test--answer'."
  (make-directory (lispm-holon--file "run") t)
  (make-network-process
   :name "lispm-holon-test daemon" :server t :family 'local
   :service (lispm-holon--file "run/holon.sock") :coding 'binary :noquery t
   :sentinel #'ignore
   :filter (lambda (process string)
             (let ((request (concat (process-get process :request) string)))
               (process-put process :request request)
               (when-let* ((end (string-search "\r\n\r\n" request)))
                 (when (>= (- (length request) end 4)
                           (if (string-match "Content-Length: \\([0-9]+\\)" request)
                               (string-to-number (match-string 1 request))
                             0))
                   (process-send-string process (lispm-holon-test--answer routes request))
                   (delete-process process)))))))

(defmacro lispm-holon-test-with-home (routes &rest body)
  "Run BODY with HOLON_HOME a fresh directory, its daemon answering ROUTES,
see `lispm-holon-test--answer'; with ROUTES nil, no daemon runs.  Emacs's
own directory is a fresh one inside it."
  (declare (indent 1))
  `(let* ((home (make-temp-file "lh" t))
          (user-emacs-directory (file-name-as-directory (expand-file-name "emacs" home)))
          (process-environment (cons (concat "HOLON_HOME=" home) process-environment))
          (lispm-holon--cache nil)
          (routes ,routes)
          (server (and routes (lispm-holon-test--serve routes))))
     (unwind-protect
         (progn ,@body)
       (when server (delete-process server))
       (delete-directory home t))))

(defmacro lispm-holon-test-with-program (configured &rest body)
  "Run BODY with test/fake-holon as Holon's command, configured with a model
when CONFIGURED; `calls' returns the arguments of its calls, a string each."
  (declare (indent 1))
  `(let* ((fake-directory (make-temp-file "lispm-holon-test-fake" t))
          (process-environment (cons (concat "FAKE_HOLON_DIR=" fake-directory) process-environment))
          (lispm-holon-program lispm-holon-test-fake))
     (when ,configured
       (with-temp-file (expand-file-name "model" fake-directory)))
     (cl-flet ((calls ()
                 (let ((file (expand-file-name "calls" fake-directory)))
                   (and (file-exists-p file)
                        (with-temp-buffer (insert-file-contents file)
                                          (split-string (buffer-string) "\n" t))))))
       (unwind-protect
           (progn ,@body)
         (delete-directory fake-directory t)))))

;;;; Agents as the daemon gives them

(defun lispm-holon-test--entry (id &optional name status)
  "Return the agent ID as the daemon lists it, named NAME, of STATUS."
  (list :identity (append (list :agent_id id :status (or status "active") :can_rename t)
                          (and name (list :name name)))
        :status "asleep"))

(defun lispm-holon-test--state (workspaces &optional brief turn)
  "Return an agent's state as the daemon gives it: in WORKSPACES, a vector
of (ID ANCHOR ACTIVE), its last BRIEF given and last TURN ended at those
times, RFC 3339 timestamps."
  (list :agent (list :agent (append (and brief (list :last_brief_at brief))
                                    (and turn (list :last_turn_terminal (list :completed_at turn))))
                     :identity '(:agent_id "x"))
        :session (if turn (list :last_turn (list :completed_at turn)) '(:status "idle"))
        :workspace
        (list :workspaces
              (vconcat (mapcar (pcase-lambda (`(,id ,anchor ,active))
                                 (append (list :workspace_id id :workspace_anchor anchor
                                               :is_active (if active t :false))
                                         (and (string-prefix-p "agent_home:" id)
                                              (list :workspace_alias "agent_home"))))
                               workspaces)))))

;;;; The agent

(ert-deftest lispm-holon-test-defined-on-load ()
  "Loaded, Holon is one of the editor's agents, taking part while its
command is on PATH, started with o."
  (let ((props (alist-get 'holon lispm-agents-defined)))
    (should (equal (plist-get props :name) "Holon"))
    (should (equal (plist-get props :program) "holon"))
    (should (equal (plist-get props :key) "o"))
    (should (= (string-width (plist-get props :text-icon)) 1))
    (should-not (plist-get props :built-in))
    (should-not (plist-get props :context))
    (should (file-readable-p (nth 1 (plist-get props :icon)))))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (should-not (lispm-agents-available-p 'holon)))
  (cl-letf (((symbol-function 'executable-find) (lambda (program &rest _) (concat "/bin/" program))))
    (should (lispm-agents-available-p 'holon))
    (should (eq (keymap-lookup lispm-agents-map "o") 'lispm-holon-new))))

(ert-deftest lispm-holon-test-home ()
  "Holon's home is HOLON_HOME, else ~/.holon."
  (let ((process-environment (cons "HOLON_HOME=/srv/holon" process-environment)))
    (should (equal (lispm-holon-home) "/srv/holon/"))
    (should (equal (lispm-holon--file "run/holon.sock") "/srv/holon/run/holon.sock")))
  (dolist (unset '("HOLON_HOME" "HOLON_HOME="))
    (let ((process-environment (cons unset process-environment)))
      (should (equal (lispm-holon-home) (expand-file-name "~/.holon/"))))))

;;;; The daemon

(ert-deftest lispm-holon-test-request-and-response ()
  "A request is plain HTTP/1.0, its body JSON; an answer is its JSON, or an
error saying what the daemon said."
  (should (equal (lispm-holon--request "GET" "/api/agents/list" nil)
                 "GET /api/agents/list HTTP/1.0\r\nHost: localhost\r\nAccept: application/json\r\n\r\n"))
  (should (string-suffix-p "Content-Type: application/json\r\nContent-Length: 7\r\n\r\n{\"a\":1}"
                           (lispm-holon--request "POST" "/x" "{\"a\":1}")))
  (should (equal (lispm-holon--utf-8 '(:t "é")) (encode-coding-string "{\"t\":\"é\"}" 'utf-8)))
  (should (equal (lispm-holon--response
                  "HTTP/1.0 200 OK\r\ncontent-type: application/json\r\n\r\n{\"a\":[1,null],\"b\":false}")
                 '(:a (1 nil) :b nil)))
  (should (equal (lispm-holon--response (encode-coding-string "HTTP/1.0 200 OK\r\n\r\n{\"t\":\"é\"}" 'utf-8))
                 '(:t "é")))
  (should (equal (cadr (should-error (lispm-holon--response
                                      "HTTP/1.0 409 Conflict\r\n\r\n{\"ok\":false,\"error\":\"taken\"}")))
                 (format-message "Holon's daemon answered 409: taken")))
  (should (equal (cadr (should-error (lispm-holon--response "HTTP/1.0 502 Bad Gateway\r\n\r\nbusy")))
                 (format-message "Holon's daemon answered 502: busy")))
  (should-error (lispm-holon--response "")))

(ert-deftest lispm-holon-test-ask-the-daemon ()
  "The daemon is asked over its socket, a question with JSON sent along; a
daemon that does not run cannot be asked."
  (let ((sent nil))
    (lispm-holon-test-with-home `(("GET /api/handshake" . (:auth (:mode "local")))
                                  ("POST /api/x" . ,(lambda (json) (setq sent json) '(201 :ok t))))
      (should (lispm-holon--running-p))
      (should (equal (lispm-holon--ask "GET" "/api/handshake") '(:auth (:mode "local"))))
      (should (equal (lispm-holon--ask "POST" "/api/x" '(:ids ["a" "é"])) '(:ok t)))
      (should (equal sent '(:ids ("a" "é"))))
      (should-error (lispm-holon--ask "GET" "/api/nothing"))))
  (lispm-holon-test-with-home nil
    (should-not (lispm-holon--running-p))
    (should-error (lispm-holon--ask "GET" "/api/handshake"))))

(ert-deftest lispm-holon-test-run ()
  "Holon's command is run with its arguments; failing, it says what Holon
said, its log lines left out, and, when Holon has no model, how to give
it one."
  (lispm-holon-test-with-program t
    (let ((lispm-holon--cache '(stamp . sessions)))
      (should (equal (lispm-holon--run "agent" "rename" "a" "--name" "A b") "{\"ok\": true}"))
      (should-not lispm-holon--cache))
    (should (equal (calls) '("agent rename a --name A b")))
    (should (equal (cadr (should-error (lispm-holon--run "agent" "delete" "default" "--yes")
                                       :type 'user-error))
                   (concat "holon agent delete default --yes: /api/control/agents/default returned "
                           "HTTP 409: the configured default agent cannot be deleted "
                           "[agent_delete_forbidden]"))))
  (lispm-holon-test-with-program nil
    (should (string-match-p "M-x lispm-holon-setup"
                            (cadr (should-error (lispm-holon--run "daemon" "start")
                                                :type 'user-error))))))

(ert-deftest lispm-holon-test-daemon-started-when-not-running ()
  "Starting a session starts Holon's daemon when it does not run, and says
so when it does not come up."
  (lispm-holon-test-with-home '(("GET /api/handshake" . (:ok t)))
    (lispm-holon-test-with-program t
      (lispm-holon--ensure-daemon)
      (should-not (calls))))
  (lispm-holon-test-with-home nil
    (lispm-holon-test-with-program t
      (let ((lispm-holon-timeout 0.3)
            (inhibit-message t))
        (should (string-match-p "did not start"
                                (cadr (should-error (lispm-holon--ensure-daemon) :type 'user-error))))
        (should (equal (calls) '("daemon start")))))
    (lispm-holon-test-with-program nil
      (let ((inhibit-message t))
        (should (string-match-p "lispm-holon-setup"
                                (cadr (should-error (lispm-holon--ensure-daemon) :type 'user-error))))))))

;;;; Sessions

(ert-deftest lispm-holon-test-session-of-an-agent ()
  "An agent is a session named as the agent, of the project it works in,
else of the one attached to it, else a chat at home; it is as recent as
its last turn or brief."
  (let ((brief "2026-10-23T10:24:26.778615Z")
        (turn "2026-10-23T10:24:30Z"))
    (should (equal (lispm-holon--session
                    (lispm-holon-test--entry "fix" "Fix it")
                    (lispm-holon--json
                     (lispm-holon--utf-8 (lispm-holon-test--state
                                          '(("ws_1" "/src/a" nil) ("ws_2" "/src/b" t)
                                            ("agent_home:fix" "/h/agents/fix" nil))
                                          brief turn)))
                    1.0)
                   (list :agent 'holon :id "fix" :cwd "/src/b" :title "Fix it"
                         :time (float-time (parse-iso8601-time-string turn)))))
    (let ((attached (lispm-holon--session
                     (lispm-holon-test--entry "fix")
                     (lispm-holon--json
                      (lispm-holon--utf-8 (lispm-holon-test--state
                                           '(("agent_home:fix" "/h/agents/fix" t) ("ws_1" "/src/a" nil))
                                           brief)))
                     1.0)))
      (should (equal (plist-get attached :cwd) "/src/a"))
      (should (equal (plist-get attached :title) "fix"))
      (should (equal (plist-get attached :time) (float-time (parse-iso8601-time-string brief)))))
    (let ((chat (lispm-holon--session
                 (lispm-holon-test--entry "chat")
                 (lispm-holon--json
                  (lispm-holon--utf-8 (lispm-holon-test--state '(("agent_home:chat" "/h/agents/chat" t)))))
                 7.0)))
      (should (equal (plist-get chat :cwd) (expand-file-name "~")))
      (should (equal (plist-get chat :time) 7.0)))))

(ert-deftest lispm-holon-test-sessions-from-the-daemon ()
  "The sessions are the agents the daemon lists, those being deleted left
out, asked again only when Holon's database changed; without the daemon
there are none."
  (let ((asked 0))
    (lispm-holon-test-with-home
        `(("GET /api/agents/list"
           . ,(lambda (_)
                (setq asked (1+ asked))
                (cons 200 (vector (lispm-holon-test--entry "one" "One")
                            (lispm-holon-test--entry "gone" nil "deleting")
                            (lispm-holon-test--entry "two")))))
          ("GET /api/agents/one/state"
           . ,(lispm-holon-test--state '(("ws_1" "/src/one" t)) "2026-10-23T10:00:00Z"))
          ("GET /api/agents/two/state"
           . ,(lispm-holon-test--state '(("agent_home:two" "/h" t)) "2026-10-23T11:00:00Z")))
      (let ((wal (lispm-holon--file "state/runtime.sqlite-wal")))
        (make-directory (file-name-directory wal) t)
        (write-region "" nil wal nil 'silent)
        (should (equal (mapcar (lambda (session) (list (plist-get session :id) (plist-get session :title)
                                                       (plist-get session :cwd)))
                               (lispm-holon-sessions))
                       `(("one" "One" "/src/one") ("two" "two" ,(expand-file-name "~")))))
        (lispm-holon-sessions)
        (should (= asked 1))
        (set-file-times wal (time-add nil 10))
        (lispm-holon-sessions)
        (should (= asked 2)))))
  (lispm-holon-test-with-home nil
    (should-not (lispm-holon-sessions))))

(ert-deftest lispm-holon-test-sessions-unavailable ()
  "A daemon that answers with an error lists no sessions, and says so."
  (lispm-holon-test-with-home '(("GET /api/handshake" . (:ok t)))
    (let ((said nil))
      (cl-letf (((symbol-function 'message) (lambda (format &rest args)
                                              (setq said (apply #'format-message format args)))))
        (should-not (lispm-holon-sessions)))
      (should (string-prefix-p (format-message "Holon sessions unavailable: Holon's daemon answered 404") said)))))

(ert-deftest lispm-holon-test-stale-socket ()
  "A socket a stopped daemon left behind is a daemon not running: no
sessions, and nothing said."
  (lispm-holon-test-with-home '(("GET /api/handshake" . (:ok t)))
    (delete-process server)
    (should (file-exists-p (lispm-holon--file "run/holon.sock")))
    (should-not (lispm-holon--running-p))
    (let ((said nil))
      (cl-letf (((symbol-function 'message) (lambda (&rest args) (setq said args))))
        (should-not (lispm-holon-sessions)))
      (should-not said))))

(ert-deftest lispm-holon-test-rename-and-delete ()
  "Renaming and deleting an agent go through Holon's command, and wait for
the daemon's list to show it, which follows a moment after."
  (let ((answers 0))
    (lispm-holon-test-with-home
        `(("GET /api/agents/list"
           . ,(lambda (_)
                (setq answers (1+ answers))
                (if (< answers 3)
                    (cons 200 (vector (lispm-holon-test--entry "a" "Old")))
                  (cons 200 (vector (lispm-holon-test--entry "a" "New" "deleting")))))))
      (lispm-holon-test-with-program t
        (lispm-holon--rename '(:id "a") "New")
        (should (= answers 3))
        (lispm-holon--delete '(:id "a"))
        (should (= answers 4))
        (should (equal (calls) '("agent rename a --name New" "agent delete a --yes")))
        (should-not lispm-holon--cache)))))

;;;; Starting and resuming

(ert-deftest lispm-holon-test-agent-id ()
  "An agent is named after its project, as Holon's ids allow, apart from
the agents there are."
  (should (equal (lispm-holon--id "lisp_machine" nil) "lisp_machine"))
  (should (equal (lispm-holon--id "My Project (2)" nil) "my-project-2"))
  (should (equal (lispm-holon--id ".dotfiles" nil) "dotfiles"))
  (should (equal (lispm-holon--id "Έργο" nil) "agent"))
  (should (equal (lispm-holon--id "web" '("web" "web-2")) "web-3")))

(ert-deftest lispm-holon-test-select ()
  "Holon's terminal UI is made to open on an agent by naming it in its
state file."
  (lispm-holon-test-with-home nil
    (lispm-holon--select "web-2")
    (let ((state (lispm-holon--json (with-temp-buffer
                                      (insert-file-contents (lispm-holon--file "state/tui/local.json"))
                                      (buffer-string)))))
      (should (equal (plist-get state :last_selected_agent_id) "web-2"))
      (should (< (abs (- (float-time (parse-iso8601-time-string (plist-get state :updated_at)))
                         (float-time)))
                 5)))))

(ert-deftest lispm-holon-test-new-agent-for-the-project ()
  "A new session is a new agent named after the project, the project
attached to it, Holon's terminal UI opened on it; outside projects it is
a chat; `lispm-holon-template' says what it is made from.  An id an
agent had, though deleted, is passed over: Holon would give the new
agent the old one's conversation."
  (lispm-holon-test-with-home
      `(("GET /api/handshake" . (:ok t))
        ("GET /api/agents/list" . ,(lambda (_) (cons 200 (vector (lispm-holon-test--entry "web")))))
        ("GET /api/agents/web-2/state"
         . ,(lambda (_) '(410 :ok :false :code "agent_deleted" :error "agent web-2 was deleted"))))
    (lispm-holon-test-with-program t
      (let ((project "/src/web"))
        (cl-letf (((symbol-function 'lispm-holon--project) (lambda () project)))
          (should (equal (lispm-holon--new-arguments) '("web-3" "tui")))
          (should (equal (calls) '("agent create web-3" "workspace attach --agent web-3 /src/web")))
          (should (equal (plist-get (lispm-holon--json
                                     (with-temp-buffer
                                       (insert-file-contents (lispm-holon--file "state/tui/local.json"))
                                       (buffer-string)))
                                    :last_selected_agent_id)
                         "web-3"))
          (setq project nil)
          (let ((lispm-holon-template "code-reviewer"))
            (should (equal (car (lispm-holon--new-arguments)) "chat")))
          (should (equal (car (last (calls))) "agent create chat --template code-reviewer")))))))

(ert-deftest lispm-holon-test-resume ()
  "Resuming opens Holon's terminal UI on the agent."
  (lispm-holon-test-with-home '(("GET /api/handshake" . (:ok t)))
    (should (equal (lispm-holon--resume-arguments "web") '("tui")))
    (should (string-match-p "\"last_selected_agent_id\":\"web\""
                            (with-temp-buffer
                              (insert-file-contents (lispm-holon--file "state/tui/local.json"))
                              (buffer-string))))))

(ert-deftest lispm-holon-test-project ()
  "A new agent's project is that of the main window's buffer."
  (let* ((directory (file-name-as-directory (make-temp-file "lispm-holon-test-project" t)))
         (buffer (find-file-noselect (expand-file-name "file" directory))))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&rest _) (and (string-prefix-p directory default-directory)
                                          (cons 'transient directory)))))
          (with-temp-buffer
            (let ((lispm-agents-main-window-function (lambda () (selected-window))))
              (save-window-excursion
                (set-window-buffer (selected-window) buffer)
                (should (equal (lispm-holon--project) (directory-file-name directory))))
              (save-window-excursion
                (set-window-buffer (selected-window) (current-buffer))
                (let ((default-directory "/"))
                  (should-not (lispm-holon--project)))))))
      (kill-buffer buffer)
      (delete-directory directory t))))

;;;; Transcripts

(ert-deftest lispm-holon-test-transcript ()
  "An agent's transcript is its conversation, oldest first, read a page at
a time: what a person asked and the briefs the agent gave back, what
Holon itself sent left out."
  (let ((older '(:turn_id "t1" :key (:turn_index 1)
                 :inputs [(:message_id "boot" :presentation_class "internal")]
                 :brief_ids ["b1"]))
        (newer '(:turn_id "t2" :key (:turn_index 2) :started_at "2026-10-23T10:22:10Z"
                 :inputs [(:message_id "m1" :presentation_class "operator")]
                 :brief_ids ["b2"]))
        (asked nil))
    (lispm-holon-test-with-home
        `(("GET /api/agents/a/conversation?limit=100"
           . (:turns [,newer] :coverage (:has_older t) :next_before_cursor "c/1="))
          ("GET /api/agents/a/conversation?limit=100&before=c%2F1%3D"
           . (:turns [,older] :coverage (:has_older :false)))
          ("POST /api/agents/a/messages:batchGet"
           . ,(lambda (json)
                (push json asked)
                '(200 :messages [(:id "m1" :body (:type "text" :text "Reply with pong."))])))
          ("POST /api/agents/a/briefs:batchGet"
           . ,(lambda (json)
                (push json asked)
                '(200 :briefs [(:id "b1" :text "Hi, I am Holon.") (:id "b2" :text "pong")]))))
      (let ((file (lispm-holon--transcript '(:id "a" :title "Agent A"))))
        (unwind-protect
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                           (concat "# Agent A\n"
                                   "\n## Holon\n\nHi, I am Holon.\n"
                                   (format-time-string "\n## You · %F %R\n\n"
                                                       (parse-iso8601-time-string "2026-10-23T10:22:10Z"))
                                   "Reply with pong.\n"
                                   "\n## Holon\n\npong\n")))
          (delete-file file)))
      (should (equal (reverse asked) '((:message_ids ("m1")) (:brief_ids ("b1" "b2"))))))))

;;;; In the sandbox

(ert-deftest lispm-holon-test-service-defined ()
  "Where LispM Agent has services, Holon's daemon is one: `holon serve' in
Holon's home, with the rules it needs on top of a level's."
  (skip-unless (featurep 'lispm-services))
  (let ((props (alist-get 'holon lispm-services)))
    (should (equal (plist-get props :name) "Holon"))
    (should (equal (cdr (funcall (plist-get props :command))) '("serve")))
    (should (eq (plist-get props :directory) #'lispm-holon-home))
    (should (eq (plist-get props :sandbox) #'lispm-holon--sandbox-rules))))

(ert-deftest lispm-holon-test-remember-workspaces ()
  "The projects LispM attached are remembered, each once, where Emacs keeps
its files."
  (lispm-holon-test-with-home nil
    (should-not (lispm-holon--workspaces))
    (should (equal (lispm-holon--remember-workspaces '("/src/a/" "/src/b")) '("/src/a" "/src/b")))
    (should-not (lispm-holon--remember-workspaces '("/src/a")))
    (should (equal (lispm-holon--remember-workspaces '("/src/c" "/src/a")) '("/src/c")))
    (should (equal (lispm-holon--workspaces) '("/src/a" "/src/b" "/src/c")))
    (should (string-prefix-p user-emacs-directory (lispm-holon--workspaces-file)))))

(ert-deftest lispm-holon-test-domains ()
  "The domains Holon's daemon may reach are those of its model providers
with credentials, ChatGPT's sign-in with its own; this machine needs none."
  (should (equal (lispm-holon--domains
                  '((:id "anthropic" :base_url "https://api.anthropic.com" :credential_configured t)
                    (:id "openai" :base_url "https://api.openai.com/v1" :credential_configured nil)
                    (:id "openai-codex" :base_url "https://chatgpt.com/backend-api/codex"
                     :credential_configured t)
                    (:id "ollama" :base_url "http://127.0.0.1:11434" :credential_configured t)
                    (:id "litellm" :base_url "http://localhost:4000" :credential_configured t)
                    (:id "proxy" :base_url "https://api.anthropic.com/v2" :credential_configured t)))
                 '("api.anthropic.com" "chatgpt.com" "auth.openai.com"))))

(ert-deftest lispm-holon-test-sandbox-rules ()
  "In the sandbox, Holon's daemon may read and write Holon's home and the
projects remembered or named, bind its socket and port, and reach its
providers and the domains named."
  (lispm-holon-test-with-home nil
    (lispm-holon-test-with-program t
      (with-temp-file (expand-file-name "providers.json" fake-directory)
        (insert "[{\"id\": \"anthropic\", \"base_url\": \"https://api.anthropic.com\","
                " \"credential_configured\": true}]"))
      (lispm-holon--remember-workspaces '("/src/web"))
      (let* ((lispm-holon-sandbox-allow-write '("/srv/shared/"))
             (lispm-holon-sandbox-domains '("html.duckduckgo.com"))
             (rules (lispm-holon--sandbox-rules))
             (home (directory-file-name (lispm-holon-home))))
        (should (equal (plist-get rules :allow-write) (list home "/src/web" "/srv/shared")))
        (should (member "/src/web" (plist-get rules :allow-read)))
        (should (member home (plist-get rules :allow-read)))
        (should (equal (plist-get rules :unix-sockets) (list (lispm-holon--file "run/holon.sock"))))
        (should (eq (plist-get rules :local-binding) t))
        (should (equal (plist-get rules :allowed-domains) '("api.anthropic.com" "html.duckduckgo.com")))))))

(ert-deftest lispm-holon-test-before-service ()
  "Before Holon's daemon starts in the sandbox, Holon's own, running, is
stopped, the projects of its agents remembered first."
  (lispm-holon-test-with-home
      `(("GET /api/handshake" . (:ok t))
        ("GET /api/agents/list"
         . ,(lambda (_) (cons 200 (vector (lispm-holon-test--entry "web")
                                          (lispm-holon-test--entry "gone" nil "deleting")))))
        ("GET /api/agents/web/state"
         . ,(lispm-holon-test--state '(("agent_home:web" "/h/agents/web" t) ("ws_1" "/src/web" nil)
                                      ("ws_2" "/src/docs" nil)))))
    (lispm-holon-test-with-program t
      (let ((lispm-holon-timeout 0.3)
            (inhibit-message t))
        (lispm-holon--before-service)
        (should (equal (lispm-holon--workspaces) '("/src/web" "/src/docs")))
        (should (equal (calls) '("daemon stop"))))))
  (lispm-holon-test-with-home nil
    (lispm-holon-test-with-program t
      (lispm-holon--before-service)
      (should-not (calls)))))

(ert-deftest lispm-holon-test-daemon-in-the-sandbox ()
  "When LispM runs Holon's daemon, starting a session starts the service in
the sandbox, not Holon's own daemon; a project new to it starts it again
first, so that it may write there, and one it knows does not."
  (let ((started 0)
        (restarted 0)
        (running nil))
    (cl-letf (((symbol-function 'lispm-service-managed-p) (lambda (id) (eq id 'holon)))
              ((symbol-function 'lispm-service-process) (lambda (_) running))
              ((symbol-function 'lispm-service-level) (lambda (_) 'low))
              ((symbol-function 'lispm-service-start)
               (lambda (_ &optional _show) (setq started (1+ started) running t)))
              ((symbol-function 'lispm-service-restart)
               (lambda (_ &optional _show) (setq restarted (1+ restarted))))
              ((symbol-function 'lispm-service-buffer-name) (lambda (_) "*Service: Holon*")))
      (lispm-holon-test-with-home
          `(("GET /api/handshake" . (:ok t))
            ("GET /api/agents/list" . ,(lambda (_) (cons 200 (vector)))))
        (lispm-holon-test-with-program t
          (let ((project "/src/web"))
            (cl-letf (((symbol-function 'lispm-holon--project) (lambda () project)))
              ;; Not running yet: started, the project remembered first.
              (should (equal (lispm-holon--new-arguments) '("web" "tui")))
              (should (= started 1))
              (should (= restarted 0))
              (should (equal (lispm-holon--workspaces) '("/src/web")))
              ;; Running: a new project starts it again, a known one does not.
              (setq project "/src/docs")
              (lispm-holon--new-arguments)
              (should (= restarted 1))
              (lispm-holon--new-arguments)
              (should (= restarted 1))
              (should (= started 1))
              (should-not (member "daemon start" (calls))))))))))

;;;; Unloading

(ert-deftest lispm-holon-test-unloading-takes-it-away ()
  "Unloading the package, as the App Store does removing it, takes Holon
away from the agents, and its daemon from the services; loading it again
brings them back."
  (unwind-protect
      (progn
        (unload-feature 'lispm-holon t)
        (should-not (assq 'holon lispm-agents-defined))
        (should-not (keymap-lookup lispm-agents-map "o"))
        (when (featurep 'lispm-services)
          (should-not (assq 'holon lispm-services))))
    (require 'lispm-holon))
  (should (assq 'holon lispm-agents-defined))
  (when (featurep 'lispm-services)
    (should (assq 'holon lispm-services))))

(provide 'lispm-holon-test)
;;; lispm-holon-test.el ends here
