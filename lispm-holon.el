;;; lispm-holon.el --- Holon, one of the editor's agents  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-holon
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, processes
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

;; Holon (https://holon.run), open-source software keeping AI agents at
;; work on your own computer, as one of the editor's agents of LispM
;; Agent (lispm-agents.el), beside Claude Code and Codex: a package of its
;; own, which LispM Desktop's App Store installs.  Loading this file makes
;; Holon an agent (`lispm-agents-define'); unloading it, as the App Store
;; does when it removes it, takes it away again.  It takes part while
;; `holon' is on PATH: o in `lispm-agents-map' starts it in the current
;; project, and its agents join the sessions of the others.
;;
;; It needs lispm-agent 0.8.0 or later (the package `lispm'), which LispM
;; Desktop loads before it.  Package-Requires leaves it out on purpose:
;; package.el would not activate this package where lispm-agent is a
;; checkout on the load path rather than a package, or a package of an
;; older version, and it could not be loaded at all.
;;
;; Holon keeps agents rather than sessions: each lives in Holon's daemon,
;; with a role, a memory and work it follows through, and goes on with
;; one conversation however often it is left and taken up again.  Here,
;; then, a Holon agent is a session:
;;
;; - Starting Holon in a project makes an agent named after the project,
;;   attaches the project to it as its workspace, and opens Holon's own
;;   terminal UI, `holon tui', on it.  The daemon is started first when it
;;   is not running.
;; - The agents are asked of the daemon over its Unix socket,
;;   HOLON_HOME/run/holon.sock, in plain HTTP/1.0: the socket is trusted
;;   whatever the daemon's access mode, and it is asked again only when
;;   Holon's database changed.  An agent belongs to the project it works
;;   in, else to the one attached to it; one with neither is a chat.
;; - Resuming an agent opens the terminal UI on it.  The UI opens on the
;;   agent it showed last, which its state file names, so that file names
;;   the agent first.
;; - Renaming and deleting an agent go through the `holon' command, as
;;   the daemon's agents are its own to change.  An agent's transcript is
;;   its conversation, what it was asked and the briefs it gave back, in
;;   Markdown.
;;
;; Holon takes no MCP servers.  While LispM runs, its agents still talk
;; with the other agents, LispM's and other programs' such as Claude Code,
;; and use LispM's skills and tools (lispm-agents-bridge.el):
;;
;; - What another agent sends a Holon agent goes to Holon's daemon, as a
;;   message from its operator: an agent at work takes it into the turn
;;   under way, one at rest starts a turn on it.  Holon's terminal UI
;;   shows it as it comes.
;; - A Holon agent open in a terminal has a server of its own there,
;;   which it reaches with LispM Agent's command bin/lispm-tool, run in
;;   its shell: the command finds the server by HOLON_AGENT_ID, which
;;   Holon gives every command of the agent's.  A skill, lispm, written
;;   into the agent's own skills while the terminal is open, says how.
;;
;; Holon sees nothing else of Emacs's state.
;;
;; Holon runs its agents' commands as you, with nothing confining them.
;; Where LispM Agent has services (lispm-services.el), Holon's daemon is
;; one, `holon': given a sandbox level of its own (`lispm-service-levels',
;; set where a desktop lists startup items), LispM runs `holon serve' in
;; the sandbox, and every command of Holon's agents runs there too.  On
;; top of the level's rules it may write Holon's home and the projects
;; LispM attached to its agents, bind its socket and local port, and
;; reach the model providers Holon has credentials for.  It then runs
;; while Emacs runs, and is started in place of Holon's own daemon,
;; which is stopped first.  A project attached afterwards starts it
;; again, as the sandbox's rules of files hold from a start; one an
;; agent attached by itself stays read-only, unless named in
;; `lispm-holon-sandbox-allow-write'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'parse-time)
(require 'project)
(require 'url-util)
(require 'url-parse)
(require 'lispm-agents)
(require 'lispm-agents-mcp)
;; LispM Agent's services, where it has them: since lispm-agent 0.7.0.
(require 'lispm-services nil t)

;; Since lispm-agent 0.8.0: servers named to bin/lispm-tool.
(declare-function lispm-agents-mcp-announce "lispm-agents-mcp" (server variable value))
(defvar lispm-agents-mcp-tool-program)
(defvar lispm-agents-mcp-peer-directory)

(declare-function lispm-define-service "lispm-services" (id &rest properties))
(declare-function lispm-undefine-service "lispm-services" (id))
(declare-function lispm-service-managed-p "lispm-services" (id))
(declare-function lispm-service-process "lispm-services" (id))
(declare-function lispm-service-level "lispm-services" (id))
(declare-function lispm-service-start "lispm-services" (id &optional show))
(declare-function lispm-service-restart "lispm-services" (id &optional show))
(declare-function lispm-service-buffer-name "lispm-services" (id))

(defgroup lispm-holon nil
  "Holon, one of the editor's agents."
  :group 'tools
  :prefix "lispm-holon-")

(defcustom lispm-holon-template nil
  "The template of the Holon agents made here, as `holon agent create
--template' takes it: a template's name or file.  nil makes Holon's own
default agent, a coding assistant."
  :type '(choice (const :tag "Holon's default" nil) string))

(defcustom lispm-holon-sandbox-allow-write nil
  "Further places Holon's daemon may write when LispM runs it in the
sandbox, beside Holon's home and the projects LispM attached to its
agents.  Changes take effect when it starts again."
  :type '(repeat directory))

(defcustom lispm-holon-sandbox-domains nil
  "Further domains Holon's daemon may reach when LispM runs it in the
sandbox, beside those of the model providers it has credentials for and
those of the sandbox's level, as for web search.  Entries are srt's:
example.com, *.example.com or example.com:443."
  :type '(repeat string))

(defconst lispm-holon-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package, holding its icon.")

(defconst lispm-holon-program "holon"
  "Holon's command.")

(defconst lispm-holon-timeout 5
  "Seconds Holon's daemon has to answer a question, or to start.")

(defun lispm-holon-home ()
  "Return Holon's home directory: HOLON_HOME, else ~/.holon."
  (let ((home (getenv "HOLON_HOME")))
    (file-name-as-directory
     (expand-file-name (if (and home (not (string-empty-p home))) home "~/.holon")))))

(defun lispm-holon--file (name)
  "Return the file NAME in Holon's home directory."
  (expand-file-name name (lispm-holon-home)))

(defvar lispm-holon--cache nil
  "Memoized Holon sessions as (STAMP . SESSIONS).
STAMP is whether the daemon's socket is there, and the modification times
of Holon's database and its write-ahead log, which every change of an
agent writes.")

;;;; The daemon (pure)

(defun lispm-holon--request (method path body)
  "Return the HTTP/1.0 request asking Holon's daemon for PATH with METHOD.
BODY, when non-nil, is the JSON sent with it, a unibyte string."
  (concat method " " path " HTTP/1.0\r\nHost: localhost\r\nAccept: application/json\r\n"
          (when body
            (format "Content-Type: application/json\r\nContent-Length: %d\r\n" (length body)))
          "\r\n" body))

(defun lispm-holon--json (text)
  "Return the JSON of TEXT: objects as plists, arrays as lists, and null
and false as nil."
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun lispm-holon--utf-8 (object)
  "Return the JSON of OBJECT as a unibyte string of UTF-8, as
`json-serialize' makes it since Emacs 30."
  (let ((json (json-serialize object)))
    (if (multibyte-string-p json) (encode-coding-string json 'utf-8) json)))

(defun lispm-holon--status (response)
  "Return the status of RESPONSE, the whole of an HTTP response of Holon's
daemon, a number.  Signal an error when RESPONSE is no answer."
  (unless (string-match "\\`HTTP/[0-9.]+ \\([0-9]+\\)" response)
    (error "Holon's daemon gave no answer"))
  (string-to-number (match-string 1 response)))

(defun lispm-holon--response (response)
  "Return the JSON answer in RESPONSE, the whole of an HTTP response of
Holon's daemon, a unibyte string.  Signal an error saying what the daemon
said when it answered with an error, or when RESPONSE is no answer."
  (let* ((status (lispm-holon--status response))
         (start (string-search "\r\n\r\n" response))
         (body (if start (decode-coding-string (substring response (+ start 4)) 'utf-8) ""))
         (json (and (not (string-blank-p body)) (ignore-errors (lispm-holon--json body)))))
    (if (< status 300)
        json
      (error "Holon's daemon answered %d: %s" status
             (or (and (keywordp (car-safe json)) (plist-get json :error))
                 (string-trim body))))))

;;;; The daemon (effects)

(defun lispm-holon--ask (method path &optional body)
  "Ask Holon's daemon for PATH with METHOD and return its JSON answer.
BODY, a plist, is sent as JSON.  Signal an error when the daemon answers
with an error, does not run, or does not answer in
`lispm-holon-timeout' seconds."
  (lispm-holon--response (lispm-holon--exchange method path body)))

(defun lispm-holon--exchange (method path &optional body)
  "Ask Holon's daemon for PATH with METHOD and return its whole response,
a unibyte string.  BODY, a plist, is sent as JSON.  Signal an error when
the daemon does not run, or does not answer in `lispm-holon-timeout'
seconds."
  (let ((buffer (generate-new-buffer " *lispm-holon*" t))
        (process nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer (set-buffer-multibyte nil))
          (setq process (make-network-process
                         :name "lispm-holon" :buffer buffer :family 'local
                         :service (lispm-holon--file "run/holon.sock")
                         :coding 'binary :noquery t :sentinel #'ignore))
          (process-send-string process (lispm-holon--request method path
                                                             (and body (lispm-holon--utf-8 body))))
          (let ((deadline (+ (float-time) lispm-holon-timeout)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.05)))
          (when (process-live-p process)
            (error "Holon's daemon did not answer in %d seconds" lispm-holon-timeout))
          (with-current-buffer buffer (buffer-string)))
      (when process (delete-process process))
      (kill-buffer buffer))))

(defun lispm-holon--running-p ()
  "Return non-nil when Holon's daemon answers."
  (and (file-exists-p (lispm-holon--file "run/holon.sock"))
       (ignore-errors (lispm-holon--ask "GET" "/api/handshake") t)))

(defun lispm-holon--said (output)
  "Return what Holon said in OUTPUT: its errors, when it gave any, else
all but its log lines."
  (let* ((lines (seq-remove (lambda (line) (string-match-p "\\`[0-9-]+T[0-9:.]+Z +[A-Z]+ " line))
                            (split-string output "\n" t "[ \t]+")))
         (errors (delq nil (mapcar (lambda (line)
                                     (and (string-prefix-p "Error: " line) (substring line 7)))
                                   lines))))
    (string-join (or errors lines) "\n")))

(defun lispm-holon--unconfigured-p (said)
  "Return non-nil when SAID, what Holon said failing, is that it has no
model to work with yet."
  (string-match-p "no default model\\|provider credentials\\|holon onboard" said))

(defun lispm-holon--run (&rest args)
  "Run Holon with ARGS and return its output.
The daemon's agents are its own to change, so making, renaming and
deleting one go through Holon, and the memoized sessions are dropped.
Signal a user error with what Holon said when it fails, or, when it has
no model yet, with how to give it one."
  (let ((errors (make-temp-file "lispm-holon")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process lispm-holon-program nil (list t errors) nil args)))
            (setq lispm-holon--cache nil)
            (unless (eq status 0)
              (let ((said (lispm-holon--said
                           (concat (buffer-string) "\n"
                                   (with-temp-buffer (insert-file-contents errors)
                                                     (buffer-string))))))
                (if (lispm-holon--unconfigured-p said)
                    (user-error "Holon has no model to work with yet: M-x lispm-holon-setup sets one up")
                  (user-error "holon %s: %s" (string-join args " ") said))))
            (string-trim (buffer-string))))
      (delete-file errors))))

(defun lispm-holon--managed-p ()
  "Return non-nil when LispM runs Holon's daemon, as a service in the
sandbox: when the service `holon' has a level of its own."
  (and (fboundp 'lispm-service-managed-p) (lispm-service-managed-p 'holon)))

(defun lispm-holon--ensure-daemon ()
  "Start Holon's daemon unless it runs: the agents live in it, and Holon's
terminal UI talks to it.  It is Holon's own background service, going
on after Emacs exits; or, when LispM runs it, the service `holon', in the
sandbox, which takes the place of Holon's own if that runs."
  (let ((managed (lispm-holon--managed-p)))
    (cond (managed
           (unless (lispm-service-process 'holon)
             (message "Starting Holon's daemon in the sandbox...")
             (lispm-service-start 'holon)))
          ((not (lispm-holon--running-p))
           (message "Starting Holon's daemon...")
           (lispm-holon--run "daemon" "start")))
    ;; A daemon just started, or starting at LispM's start, answers once
    ;; it is up.
    (let ((deadline (+ (float-time) lispm-holon-timeout)))
      (while (and (not (lispm-holon--running-p)) (< (float-time) deadline))
        (sleep-for 0.1)))
    (unless (lispm-holon--running-p)
      (user-error "Holon's daemon did not start; %s says why"
                  (if managed (lispm-service-buffer-name 'holon) "`holon daemon logs'")))))

;;;; Sessions (pure)

(defun lispm-holon--time (string)
  "Return the time of STRING, an RFC 3339 timestamp, as float seconds, or nil."
  (and (stringp string)
       (ignore-errors (float-time (parse-iso8601-time-string string)))))

(defun lispm-holon--last-time (state)
  "Return when the Holon agent of STATE last did anything, float seconds,
or nil: its last turn, or the last brief it gave."
  (let* ((agent (plist-get (plist-get state :agent) :agent))
         (times (delq nil (mapcar #'lispm-holon--time
                                  (list (plist-get agent :last_brief_at)
                                        (plist-get (plist-get agent :last_turn_terminal) :completed_at)
                                        (plist-get (plist-get (plist-get state :session) :last_turn)
                                                   :completed_at))))))
    (and times (apply #'max times))))

(defun lispm-holon--project-workspaces (state)
  "Return the workspaces attached to the Holon agent of STATE but its own
home: its projects."
  (seq-remove (lambda (workspace)
                (string-prefix-p "agent_home:" (plist-get workspace :workspace_id)))
              (plist-get (plist-get state :workspace) :workspaces)))

(defun lispm-holon--directory (state)
  "Return the directory of the project the Holon agent of STATE belongs
to, or nil: the workspace it works in when that is not its own home, else
the first attached to it that is not."
  (let ((workspaces (lispm-holon--project-workspaces state)))
    (plist-get (or (seq-find (lambda (workspace) (plist-get workspace :is_active)) workspaces)
                   (car workspaces))
               :workspace_anchor)))

(defun lispm-holon--session (entry state fallback-time)
  "Return the session of a Holon agent: ENTRY as the daemon lists it,
STATE as it gives the agent's state.  Its title is the agent's name, else
its id; its directory the project it belongs to, see
`lispm-holon--directory', else home, which makes it a chat; its time
when it last did anything, else FALLBACK-TIME."
  (let ((identity (plist-get entry :identity)))
    (list :agent 'holon
          :id (plist-get identity :agent_id)
          :cwd (or (lispm-holon--directory state) (expand-file-name "~"))
          :title (or (plist-get identity :name) (plist-get identity :agent_id))
          :time (or (lispm-holon--last-time state) fallback-time))))

(defun lispm-holon--active-p (entry)
  "Return non-nil for the agent of ENTRY, as the daemon lists it, unless it
is being deleted."
  (equal (plist-get (plist-get entry :identity) :status) "active"))

(defun lispm-holon--entry-id (entry)
  "Return the id of the agent of ENTRY, as the daemon lists it."
  (plist-get (plist-get entry :identity) :agent_id))

;;;; Sessions (effects)

(defun lispm-holon--stamp ()
  "Return whether Holon's daemon has its socket, and the modification times
of Holon's database and its write-ahead log."
  (cons (file-exists-p (lispm-holon--file "run/holon.sock"))
        (mapcar (lambda (file) (and (file-exists-p file) (lispm-agents-file-time file)))
                (list (lispm-holon--file "state/runtime.sqlite")
                      (lispm-holon--file "state/runtime.sqlite-wal")))))

(defun lispm-holon--read-sessions ()
  "Return the session of every Holon agent, asked of the daemon now.
An agent that has done nothing yet is as old as its home directory."
  (mapcar (lambda (entry)
            (let ((id (lispm-holon--entry-id entry)))
              (lispm-holon--session
               entry (lispm-holon--ask "GET" (format "/api/agents/%s/state" id))
               (let ((home (lispm-holon--file (concat "agents/" id))))
                 (if (file-exists-p home) (lispm-agents-file-time home) (float-time))))))
          (seq-filter #'lispm-holon--active-p (lispm-holon--ask "GET" "/api/agents/list"))))

(defun lispm-holon--not-running-p (err)
  "Return non-nil when ERR, signalled connecting to the daemon's socket,
says that no daemon listens there: a daemon that stopped may leave its
socket behind."
  (and (eq (car err) 'file-error)
       (string-match-p "Connection refused\\|No such file" (error-message-string err))))

(defun lispm-holon-sessions ()
  "Return a session for every Holon agent, asking Holon's daemon again only
when Holon's database changed.  While the daemon does not run, there are
none; a daemon that cannot be asked lists none, and says so."
  (let ((stamp (lispm-holon--stamp)))
    (unless (and lispm-holon--cache (equal (car lispm-holon--cache) stamp))
      (setq lispm-holon--cache
            (cons stamp
                  (and (car stamp)
                       (condition-case err
                           (lispm-holon--read-sessions)
                         (error (unless (lispm-holon--not-running-p err)
                                  (message "Holon sessions unavailable: %s" (error-message-string err)))
                                nil))))))
    (cdr lispm-holon--cache)))

(defun lispm-holon--await (predicate)
  "Wait until PREDICATE holds of the agents the daemon lists, a list of
entries, for `lispm-holon-timeout' seconds at most.  The list follows a
change a moment after Holon made it; the sessions are read again after."
  (let ((deadline (+ (float-time) lispm-holon-timeout)))
    (while (and (not (ignore-errors (funcall predicate (lispm-holon--ask "GET" "/api/agents/list"))))
                (< (float-time) deadline))
      (sleep-for 0.1))
    (setq lispm-holon--cache nil)))

(defun lispm-holon--rename (session title)
  "Rename the Holon agent of SESSION to TITLE, its name; its id stays."
  (let ((id (plist-get session :id)))
    (lispm-holon--run "agent" "rename" id "--name" title)
    (lispm-holon--await
     (lambda (entries)
       (seq-some (lambda (entry)
                   (and (equal (lispm-holon--entry-id entry) id)
                        (equal (plist-get (plist-get entry :identity) :name) title)))
                 entries)))))

(defun lispm-holon--delete (session)
  "Delete the Holon agent of SESSION, with everything Holon keeps of it.
The daemon goes on deleting it in the background; meanwhile it is listed
no more."
  (let ((id (plist-get session :id)))
    (lispm-holon--run "agent" "delete" id "--yes")
    (lispm-holon--await
     (lambda (entries)
       (not (seq-some (lambda (entry)
                        (and (equal (lispm-holon--entry-id entry) id) (lispm-holon--active-p entry)))
                      entries))))))

;;;; Transcripts

(defun lispm-holon--message-text (message)
  "Return the text of MESSAGE, one sent to a Holon agent, or nil."
  (let ((body (plist-get message :body)))
    (cond ((stringp body) body)
          ((equal (plist-get body :type) "text") (plist-get body :text)))))

(defun lispm-holon--markdown (title turns messages briefs)
  "Return the transcript of the Holon agent TITLE, in Markdown.
TURNS are its conversation's turns, oldest first; for each, what a person
asked, from MESSAGES, and the briefs the agent gave back, from BRIEFS.
MESSAGES and BRIEFS are hash tables of texts by id.  What Holon itself
sent the agent is left out."
  (concat
   "# " title "\n"
   (mapconcat
    (lambda (turn)
      (concat
       (mapconcat (lambda (input)
                    (when-let* ((text (and (equal (plist-get input :presentation_class) "operator")
                                           (gethash (plist-get input :message_id) messages))))
                      (format "\n## You%s\n\n%s\n"
                              (if-let* ((time (lispm-holon--time (plist-get turn :started_at))))
                                  (format-time-string " · %F %R" time)
                                "")
                              text)))
                  (plist-get turn :inputs) "")
       (mapconcat (lambda (id)
                    (when-let* ((text (gethash id briefs)))
                      (format "\n## Holon\n\n%s\n" text)))
                  (plist-get turn :brief_ids) "")))
    turns "")))

(defconst lispm-holon-page-limit 50
  "The most pages of a conversation, a hundred turns each, a transcript
reads.")

(defun lispm-holon--turns (id)
  "Return the turns of the conversation of the Holon agent ID, oldest first."
  (let ((path (format "/api/agents/%s/conversation?limit=100" id))
        (pages 0)
        (turns nil))
    (while path
      (let ((page (lispm-holon--ask "GET" path)))
        (setq turns (append (plist-get page :turns) turns)
              pages (1+ pages)
              path (when-let* ((before (and (< pages lispm-holon-page-limit)
                                            (plist-get (plist-get page :coverage) :has_older)
                                            (plist-get page :next_before_cursor))))
                     (format "/api/agents/%s/conversation?limit=100&before=%s"
                             id (url-hexify-string before))))))
    (sort (seq-uniq turns (lambda (a b) (equal (plist-get a :turn_id) (plist-get b :turn_id))))
          :key (lambda (turn) (plist-get (plist-get turn :key) :turn_index)))))

(defun lispm-holon--texts (id kind ids text)
  "Return a hash table of the texts of the IDS of the Holon agent ID.
KIND is what they are, \"message\" or \"brief\", asked for a hundred at
a time; TEXT returns the text of one, or nil."
  (let ((table (make-hash-table :test #'equal)))
    (while ids
      (dolist (item (plist-get (lispm-holon--ask
                                "POST" (format "/api/agents/%s/%ss:batchGet" id kind)
                                (list (intern (format ":%s_ids" kind)) (vconcat (seq-take ids 100))))
                               (intern (format ":%ss" kind))))
        (when-let* ((string (funcall text item)))
          (puthash (plist-get item :id) string table)))
      (setq ids (seq-drop ids 100)))
    table))

(defun lispm-holon--transcript (session)
  "Return a file holding the transcript of the Holon agent of SESSION, in
Markdown, written to a fresh temporary file."
  (let* ((id (plist-get session :id))
         (turns (lispm-holon--turns id))
         (messages (lispm-holon--texts
                    id "message"
                    (mapcan (lambda (turn)
                              (mapcan (lambda (input)
                                        (when (equal (plist-get input :presentation_class) "operator")
                                          (list (plist-get input :message_id))))
                                      (plist-get turn :inputs)))
                            turns)
                    #'lispm-holon--message-text))
         (briefs (lispm-holon--texts id "brief"
                                     (mapcan (lambda (turn) (copy-sequence (plist-get turn :brief_ids)))
                                             turns)
                                     (lambda (brief) (plist-get brief :text))))
         (file (make-temp-file (format "holon-%s-" id) nil ".md")))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (lispm-holon--markdown (plist-get session :title) turns messages briefs)
                    nil file nil 'silent))
    file))

;;;; Starting and resuming

(defun lispm-holon--id (name taken)
  "Return an agent id made of NAME that TAKEN, a list of ids, lacks.
Holon's ids hold ASCII letters, digits, `.', `-' and `_'; any other run
of characters becomes a `-'."
  (let* ((base (string-trim (replace-regexp-in-string "[^a-z0-9._-]+" "-" (downcase name))
                            "[-.]+" "[-.]+"))
         (base (if (string-empty-p base) "agent" base))
         (id base)
         (n 1))
    (while (member id taken)
      (setq n (1+ n)
            id (format "%s-%d" base n)))
    id))

(defconst lispm-holon-id-tries 100
  "The most ids tried for a new agent.")

(defun lispm-holon--fresh-id (name)
  "Return an id made of NAME for a new Holon agent, one no agent has had.
Holon gives an agent made under the id of one deleted that one's
conversation, so an id the daemon knows of in any way, which it answers
other than not found for, is passed over too."
  (let ((taken (mapcar #'lispm-holon--entry-id (lispm-holon--ask "GET" "/api/agents/list")))
        (id nil))
    (while (progn (setq id (lispm-holon--id name taken))
                  (/= (lispm-holon--status
                       (lispm-holon--exchange "GET" (format "/api/agents/%s/state" id)))
                      404))
      (push id taken)
      (when (> (length taken) lispm-holon-id-tries)
        (error "Holon's daemon knows every id made of %s" name)))
    id))

(defun lispm-holon--select (id)
  "Have Holon's terminal UI open on the agent ID.
The UI opens on the agent it showed last, which its state file names:
ID is written there."
  (let ((file (lispm-holon--file "state/tui/local.json"))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory file) t)
    (write-region (lispm-holon--utf-8 (list :last_selected_agent_id id
                                            :updated_at (format-time-string "%FT%T.%6NZ" nil t)))
                  nil file nil 'silent)))

(defun lispm-holon--project ()
  "Return the root of the project of the main window's buffer, or nil.
New sessions start there, see `lispm-agents-new'."
  (with-selected-window (funcall lispm-agents-main-window-function)
    (when-let* ((project (project-current)))
      (directory-file-name (expand-file-name (project-root project))))))

(defun lispm-holon--new-arguments ()
  "Make a Holon agent for the project new sessions start in, and return
\(ID . ARGUMENTS) opening Holon's terminal UI on it.  The agent is named
after the project, which is attached to it as its workspace; outside
projects it is a chat, named `chat'.  `lispm-holon-template' says what
it is made from.  A project new to Holon's daemon in the sandbox starts
it again first, so that it may write there."
  (let ((project (lispm-holon--project)))
    (when (and project
               (lispm-holon--remember-workspaces (list project))
               (lispm-holon--managed-p)
               (lispm-service-process 'holon)
               (not (eq (lispm-service-level 'holon) 'none)))
      (message "Holon's daemon starts again, so that it may write %s" (abbreviate-file-name project))
      (lispm-service-restart 'holon))
    (lispm-holon--ensure-daemon)
    (lispm-holon--new-agent project)))

(defun lispm-holon--new-agent (project)
  "Make a Holon agent for PROJECT, or a chat when it is nil, and return
\(ID . ARGUMENTS) opening Holon's terminal UI on it."
  (let* ((id (lispm-holon--fresh-id (if project (file-name-nondirectory project) "chat"))))
    (apply #'lispm-holon--run "agent" "create" id
           (and lispm-holon-template (list "--template" lispm-holon-template)))
    (when project
      (lispm-holon--run "workspace" "attach" "--agent" id project))
    (lispm-holon--select id)
    (cons id (list "tui"))))

(defun lispm-holon--resume-arguments (id)
  "Return the arguments opening Holon's terminal UI on the agent ID."
  (lispm-holon--ensure-daemon)
  (lispm-holon--select id)
  (list "tui"))

;;;; Other agents' messages, and LispM's tools

(cl-defstruct (lispm-holon-mcp (:include lispm-agents-mcp)
                               (:constructor lispm-holon-mcp--make))
  "The server of a Holon terminal, which its agent reaches with
bin/lispm-tool.")

(defvar lispm-holon--served nil
  "The agents of live servers of Holon terminals, as (SERVER . ID).")

(defun lispm-holon--messaging-p ()
  "Return non-nil when LispM serves the editor's agents its tools and
messages, and LispM Agent can name a server to bin/lispm-tool."
  (and (featurep 'lispm-agents-bridge) (fboundp 'lispm-agents-mcp-announce)))

(defun lispm-holon--skill-file (id)
  "Return the file of the skill lispm among the agent ID's own skills."
  (lispm-holon--file (format "agents/%s/skills/lispm/SKILL.md" id)))

(defun lispm-holon--skill ()
  "Return the skill telling a Holon agent how to work with LispM, as Holon
reads skills: a SKILL.md."
  (let ((tool lispm-agents-mcp-tool-program))
    (concat
     "---\nname: lispm\n"
     "description: Talk with the other agents in LispM, as Claude Code, Codex and LispM's own, "
     "and use LispM's skills and tools, with the lispm-tool command.\n---\n\n"
     "# LispM\n\n"
     "You run in LispM, beside other agents: LispM's own, and those of other programs, such as "
     "Claude Code and Codex. What they send comes to you as your operator's message: "
     "[Message from NAME]; [Request ID from NAME], which NAME waits for you to answer with "
     "reply; [Reply from NAME to request ID].\n\n"
     "Reach them, and LispM's skills and tools, by running this command with ExecCommand:\n\n"
     (format "- `%s` says who you are among the agents, and lists the tools with their inputs.\n" tool)
     (format "- `%s NAME 'INPUT'` calls the tool NAME on INPUT, a JSON object; with `-` in place "
             tool)
     "of INPUT it reads the input from standard input, best for long texts.\n\n"
     "The tools:\n\n"
     "- list_agents `{}`: who is here, and whether each is busy.\n"
     "- send_message `{\"to\": NAME, \"text\": TEXT}`: tell another agent something.\n"
     "- ask_agent `{\"agent\": NAME, \"request\": TEXT}`: ask another agent for something; its "
     "reply comes to you later as a message. Naming a role starts a LispM helper of that role.\n"
     "- reply `{\"request\": ID, \"text\": TEXT}`: answer a request sent to you.\n"
     "- search_tools `{\"query\": WORDS}`, skill `{\"name\": NAME}` and call_tool "
     "`{\"name\": NAME, \"input\": {...}}`: find, load and use LispM's skills and tools. A call "
     "that changes things waits for the user's approval in LispM, which may take a while.\n\n"
     "Messages and requests are self-contained: the others cannot see your conversation. "
     "Name places as [[file:PATH::LINE]]. The command exits with 1 when the tool failed, its "
     "output saying why, and with 2 when LispM cannot be reached from here.\n")))

(defun lispm-holon--write-skill (id)
  "Give the agent ID the skill lispm, unless it has it as it is."
  (let ((file (lispm-holon--skill-file id))
        (text (lispm-holon--skill)))
    (unless (and (file-readable-p file)
                 (equal text (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))))
      (make-directory (file-name-directory file) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region text nil file nil 'silent)))))

(defun lispm-holon--context (directory &optional id)
  "Serve LispM's tools to the Holon agent ID, opened in a terminal in
DIRECTORY, while LispM runs: a server it reaches with bin/lispm-tool by
its HOLON_AGENT_ID, and the skill lispm saying how."
  (when (and id (lispm-holon--messaging-p))
    (let ((server (lispm-agents-mcp-start "holon-mcp" #'lispm-holon-mcp--make directory)))
      (condition-case err
          (progn (lispm-agents-mcp-announce server "HOLON_AGENT_ID" id)
                 (lispm-holon--write-skill id))
        (error (lispm-agents-context-stop server)
               (signal (car err) (cdr err))))
      (push (cons server id) lispm-holon--served)
      server)))

(cl-defmethod lispm-agents-context-stop ((server lispm-holon-mcp))
  "Stop SERVER, and take the skill lispm from its agent, which LispM no
longer serves."
  (when-let* ((entry (assq server lispm-holon--served)))
    (setq lispm-holon--served (delq entry lispm-holon--served))
    (unless (rassoc (cdr entry) lispm-holon--served)
      (ignore-errors
        (delete-directory (file-name-directory (lispm-holon--skill-file (cdr entry))) t))))
  (cl-call-next-method))

(defun lispm-holon--terminal-id (terminal)
  "Return the id of the Holon agent of TERMINAL; an error if unknown."
  (or (lispm-agents-terminal-session-id terminal)
      (error "%s runs no Holon agent known yet" (buffer-name terminal))))

(defun lispm-holon--deliver (terminal text)
  "Give the Holon agent of TERMINAL TEXT as a message from its operator.
Holon's daemon takes it at once: an agent at work into the turn under
way, one at rest in a turn of its own.  Return t."
  (lispm-holon--ask "POST" (format "/api/control/agents/%s/prompt"
                                   (url-hexify-string (lispm-holon--terminal-id terminal)))
                    (list :text text))
  t)

(defconst lispm-holon-resting '("asleep" "awake_idle")
  "The statuses of a Holon agent that waits for input.")

(defun lispm-holon--ready-p (terminal)
  "Return non-nil when the Holon agent of TERMINAL waits for input."
  (when-let* ((id (lispm-agents-terminal-session-id terminal))
              (state (ignore-errors
                       (lispm-holon--ask "GET" (format "/api/agents/%s/state" (url-hexify-string id))))))
    (member (plist-get (plist-get (plist-get state :agent) :agent) :status) lispm-holon-resting)))

;;;; In the sandbox

(defun lispm-holon--workspaces-file ()
  "Return the file listing the projects Holon's daemon may write in the
sandbox.  It lies where Emacs keeps its own files, which sandboxed
programs may never write."
  (expand-file-name (locate-user-emacs-file "lispm-holon-workspaces.eld")))

(defun lispm-holon--workspaces ()
  "Return the projects LispM attached to Holon's agents, which Holon's
daemon may write in the sandbox."
  (let ((file (lispm-holon--workspaces-file)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (seq-filter #'stringp (read (current-buffer))))))))

(defun lispm-holon--remember-workspaces (directories)
  "Add DIRECTORIES to the projects Holon's daemon may write in the
sandbox; return those it lacked."
  (let* ((known (lispm-holon--workspaces))
         (new (seq-uniq (seq-remove (lambda (directory) (member directory known))
                                    (mapcar (lambda (directory)
                                              (directory-file-name (expand-file-name directory)))
                                            directories)))))
    (when new
      (let ((coding-system-for-write 'utf-8-unix)
            (print-length nil)
            (print-level nil))
        (make-directory (file-name-directory (lispm-holon--workspaces-file)) t)
        (write-region (concat ";; The projects Holon's daemon may write in the sandbox, see lispm-holon.el.\n"
                              (prin1-to-string (append known new)) "\n")
                      nil (lispm-holon--workspaces-file) nil 'silent)))
    new))

(defun lispm-holon--domains (providers)
  "Return the domains of PROVIDERS, Holon's model providers as `holon
config providers list' gives them, that have credentials.  Those on this
machine are left out: the sandbox lets its programs reach this machine."
  (seq-uniq
   (mapcan (lambda (provider)
             (when-let* (((plist-get provider :credential_configured))
                         (host (ignore-errors (url-host (url-generic-parse-url
                                                         (plist-get provider :base_url)))))
                         ((not (member host '("" "localhost" "127.0.0.1" "::1" "[::1]")))))
               (cons host
                     ;; Signing in with ChatGPT renews its tokens there.
                     (and (equal (plist-get provider :id) "openai-codex")
                          (list "auth.openai.com")))))
           providers)))

(defun lispm-holon--sandbox-rules ()
  "Return what Holon's daemon needs of the sandbox on top of its level's
rules: to read and write Holon's home and the projects of
`lispm-holon--workspaces' and `lispm-holon-sandbox-allow-write', to read
its own program, and bin/lispm-tool and the files naming servers to it,
to bind its socket and local port, and to reach its model providers and
`lispm-holon-sandbox-domains'."
  (let* ((home (directory-file-name (lispm-holon-home)))
         (program (executable-find lispm-holon-program))
         (projects (append (lispm-holon--workspaces)
                           (mapcar (lambda (directory) (directory-file-name (expand-file-name directory)))
                                   lispm-holon-sandbox-allow-write)))
         (lispm (and (boundp 'lispm-agents-mcp-tool-program)
                     (list (directory-file-name
                            (file-name-directory (file-truename lispm-agents-mcp-tool-program)))
                           (directory-file-name (expand-file-name lispm-agents-mcp-peer-directory))))))
    (list :allow-read (append (list home)
                              (and program (list (directory-file-name
                                                  (file-name-directory (file-truename program)))))
                              lispm
                              projects)
          :allow-write (cons home projects)
          :unix-sockets (list (lispm-holon--file "run/holon.sock"))
          :local-binding t
          :allowed-domains (append (lispm-holon--domains
                                    (ignore-errors
                                      (lispm-holon--json (lispm-holon--run "config" "providers" "list"))))
                                   lispm-holon-sandbox-domains))))

(defun lispm-holon--before-service ()
  "Make way for Holon's daemon in the sandbox: Holon's own daemon, when it
runs, is stopped, the projects of its agents first remembered as ones
the daemon in the sandbox may write."
  (when (lispm-holon--running-p)
    (ignore-errors
      (lispm-holon--remember-workspaces
       (mapcan (lambda (entry)
                 (mapcar (lambda (workspace) (plist-get workspace :workspace_anchor))
                         (lispm-holon--project-workspaces
                          (lispm-holon--ask "GET" (format "/api/agents/%s/state"
                                                          (lispm-holon--entry-id entry))))))
               (seq-filter #'lispm-holon--active-p (lispm-holon--ask "GET" "/api/agents/list")))))
    (message "Stopping Holon's own daemon, for the one in the sandbox...")
    (lispm-holon--run "daemon" "stop")
    (let ((deadline (+ (float-time) lispm-holon-timeout)))
      (while (and (lispm-holon--running-p) (< (float-time) deadline))
        (sleep-for 0.1)))))

(defun lispm-holon-setup ()
  "Set Holon up, in a terminal: `holon onboard' asks for a model provider,
its credentials and a default model.  Holon starts no agent before."
  (interactive)
  (unless (executable-find lispm-holon-program)
    (user-error "Holon is not installed: `%s' is not on PATH" lispm-holon-program))
  (pop-to-buffer
   (let ((process-environment (lispm-agents-independent-environment process-environment)))
     (lispm-agents-terminal-make "holon onboard" lispm-holon-program "onboard"))))

;;;; The agent

(defun lispm-holon-new ()
  "Start a new Holon agent at the current project root."
  (interactive)
  (lispm-agents-new 'holon))

(lispm-agents-define
 'holon
 :name "Holon"
 ;; Holon's mark, its rings.
 :icon `(lispm-agents-image-icon ,(expand-file-name "icons/holon.png" lispm-holon-directory) nil)
 :text-icon "⊚"
 :command #'lispm-holon-new
 :key "o"
 :program lispm-holon-program
 :sessions #'lispm-holon-sessions
 :rename #'lispm-holon--rename
 :delete #'lispm-holon--delete
 :transcript #'lispm-holon--transcript
 ;; An agent's id is a name a person reads.
 :short-id #'identity
 :new-arguments #'lispm-holon--new-arguments
 :resume-arguments #'lispm-holon--resume-arguments
 :context #'lispm-holon--context
 :deliver #'lispm-holon--deliver
 :ready #'lispm-holon--ready-p)

(when (fboundp 'lispm-define-service)
  (lispm-define-service
   'holon
   :name "Holon"
   :description "Holon's daemon, where its agents live and work."
   :command (lambda () (list (or (executable-find lispm-holon-program) lispm-holon-program) "serve"))
   :directory #'lispm-holon-home
   :sandbox #'lispm-holon--sandbox-rules
   :before-start #'lispm-holon--before-service))

(defun lispm-holon-unload-function ()
  "Take Holon away from the editor's agents, and its daemon from the
services, stopping it there, as this package is unloaded."
  (lispm-agents-undefine 'holon)
  (when (fboundp 'lispm-undefine-service)
    (lispm-undefine-service 'holon))
  ;; Unloading goes on as usual.
  nil)

(provide 'lispm-holon)
;;; lispm-holon.el ends here
