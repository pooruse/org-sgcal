(require 'json)
(require 'request)
(require 'org-element)
(require 'org-archive)
(require 'cl-lib)


(defconst org-sgcal-auth-url
  "https://accounts.google.com/o/oauth2/auth"
  "Google OAuth2 server URL.")

(defconst org-sgcal-token-url
  "https://www.googleapis.com/oauth2/v3/token"
  "Google OAuth2 server URL.")

(defconst org-sgcal-resource-url
  "https://www.googleapis.com/auth/calendar"
  "URL used to request access to calendar resources.")

(defconst org-sgcal-events-url
  "https://www.googleapis.com/calendar/v3/calendars/%s/events"
  "URL for event management")

(defcustom org-sgcal-timezone "Asia/Taipei"
  "Default timezone for org-sgcal"
  :group 'org-sgcal
  :type 'string) 

(defvar org-sgcal-token-plist nil)

(defun org-sgcal-refresh-token ()
  "refresh token and store tokens into `org-sgcal-token-list'. "
  (let ((ele-buffer (org-element-parse-buffer)))
    (org-element-map)))



;;; http request functions
(defun org-sgcal-request-authorization (client-id)
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url
   (concat org-sgcal-auth-url
           "?client_id=" (url-hexify-string)
           "&response_type=code"
           "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
           "&scope=" (url-hexify-string org-sgcal-resource-url)))
  (read-string "Enter the code your browser displayed: "))


(defun org-sgcal-request-token (client-id client-secret)
  "Request OAuth access at TOKEN-URL."
  (request
   org-sgcal-token-url
   :type "POST"
   :data `(("client_id" . ,client-id)
           ("client_secret" . ,client-secret)
           ("code" . ,(org-sgcal-request-authorization))
           ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
           ("grant_type" . "authorization_code"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               data))
   :error
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown)))))

(defun org-sgcal-get-event-list (cid a-token min max)
  "Get event list from calendar"
  (request
   (org-sgcal--get-events-url cid)
   :type "GET"
   :params `(("access_token" . ,a-token)
             ("key" . ,org-gcal-client-secret)
             ("singleEvents" . "True")
             ("orderBy" . "startTime")
             ("timeMin" . ,min)
             ("timeMax" . ,max)
             ("grant_type" . "authorization_code"))
   :parser 'json-read
   :error
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Got error: %S" error-thrown)
                  nil))))

(defun org-sgcal-post-event (cid start end smry loc desc
                                 a-token client-secret
                                 &optional eid)
  "post or update event in specify calendar(depends on eid). "
  (request
   (concat (org-sgcal--get-events-url cid)
           (when eid (concat "/" eid)))
   :type (if id "PATCH" "POST")
   :headers '(("Content-Type" . "application/json"))
   :data (json-encode `(("start" ("dateTime" . ,start) ("timeZone" . ,org-sgcal-timezone))
                        ("end" ("dateTime" ,end) ("timeZone" . ,org-sgcal-timezone))
                        ("summary" . ,smry)
                        ("location" . ,loc)
                        ("description" . ,desc)))
   :params `(("access_token" . ,a-token)
             ("key" . ,client-secret)
             ("grant_type" . "authorization_code"))

   :parser 'json-read
   :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (message "Got error: %S" error-thrown)
               nil))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               data))))

(defun org-sgcal-delete-event (cid eid a-token client-secret)
  "delete specify event from calendar"
  (request
   (concat (org-sgcal--get-events-url cid) "/" eid)
   :type "DELETE"
   :headers '(("Content-Type" . "application/json"))
   :params `(("access_token" . ,a-token)
             ("key" . ,client-secret)
             ("grant_type" . "authorization_code"))
   :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (message "Got error: %S" error-thrown)
               nil))))



;;; internal functions
(defun org-sgcal--get-events-url (cid)
  "Internal function, return calendar url by calendar id"
  (format org-sgcal-events-url cid))


;;; org-scgcal tools
(defun org-sgcal-replace-element (ele-A ele-B)
  "replace ele-A to ele-B in current buffer
ele-A must be a element exists in current buffer"
  (let ((beg-A (org-element-property :begin ele-A))
	(end-A (org-element-property :end ele-A))
	(body-B (org-element-interpret-data ele-B)))
    (goto-char beg-A)
    (delete-region beg-A end-A)
    (insert body-B)))

(defun org-sgcal-create-headline (head &optional properties contents start end)
  "head is a list which contains (title level todo-keyword)

property is a list of pair which contains key and value,
for example property can be: '((\"key1\" \"test\") (\"key2\" \"123\"))
which will shows in org document as below

:PROPERTIES:
:key1: test
:key2: 123
:END:

start/end is a list contain date scheduled/deadline which formats as (year month day)
contents is org struct text below property drawer
"
  (let ((e-head (org-element-create 'headline))
	(e-sect (org-element-create 'section))
	(e-draw (org-element-create 'property-drawer))
	(e-para (org-element-create 'paragraph))
	(e-plan (org-element-create 'planning)))
    ;; set headline
    (let ((title (nth 0 head))
	  (level (nth 1 head))
	  (todok (nth 2 head)))
      (setq e-head (org-element-put-property e-head :title title))
      (setq e-head (org-element-put-property e-head :level level))
      (when todok
	(setq e-head (org-element-put-property e-head :todo-keyword todok))))

    ;; set planning
    (let ((s-stamp (if start (org-element-create 'timestamp) nil))
	  (e-stamp (if end (org-element-create 'timestamp) nil)))
      (when start
	(setq s-stamp (org-element-put-property s-stamp :type 'active))
	(setq s-stamp (org-element-put-property s-stamp :year-start (nth 0 start)))
	(setq s-stamp (org-element-put-property s-stamp :month-start (nth 1 start)))
	(setq s-stamp (org-element-put-property s-stamp :day-start (nth 2 start)))
	(setq e-plan (org-element-put-property e-plan :scheduled s-stamp)))
      (when end
	(setq e-stamp (org-element-put-property e-stamp :type 'active))
	(setq e-stamp (org-element-put-property e-stamp :year-start (nth 0 end)))
	(setq e-stamp (org-element-put-property e-stamp :month-start (nth 1 end)))
	(setq e-stamp (org-element-put-property e-stamp :day-start (nth 2 end)))
	(setq e-plan (org-element-put-property e-plan :deadline e-stamp))))
    
    ;; create and set node properties
    (when properties
	(dolist (p properties)
	  (let ((node (org-element-create 'node-property)))
	    (setq node (org-element-put-property node :key (car p)))
	    (setq node (org-element-put-property node :value (cadr p)))
	    (setq e-draw (org-element-adopt-elements e-draw node)))))

    ;; set paragraph
    (when contents
      (setq e-para (org-element-set-contents e-para contents)))

    ;; set section
    (setq e-sect (org-element-adopt-elements e-sect e-plan))
    (when properties
      (setq e-sect (org-element-adopt-elements e-sect e-draw)))
    (setq e-sect (org-element-adopt-elements e-sect e-para))
    (setq e-head (org-element-adopt-elements e-head e-sect))
    e-head))

(provide 'org-sgcal)
;;; org-sgcal.el ends here
