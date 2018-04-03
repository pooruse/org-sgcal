(require 'alert)
(require 'json)
(require 'request-deferred)
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

(defun org-sgcal-request-authorization (client-id)
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url
   (concat org-sgcal-auth-url
           "?client_id=" (url-hexify-string )
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

(defun org-sgcal--get-events-url (cid)
  "Internal function, return calendar url by calendar id"
  (format org-sgcal-events-url cid))

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
               nil))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               data))))

(provide 'org-sgcal)

;;; org-sgcal.el ends here
