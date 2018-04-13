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

(defconst org-sgcal-request-time-format
  "%Y-%m-%dT%H:%M:%SZ"
  "time format for get event list")

(defcustom org-sgcal-timezone "Asia/Taipei"
  "Default timezone for org-sgcal"
  :group 'org-sgcal
  :type 'string) 

(defcustom org-sgcal-up-days 30
  "Number of days to get events before today."
  :group 'org-gcal
  :type 'integer)

(defcustom org-sgcal-down-days 60
  "Number of days to get events after today."
  :group 'org-gcal
  :type 'integer)

(defvar org-sgcal-token-alist nil)



;;; org-sgcal user functions
(defun org-sgcal-update-tokens ()
  "Update tokens by settings of current buffer"
  (interactive)
  (org-sgcal--update-token-alist (lambda (&rest argv)
                                   (apply #'org-sgcal-request-token argv))
                                 (lambda (&rest argv)
                                   (apply $'org-sgcal-refresh-token argv))))

(defun org-sgcal-fetch-all ()
  "Fetch all events according by settings of current buffer.
This function will erase current buffer if success."
  (interactive)
  (org-sgcal--update-level3-headlines (lambda (&rest argv)
                                        (apply #'org-sgcal-get-event-list argv))))


;;; http request functions
(defun org-sgcal-request-authorization (client-id nickname)
  "Request OAuth authorization at AUTH-URL by launching `browse-url'.
CLIENT-ID is the client id provided by the provider.
It returns the code provided by the service."
  (browse-url
   (concat org-sgcal-auth-url
           "?client_id=" (url-hexify-string client-id)
           "&response_type=code"
           "&redirect_uri=" (url-hexify-string "urn:ietf:wg:oauth:2.0:oob")
           "&scope=" (url-hexify-string org-sgcal-resource-url)))
  (read-string (concat "(" nickname ")" "Enter the code on your browser: ")))


(defun org-sgcal-request-token (client-id client-secret nickname)
  "Request OAuth access at TOKEN-URL."
  (let (data)
    (request
     org-sgcal-token-url
     :sync t
     :type "POST"
     :data `(("client_id" . ,client-id)
	     ("client_secret" . ,client-secret)
	     ("code" . ,(org-sgcal-request-authorization client-id nickname))
	     ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
	     ("grant_type" . "authorization_code"))
     :parser 'org-sgcal--json-read
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (request-response-data response))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (message (format "Error code: %s" error-thrown)))))
    data))

(defun org-sgcal-refresh-token (client-id client-secret refresh-token)
  "refresh google api auth 2 token"
  (let (data)
    (request
     org-sgcal-token-url
     :sync t
     :type "POST"
     :data `(("client_id" . ,client-id)
	     ("client_secret" . ,client-secret)
	     ("refresh_token" . ,refresh-token)
	     ("grant_type" . "refresh_token"))
     :parser 'org-sgcal--json-read
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (request-response-data response))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (message (format "Error code: %s" error-thrown)))))
    data))

(defun org-sgcal-get-event-list (cid client-secret a-token min max)
  "Get event list from calendar"
  (let (data)
    (request
     (org-sgcal--get-events-url cid)
     :sync t
     :type "GET"
     :params `(("access_token" . ,a-token)
	       ("key" . ,client-secret)
	       ("singleEvents" . "True")
	       ("orderBy" . "startTime")
	       ("timeMin" . ,min)
	       ("timeMax" . ,max)
	       ("grant_type" . "authorization_code"))
     :parser 'org-sgcal--json-read
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (request-response-data response))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (message (format "Error code: %s" error-thrown)))))
    data))

(defun org-sgcal-post-event (cid start end smry loc desc
                                 a-token client-secret
                                 &optional eid)
  "post or update event in specify calendar(depends on eid). "
  (let ((data)
        (stime (if (> (length start) 11) "dateTime" "date")))
    (request
     (concat (org-sgcal--get-events-url cid)
	     (when eid (concat "/" eid)))
     :sync t
     :type (if eid "PATCH" "POST")
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode `(("start" (,stime . ,start) ("timeZone" . ,org-sgcal-timezone))
			  ("end" (,stime ,end) ("timeZone" . ,org-sgcal-timezone))
			  ("summary" . ,smry)
			  ("location" . ,loc)
			  ("description" . ,desc)))
     :params `(("access_token" . ,a-token)
	       ("key" . ,client-secret)
	       ("grant_type" . "authorization_code"))

     :parser 'org-sgcal--json-read
     :error (cl-function
	     (lambda (&key error-thrown &allow-other-keys)
	       (message (format "Get error: %s" error-thrown))))
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (request-response-data response)))))
    data))

(defun org-sgcal-delete-event (cid eid a-token client-secret)
  "delete specify event from calendar"
  (let (out)
    (request
     (concat (org-sgcal--get-events-url cid) "/" eid)
     :sync t
     :type "DELETE"
     :headers '(("Content-Type" . "application/json"))
     :params `(("access_token" . ,a-token)
	       ("key" . ,client-secret)
	       ("grant_type" . "authorization_code"))
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (setq out t)))
     :error (cl-function
	     (lambda (&key error-thrown &allow-other-keys)
	       (message (format "Error code: %s" error-thrown)))))
    out))


;;; internal functions (testable)
(defun org-sgcal--get-events-url (cid)
  "Internal function, return calendar url by calendar id"
  (format org-sgcal-events-url cid))


;;; org-scgcal tools
(defun org-sgcal--replace-element (ele-A ele-B)
  "replace ele-A to ele-B in current buffer
ele-A must be a element exists in current buffer"
  (let ((beg-A (org-element-property :begin ele-A))
	(end-A (org-element-property :end ele-A))
	(body-B (org-element-interpret-data ele-B)))
    (goto-char beg-A)
    (delete-region beg-A end-A)
    (insert body-B)))

(defun org-sgcal--create-headline (head &optional properties contents start end)
  "head is a list which contains (title level todo-keyword)

property is a list of pair which contains key and value,
for example property can be: '((\"key1\" \"test\") (\"key2\" \"123\"))
which will shows in org document as below

:PROPERTIES:
:key1: test
:key2: 123
:END:

start/end is a list contain date scheduled/deadline which formats
as (SEC MIN HOUR DAY MON YEAR DOW DST TZ). The values are identical to those of `decode-time'.
If any values that are unknown, please set it to nil.
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
	(setq s-stamp (org-element-put-property s-stamp :year-start (nth 5 start)))
	(setq s-stamp (org-element-put-property s-stamp :month-start (nth 4 start)))
	(setq s-stamp (org-element-put-property s-stamp :day-start (nth 3 start)))
        (setq s-stamp (org-element-put-property s-stamp :hour-start (nth 2 start)))
        (setq s-stamp (org-element-put-property s-stamp :minute-start (nth 1 start)))
	(setq e-plan (org-element-put-property e-plan :scheduled s-stamp)))
      (when end
	(setq e-stamp (org-element-put-property e-stamp :type 'active))
	(setq e-stamp (org-element-put-property e-stamp :year-start (nth 5 end)))
	(setq e-stamp (org-element-put-property e-stamp :month-start (nth 4 end)))
	(setq e-stamp (org-element-put-property e-stamp :day-start (nth 3 end)))
        (setq s-stamp (org-element-put-property e-stamp :hour-start (nth 2 end)))
        (setq s-stamp (org-element-put-property e-stamp :minute-start (nth 1 end)))
	(setq e-plan (org-element-put-property e-plan :deadline e-stamp))))
    
    ;; create and set node properties
    (when properties
	(dolist (p properties)
	  (let ((node (org-element-create 'node-property)))
	    (setq node (org-element-put-property node :key (car p)))
	    (setq node (org-element-put-property node :value (cdr p)))
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

(defun org-sgcal--headline-map (level data fun &optional argv)
  "recursive type of org-element-map"
  (if (= level 0)
      (apply fun (reverse argv))
    (org-element-map data
	'headline (lambda (h)
		    (org-sgcal--headline-map
		     (1- level) (org-element-contents h) fun (cons h argv)))
	nil nil 'headline)))

(defun org-sgcal--parse-item (item)
  "parse json object from google api"
  (let ((id (cdr (assq 'id item)))
        (sumy (cdr (assq 'summary item)))
        (desc (cdr (assq 'description item)))
        (start (cdr (assq 'start item)))
        (end (cdr (assq 'end item)))
        (updated (cdr (assq 'updated item))))
    (let ((start-time
           (cond ((assq 'date start)
                  (parse-time-string (cdr (assq 'date start))))
                 ((assq 'dateTime start)
                  (decode-time (date-to-time
				(org-sgcal--convert-time-string
				 (cdr (assq 'dateTime start))))))
                 (t nil)))
          (end-time
           (cond ((assq 'date end)
                  (parse-time-string (cdr (assq 'date end))))
                 ((assq 'dateTime end)
                  (decode-time (date-to-time
				(org-sgcal--convert-time-string
				 (cdr (assq 'dateTime end))))))
                 (t nil))))
      (org-sgcal--create-headline `(,sumy ,level nil)
                                 `(("ID" . ,id) ("UPDATED" . ,updated))
                                 desc
                                 start-time
                                 end-time))))

(defun org-sgcal--parse-event-list (response-json level)
  "Parse event list from google api, get event list
to org element AVL tree
LEVEL will set to each headline"
  (let ((items (cdr (assq 'items response-json))))
    (mapcar 'org-sgcal--parse-item items)))

(defun org-sgcal--json-read ()
  (json-read-from-string
   (decode-coding-string
    (buffer-substring-no-properties (point-min) (point-max)) 'utf-8)))

(defun org-sgcal--convert-time-string (str)
  "This function will convert wikipedia ISO-8601
String to format that `data-to-time' can accept"
  (let ((case1 (string-match "+" str)))
    (cond (case1
	   (concat (substring str 0 case1)
		   (replace-regexp-in-string ":" "" str nil nil nil 19)))
	  (t str))))

(defun org-sgcal--update-token-alist (request-fun refresh-fun)
  "Update tokens by settings of current buffer"
  (let ((ele (org-element-parse-buffer)))
    (org-sgcal--headline-map
     1 ele (lambda (h1)
	     (let ((title (substring-no-properties (car (org-element-property :title h1))))
		   (client-id (substring-no-properties (org-element-property :CLIENT-ID h1)))
		   (client-secret (substring-no-properties (org-element-property :CLIENT-SECRET h1))))
               (let ((account (assq (intern title) org-sgcal-token-alist)))
                 (if account
                     (let ((rtoken (cdr (assq 'refresh_token account))))
                       (setcdr (assq 'access_token account)
			       (cdr (assq 'access_token (funcall refresh-fun client-id client-secret rtoken)))))
                   (add-to-list 'org-sgcal-token-alist
				`(,(intern title) . ,(funcall request-fun client-id client-secret title))))))))))

(defun org-sgcal--update-level3-headlines (get-events-fun)
  "Fetch all events according by settings of current buffer.
This function will erase current buffer if success."
  (interactive)
  (let ((ele (org-element-parse-buffer)))
    (org-sgcal--headline-map
     2 ele
     (lambda (h1 h2)
       (let ((title (substring-no-properties (car (org-element-property :title h1))))
	     (client-id (org-element-property :CLIENT-ID h1))
	     (client-secret (org-element-property :CLIENT-SECRET h1)))
         (let ((account (assq (intern title) org-sgcal-token-alist)))
           (if account
               (let* ((acount-data (cdr account))
                      (atoken (cdr(assq 'access_token acount-data))))
                 (let ((name (car (org-element-property :title h2)))
		       (cid (org-element-property :CALENDAR-ID h2))
                       (max (format-time-string
                             org-sgcal-request-time-format
                             (time-add (current-time) (days-to-time org-sgcal-up-days))))
                       (min (format-time-string
                             org-sgcal-request-time-format
                             (time-subtract (current-time) (days-to-time org-sgcal-down-days))))
                       (new_h2))
                   (setq new_h2 (org-sgcal--create-headline `(,name 2 nil)
                                                           `(("CALENDAR-ID" . ,cid))))
		   (setq new_h2 (apply #'org-element-adopt-elements
                                 new_h2 (org-sgcal--parse-event-list
                                         (funcall get-events-fun cid client-secret atoken min max) 3)))
                   
                   (org-element-extract-element h2)
                   (setq h1 (org-element-adopt-elements h1 new_h2)))
                 (erase-buffer)
                 (insert (org-element-interpret-data ele))
		 (org-indent-region (point-min) (point-max)))
             (message (concat " Can't find access-token for " title)))))))))

(provide 'org-sgcal)
;;; org-sgcal.el ends here
