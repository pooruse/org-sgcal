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
  '("%Y-%m-%d" .  "%Y-%m-%dT%H:%M:%SZ")
  "time format for get event list")

(defconst org-sgcal-request-time-out
  10
  "timeout for http requests")

(defconst org-sgcal--version
  "1.0.2"
  "version for org-sgcal")

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
(defvar org-sgcal-error-string nil)

(defconst org-sgcal-error-plist
  `(:httpErr400 "Request body data format error"
		:httpErr401 "Access token expired, please run org-sgcal-update-tokens again"
		:headingFormatErr ,(concat "The minimun requirement of heading is title and :SCHEDULD\n"
					   "which formats <YY-MM-XX>")
		:startDateTimeWithoutEnd ,(concat "If your :SCHEDULED in your headind formats <YY-MM-XX> W HH:MM.\n"
						  "You should add an :DEADLINE to it with same format")
		:deleteErr "No events it in properties of this heading"
		:httpErr "Http error code: %s"
		:notokenErr ,(concat "Can't find token for \"%s\" "
                                    "Please run org-sgcal-update-tokens")
		:requestTokenErr "Fail on request token for \"%s\". Error code is %s."
		:refreshTokenErr "Fail on refresh token for \"%s\". Error code is %s."
		:tokenHeadingFormatErr "Fail because heading \"%s\" did not contain client-id or client-secret"
                :applyAtPointErr "Point is not on any heading."
		:noApiHeadingErr "Please give a name to your api heading"
		:noCalHeadingErr "Pleave give a name to your canlendar heading"
                :unknownHttpErr ,(concat "Unknown http error, maybe you can run "
                                        "org-sgcal-update-tokens and run again.")
                :eid404Err "Can't find events with title \"%s\".".)
  "This list contains all error could happend in sgcal")


;;; maybe class
(defun maybe-error-make (reason)
  "create a maybe-error class with error code"
  (cons 'maybe-error reason))

(defun maybe-error-get (maybe-err)
  "Get error message from maybe-err"
  (let ((key (car maybe-err))
	(val (cdr maybe-err)))
    (if (eq key 'maybe-error) val nil)))

(defun maybe-error-string (err)
  "Translate error simbol to string and show it to user "
  (cond
   ((listp err)
    (apply #'format
	   (plist-get org-sgcal-error-plist (car err))
	   (cdr err)))
   (err (plist-get org-sgcal-error-plist err))
   (t "Unknown Error Happened")))

(defun maybe-error-flatten (var)
  "Flatten nested maybe-error list,
For example '(((maybe-error a) (maybe-error b)) (maybe-error c)))
will become '((maybe-error a) (maybe-error b) (maybe-error c)))"
  (let ((tmp))
    (letrec ((_flatten
	      (lambda (_var)
		(cond ((eq (car _var) 'maybe-error) (setq tmp (cons _var tmp)))
		      ((consp (car _var)) 
		       (progn (funcall _flatten (cdr _var))
			      (funcall _flatten (car _var)))))
		)))
      (funcall _flatten var))
  tmp))

(defun maybe-make (val)
  "contructor for maybe"
  (cons 'maybe val))


(defun maybe-get (maybe-val)
  "get value in maybe"
  (let ((key (car maybe-val))
	(val (cdr maybe-val)))
    (if (eq key 'maybe) val nil)))

(defun maybe-map (maybe-val maybe-fun)
  "apply function with maybe-val, 
and package it to maybe"
  (let ((argv (maybe-get maybe-val)))
    (if argv 
	(maybe-make (funcall maybe-fun argv))
      maybe-val)))

(defun maybe-flatmap (maybe-val maybe-fun)
  "apply function with maybe-val 
and return the result"
  (let ((argv (maybe-get maybe-val)))
    (if argv 
	(funcall maybe-fun argv)
      maybe-val)))

;;; org-sgcal user functions
(defun org-sgcal-version ()
  "Get org-sgcal-version"
  (interactive)
  (message org-sgcal--version))

(defun org-sgcal-update-tokens ()
  "Update tokens by settings of current buffer"
  (interactive)
  (let ((err-list (org-sgcal--update-token-alist
	      (lambda (&rest argv)
		(apply #'org-sgcal-request-token argv))
	      (lambda (&rest argv)
		(apply #'org-sgcal-refresh-token argv)))))
    (dolist (err err-list)
      (let ((err-type (maybe-error-get err)))
	(if err-type
	    (message (maybe-error-string err-type))
	  (message "Update tokens success"))))))

(defun org-sgcal-clear-tokens ()
  "set org-sgcal-token-alist to nil"
  (interactive)
  (setq org-sgcal-token-alist nil))

(defun org-sgcal-fetch-all ()
  "Fetch all events according by settings of current buffer.
This function will erase current buffer if success."
  (interactive)
  (let ((err-list (org-sgcal--update-level3-headlines (lambda (&rest argv)
							(apply #'org-sgcal-get-event-list argv)))))
    (dolist (err (maybe-error-flatten err-list))
      (let ((err-type (maybe-error-get err)))
	(if err-type
	    (message (maybe-error-string err-type)))))))

(defun org-sgcal-post-at-point ()
  "Post or update events at point"
  (interactive)
  (let ((err (maybe-error-get (org-sgcal--apply-and-update-at-point #'org-sgcal-post-event))))
    (if err
	(message (maybe-error-string err))
      (message "Post success"))))

(defun org-sgcal-delete-at-point ()
  "Delete event at point if available"
  (interactive)
  (let ((err (maybe-error-get (org-sgcal--delete-at-point-and-apply #'org-sgcal-delete-event #'y-or-n-p))))
    (if err
	(message (maybe-error-string err))
      (message "Delete Success"))))

(defun org-sgcal-update-by-tag ()
  "You can tag each head to :update: to update it or
 :delete: to delete it"
  (interactive)
  (let ((err-list (org-sgcal--apply-at-tags #'org-sgcal-post-event #'org-sgcal-delete-event)))
    (dolist (err (maybe-error-flatten err-list))
      (let ((err-type (maybe-error-get err)))
	(if err-type
	    (message (maybe-error-string err-type))
	  (message "update by tag success"))))))

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
  (let ((data (maybe-error-make :unknownHttpErr)))
    (request
     org-sgcal-token-url
     :sync t
     :timeout org-sgcal-request-time-out
     :type "POST"
     :data `(("client_id" . ,client-id)
	     ("client_secret" . ,client-secret)
	     ("code" . ,(org-sgcal-request-authorization client-id nickname))
	     ("redirect_uri" .  "urn:ietf:wg:oauth:2.0:oob")
	     ("grant_type" . "authorization_code"))
     :parser 'org-sgcal--json-read
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (maybe-make (request-response-data response)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (setq data (maybe-error-make (:requestTokenErr nickname error-thrown))))))
    data))

(defun org-sgcal-refresh-token (client-id client-secret refresh-token nickname)
  "refresh google api auth 2 token"
  (let ((data (maybe-error-make :unknownHttpErr)))
    (request
     org-sgcal-token-url
     :sync t
     :timeout org-sgcal-request-time-out
     :type "POST"
     :data `(("client_id" . ,client-id)
	     ("client_secret" . ,client-secret)
	     ("refresh_token" . ,refresh-token)
	     ("grant_type" . "refresh_token"))
     :parser 'org-sgcal--json-read
     :success (cl-function
	       (lambda (&key response &allow-other-keys)
		 (setq data (maybe-make (request-response-data response)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (setq data (maybe-error-make (:refreshTokenErr nickname error-thrown))))))
    data))

(defun org-sgcal-get-event-list (cid a-token client-secret min max)
  "Get event list from calendar"
  (let ((data (maybe-error-make :unknownHttpErr)))
    (request
     (org-sgcal--get-events-url cid)
     :sync t
     :timeout org-sgcal-request-time-out
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
		 (setq data (maybe-make (request-response-data response)))))
     :error
     (cl-function (lambda (&key error-thrown &allow-other-keys)
		    (setq data (mabe-error `(:httpErr ,error-thrown))))))
    data))

(defun org-sgcal-post-event (cid a-token client-secret eid
				 start end smry loc desc color-id)
  "post or update event in specify calendar(depends on eid). "
  (let ((data (maybe-error-make :unknownHttpErr))
        (stime (if (> (length start) 11) "dateTime" "date")))
    (cond ((and (equal stime "dateTime") (null end))
           (maybe-error-make :startDateTimeWithoutEnd))
          ((and (equal stime "dateTime") (< (length end) 11))
           (maybe-error-make :startDateTimeWithoutEnd))
          (t
           (progn 
             (request
              (concat (org-sgcal--get-events-url cid)
                      (when eid (concat "/" eid)))
              :sync t
              :timeout org-sgcal-request-time-out
              :type (if eid "PATCH" "POST")
              :headers '(("Content-Type" . "application/json"))
              :data (json-encode `(("start" (,stime . ,start) ("timeZone" . ,org-sgcal-timezone))
                                   ("end" (,stime ,end) ("timeZone" . ,org-sgcal-timezone))
                                   ("summary" . ,smry)
                                   ("location" . ,loc)
                                   ("description" . ,desc)
                                   ("colorId" . ,color-id)))
              :params `(("access_token" . ,a-token)
                        ("key" . ,client-secret)
                        ("grant_type" . "authorization_code"))

              :parser 'org-sgcal--json-read
              :error (cl-function
                      (lambda (&key error-txhrown &allow-other-keys)
                        (setq data (maybe-error-make `(:httpErr ,error-thrown)))))
              :success (cl-function
                        (lambda (&key response &allow-other-keys)
                          (cond ((eq (request-response-status-code response) 404)
                                 (setq data (maybe-error-make `(:eid404Err ,smry))))
                                (t
                                 (setq data (maybe-make (request-response-data response)))))))
              )
             data)))))

(defun org-sgcal-delete-event (cid a-token client-secret eid)
  "delete specify event from calendar"
  (if eid
      (let ((out (maybe-error-make :unknownHttpErr)))
	(request
	 (concat (org-sgcal--get-events-url cid) "/" eid)
	 :sync t
         :timeout org-sgcal-request-time-out
	 :type "DELETE"
	 :headers '(("Content-Type" . "application/json"))
	 :params `(("access_token" . ,a-token)
		   ("key" . ,client-secret)
		   ("grant_type" . "authorization_code"))
	 :success (cl-function
		   (lambda (&key data &allow-other-keys)
		     (setq out (maybe-make t))))
	 :error (cl-function
		 (lambda (&key error-thrown &allow-other-keys)
		   (setq out (maybe-error-make `(:httpErr ,error-thrown))))))
	out)
    (maybe-error-make :deleteErr)))


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
    (insert body-B)
    (cons beg-A (+ beg-A (length body-B)))))

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
        
        (let ((node (org-element-create 'node-property))
              (key (car p))
              (value (cdr p)))
          (setq node (org-element-put-property node :key key))
          (setq node (org-element-put-property node :value value))
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
		     (1- level)
		     (org-element-contents h)
		     fun
		     (cons h argv)))
	nil nil 'headline)))

(defun org-sgcal--parse-item (item level)
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
    (mapcar (lambda (item) (org-sgcal--parse-item item level)) items)))

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
		   (_client-id (org-element-property :CLIENT-ID h1))
		   (_client-secret (org-element-property :CLIENT-SECRET h1)))
	       (if (and _client-id _client-secret)
		   (let ((account (assq (intern title) org-sgcal-token-alist))
			 (client-id (substring-no-properties _client-id))
			 (client-secret (substring-no-properties _client-secret)))
		     (if (equal title "")
			 (maybe-error-make :noApiHeadingErr)
		       (if account
			   (let*  ((rtoken (cdr (assq 'refresh_token account)))
				   (refresh-ret
				    (funcall refresh-fun client-id client-secret rtoken title)))
			     (maybe-map refresh-ret
					(lambda (_res)
                                          (let ((res _res))
                                            (setcdr (assq 'access_token account)
                                                    (cdr (assq 'access_token res)))
                                            (setcdr (assq 'expires_in account)
                                                    (time-add (current-time) (seconds-to-time (cdr (assq 'expires_in res)))))
                                            res))))
			 (let ((request-ret (funcall request-fun client-id client-secret title)))
			   (maybe-map request-ret
				      (lambda (_res_req)
                                        (let ((res _res_req))
                                          (setcdr (assq 'expires_in res)
                                                  (time-add (current-time) (seconds-to-time (cdr (assq 'expires_in res)))))
                                          (cl-pushnew `(,(intern title) . ,res)
                                                      org-sgcal-token-alist)
                                          res)))))))
		 (maybe-error-make `(:tokenHeadingFormatErr ,title))))))))

(defun org-sgcal--update-level3-headlines (get-events-fun)
  "Fetch all events according by settings of current buffer.
This function will erase current buffer if success."
  (let ((ele (org-element-parse-buffer)))
    (let ((res-list 
	   (org-sgcal--headline-map
	    2 ele
	    (lambda (h1 h2)
	      (let ((title (substring-no-properties (car (org-element-property :title h1))))
		    (client-id (org-element-property :CLIENT-ID h1))
		    (client-secret (org-element-property :CLIENT-SECRET h1)))
		(let ((atoken (org-sgcal--get-atoken title))
                      (name (car (org-element-property :title h2)))
                      (cid (org-element-property :CALENDAR-ID h2))
                      (color-id (org-element-property :COLOR-ID h2))
                      (max (convert-time-to-string
                            (decode-time
                             (time-add
                              (current-time)
                              (days-to-time org-sgcal-up-days)))))
                      (min (convert-time-to-string
                            (decode-time
                             (time-subtract
                              (current-time)
                              (days-to-time org-sgcal-down-days)))))
                      (new_h2))
                  (cond ((equal "" name) (maybe-error-make :noCalHeadingErr))
                        ((equal "" title) (maybe-error-make :noApiHeadingErr))
                        ((null atoken) (maybe-error-make `(:notokenErr ,title)))
                        (t
                         (maybe-map (funcall get-events-fun cid atoken client-secret min max)
                                    (lambda (res)
                                      (setq new_h2
                                            (org-sgcal--create-headline
                                             `(,name 2 nil)
                                             (cons `("CALENDAR-ID" . ,cid)
                                                   (when color-id `(("COLOR-ID" . ,color-id))))))
                                      (setq new_h2
                                            (apply #'org-element-adopt-elements
                                                   new_h2
                                                   (org-sgcal--parse-event-list
                                                    res 3)))
                                      (org-element-extract-element h2)
                                      (setq h1 (org-element-adopt-elements h1 new_h2))
                                      res))))))))))
      (erase-buffer)
      (insert (org-element-interpret-data ele))
      (org-indent-region (point-min) (point-max))
      res-list)))

(defun org-sgcal--search-up (&optional argv)
  "this function will search property's and return it as plist
when the point is at headline level 3, 
it will try to find id and updated 

when the point is at headline level 2, 
it will try to find cid and color-id 

when the point is at headline headline level 1,
it will try to find client-id client-secret.
this function will also search up by `outline-up-headling'.

For example. => means where is your point

Example 1

* head1
:PROPERTIES:
:CLIENT-SECRET: test-client-secret
:CLIENT-ID: test-client-id
:END:

** head2
:PROPERTIES:
:COLOR-ID: (TODO 1 DONE 2)
:CALENDAR-ID: test-cid
:END:

=> *** head3
DEADLINE: <2018-02-28 三 18:00> SCHEDULED: <2018-02-28 三 17:00>
:PROPERTIES:
:ID: test-id
:UPDATED: test-updated
:END:

returns 
 (:name head3 :id test-id :updated test-updated 
 :start (nil 0 17 28 2 2018 nil)
 :end (nil 0 18 28 2 2018 nil)
 :color-id (TODO 1 DONE 2)
 :title head1
 :client-secret test-client-secret
 :client-id test-client-id)

Example 2

* head1
:PROPERTIES:
:CLIENT-SECRET: test-client-secret
:CLIENT-ID: test-client-id
:END:

=> *** head3
:PROPERTIES:
:ID: test-id
:UPDATED: test-updated
:END:

returns 
 (:name head3 :id test-id :updated test-updated 
 :title head1
 :client-secret test-client-secret
 :client-id test-client-id)

note: 
1. if your point is not at headline,
this function will return nil immediately
2. by side-effect the point will be changed
according to the search. 
"
  (if (org-at-heading-p)
      (let ((here (org-element-at-point))
	    (newargv argv))
	(let ((level (org-element-property :level here)))
	  (cond ((= level 3)
		 (progn
		   (setq newargv
			 (plist-put newargv :name
				    (substring-no-properties
				     (org-element-property :title here))))
		   (setq newargv
			 (plist-put newargv :todo
				    (let ((todo-keyword (org-element-property :todo-keyword here)))
				      (if todo-keyword
					  (substring-no-properties todo-keyword)
					nil))))
		   (setq newargv
			 (plist-put newargv :start
				    (let ((stamp (org-element-property :scheduled here)))
				      (if stamp
					  `(0
					    ,(org-element-property :minute-start stamp)
					    ,(org-element-property :hour-start stamp)
					    ,(org-element-property :day-start stamp)
					    ,(org-element-property :month-start stamp)
					    ,(org-element-property :year-start stamp)
					    nil)
					nil)
				      )))
		   (setq newargv
			 (plist-put newargv :end
				    (let ((stamp (org-element-property :deadline here)))
				      (if stamp
					  `(0
					    ,(org-element-property :minute-start stamp)
					    ,(org-element-property :hour-start stamp)
					    ,(org-element-property :day-start stamp)
					    ,(org-element-property :month-start stamp)
					    ,(org-element-property :year-start stamp)
					    nil)
					nil)
				      )))
		   (setq newargv (plist-put newargv :id (org-element-property :ID here)))
		   (setq newargv (plist-put newargv :updated (org-element-property :UPDATED here)))
		   (setq newargv (plist-put newargv :contents
					    (let ((end-position (org-element-property :end here)))
					      (goto-char (1- end-position))
					      (let ((para (org-element-at-point)))
						(if (eq 'paragraph (org-element-type para))
						    (buffer-substring (org-element-property :begin para)
								      (org-element-property :end para))
						  nil)))))
		   (outline-up-heading 1)
		   (org-sgcal--search-up newargv)))
		((= level 2)
		 (progn
		   (setq newargv
			 (plist-put newargv :color-id
				    (let ((color-id-plist (org-element-property :COLOR-ID here)))
				      (if color-id-plist
					  (car (read-from-string (org-element-property :COLOR-ID here)))
					nil))))
		   (setq newargv (plist-put newargv :cid (org-element-property :CALENDAR-ID here)))
		   (outline-up-heading 1)
		   (org-sgcal--search-up newargv)))
		((= level 1)
		 (progn
		   (setq newargv
			 (plist-put newargv :title
				    (substring-no-properties
				     (org-element-property :title here))))
		   (setq newargv (plist-put newargv :client-id (org-element-property :CLIENT-ID here)))
		   (setq newargv (plist-put newargv :client-secret (org-element-property :CLIENT-SECRET here)))
		   newargv))
		(t nil)))) nil))

(defun org-sgcal--apply-at-point (fun)
  "apply fun at point
this function should format like
 (defun some-fun (title properties-plist))"
  (let ((props (save-excursion
                 (when (not (org-at-heading-p))
                   (org-previous-visible-heading 1))
                 (org-sgcal--search-up))))
    (if props
        (let ((name (plist-get props :name))
              (todo (plist-get props :todo))
              (start (plist-get props :start))
              (end (plist-get props :end))
              (desc (plist-get props :contents))
              (eid (plist-get props :id))
              (updated (plist-get props :updated))
              (cid (plist-get props :cid))
              (color-id (plist-get props :color-id))
              (client-secret (plist-get props :client-secret))
              (title (plist-get props :title)))
          
          (let ((atoken (org-sgcal--get-atoken title))
                (smry (concat (when todo (concat todo " ")) name))
                (start-date (if start (convert-time-to-string start)))
                (end-date (cond (end (convert-time-to-string end))
                                 ((not start) nil)
                                 ((not (nth 2 start))
                                  (convert-time-to-string
                                   `(nil nil nil ,(1+ (nth 3 start))
					 ,(nth 4 start)
					 ,(nth 5 start)
					 nil))))))
            (cond
             ((null atoken) (maybe-error-make `(:notokenErr ,title)))
             ((and cid
                   atoken
                   client-secret
                   start
                   (not (equal "" name)))
              (funcall
               fun cid atoken client-secret
               eid start-date end-date
               smry nil desc
               (if todo (plist-get color-id (intern todo)) nil)))
             (t (maybe-error-make :headingFormatErr)))))
      (maybe-error-make :applyAtPointErr))))

(defun org-sgcal--delete-at-point-and-apply (delete-request-fun ask-fun)
  "run funtion at point if available. if apply success,
delete heading at point"
  (maybe-flatmap
   (org-sgcal--apply-at-point
    (lambda (cid a-token client-secret eid
		 start end smry loc desc color-id)
      (if (and cid a-token client-secret eid)
	  (when (funcall
		 ask-fun
		 (format "Do you really want delete event?\n%s\n" smry))
	    (funcall delete-request-fun cid a-token client-secret eid)))))
   (lambda (head)
     (when (not (org-at-heading-p))
       (org-previous-visible-heading 1))
     (let ((here (org-element-at-point)))
       (delete-region (org-element-property :begin here)
		      (org-element-property :end here))))))

(defun convert-time-to-string (date-time)
  "date-time is a list which format is the same
as `decode-time' return"
  (if (nth 2 date-time)
      (format-time-string
       
       (cdr org-sgcal-request-time-format)
       (apply #'encode-time
              date-time))
    (progn
      (format-time-string
       (car org-sgcal-request-time-format)
       (apply #'encode-time
	      `(0 0 0
		  ,(nth 3 date-time)
		  ,(nth 4 date-time)
		  ,(nth 5 date-time)
		  nil))))))

(defun org-sgcal--apply-and-update-at-point (post-fun)
  "Apply update heading at point (if success)"
  (maybe-map
   (org-sgcal--apply-at-point post-fun)
   (lambda (ret)
     (when (not (org-at-heading-p))
       (org-previous-visible-heading 1))
     (let ((here (org-element-at-point)))
       (org-sgcal--replace-element
	here (org-sgcal--parse-item ret 3)))
     (org-previous-visible-heading 1)
     (let ((here (org-element-at-point)))
       (if here
	   (org-indent-region
	    (org-element-property :begin here)
	    (org-element-property :end here))))
     ret)))

(defun org-sgcal--apply-at-tags (update-fun delete-fun)
  "Apply update-fun on heading with :UPDATE: tag.
apply delete-fun on heading with :DELETE: tag"
  (let ((ele (org-element-parse-buffer))
        (update-tag "UPDATE")
        (delete-tag "DELETE"))
    (let ((res-list 
	   (org-sgcal--headline-map
	    3 ele
	    (lambda (h1 h2 h3)
	      (let* ((title (substring-no-properties (car (org-element-property :title h1))))
		    (client-id (org-element-property :CLIENT-ID h1))
		    (client-secret (org-element-property :CLIENT-SECRET h1))
                    (atoken (org-sgcal--get-atoken title))
                    (cname (car (org-element-property :title h2)))
                    (cid (org-element-property :CALENDAR-ID h2))
                    
                    (color-id (org-element-property :COLOR-ID h2))
                    (eid (org-element-property :ID h3))
                    (todo (let  ((_todo (org-element-property :todo-keyword h3)))
                            (if _todo
                                (substring-no-properties _todo)
                              nil)))
                    
                    (ename (substring-no-properties (car (org-element-property :title h3))))
                    (start (let ((stamp (org-element-property :scheduled h3)))
                             (if stamp
                                 `(0
                                   ,(org-element-property :minute-start stamp)
                                   ,(org-element-property :hour-start stamp)
                                   ,(org-element-property :day-start stamp)
                                   ,(org-element-property :month-start stamp)
                                   ,(org-element-property :year-start stamp)
                                   nil)
                               nil)))
                    
                    (end (let ((stamp (org-element-property :deadline h3)))
				      (if stamp
					  `(0
					    ,(org-element-property :minute-start stamp)
					    ,(org-element-property :hour-start stamp)
					    ,(org-element-property :day-start stamp)
					    ,(org-element-property :month-start stamp)
					    ,(org-element-property :year-start stamp)
					    nil)
					nil)
				      ))
                    (desc (car (org-element-map h3 'paragraph
                                 (lambda (p) 
                                   (org-element-contents p)) nil t)))
                    
                    (updated (org-element-property :UPDATED h3))
                    
                    (smry (concat (when todo (concat todo " ")) ename))
                    (start-date (if start (convert-time-to-string start)))
                    (end-date (cond (end (convert-time-to-string end))
                                    ((not start) nil)
                                    ((not (nth 2 start))
                                     (convert-time-to-string
                                      `(nil nil nil ,(1+ (nth 3 start))
                                            ,(nth 4 start)
                                            ,(nth 5 start)
                                            nil)))))
                    
                    (tags (org-element-property :tags h3))
                    (tag (letrec
                             ((_find-tag (lambda (_tags)
                                           (let ((_tag (car _tags)))
                                             (cond ((null _tag) nil)
                                                   ((equal update-tag (substring-no-properties _tag)) update-tag)
                                                   ((equal delete-tag (substring-no-properties _tag)) delete-tag)
                                                   (t (funcall _find-tag (cdr _tags))))))))
                           (funcall _find-tag tags))))
                (cond
                 ((null tag) nil)
                 
                 ((equal "" cname) (maybe-error-make :noCalHeadingErr))
                 ((equal "" title) (maybe-error-make :noApiHeadingErr))
                 ((null atoken) (maybe-error-make `(:notokenErr ,title)))
                 ((or (null cid)
                      (null atoken)
                      (null client-secret)
                      (null start)
                      (equal ename "")) (maybe-error-make :headingFormatErr))
                 ((equal tag update-tag)
                  (maybe-map
                   (funcall
                    update-fun cid atoken client-secret
                    eid start-date end-date
                    smry nil desc
                    (cond ((null todo) nil)
                          ((null color-id) nil)
                          (t (plist-get (car (read-from-string color-id)) todo))))
                   (lambda (item)
                     (org-element-set-element h3 (org-sgcal--parse-item item 3))
                     t)))
                 ((equal tag delete-tag)
                  (maybe-map
                   (funcall delete-fun cid atoken client-secret eid)
                   (lambda (ret)
                     (org-element-extract-element h3)
                     )))))))))
      (erase-buffer)
      (insert (org-element-interpret-data ele))
      (org-indent-region (point-min) (point-max))
      res-list)))

(defun org-sgcal--get-atoken (title)
  "Get atoken from token-alist"
  (let* ((account (assq (intern title) org-sgcal-token-alist))
         (atoken (cdr (assq 'access_token account)))
         (expires_in (cdr (assq 'expires_in account))))
    (cond
     ((null account) nil)
     ((null expires_in) nil)
     ((time-less-p (current-time) expires_in) atoken)
     (t nil))))

(provide 'org-sgcal)
;;; org-sgcal.el ends here
