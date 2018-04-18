(defun test-org-element ()
  "print current buffer org struct in *scratch"
  (interactive)
  (let ((ele (org-element-parse-buffer)))
    (test-helper ele)))

(defun test-helper (ele)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (newline)
    (insert (format "%s" (pp ele)))))

(defun convert-time-to-org-string (date-time)
  "date-time is a list which format is the same
as `decode-time' return"
  (if (nth 2 date-time)
      (format-time-string
       (cdr org-time-stamp-formats)
       (apply #'encode-time
              date-time))
    (progn
      (format-time-string
       (car org-time-stamp-formats)
       (apply #'encode-time
	      `(0 0 0
		  ,(nth 3 date-time)
		  ,(nth 4 date-time)
		  ,(nth 5 date-time)
		  nil))))))

(defun return-t (&rest argv) "return t for test" (maybe-make t))
(defun return-nil (&rest argv) "return nil for test" nil)
(defun return-maybe-error (err &rest argv)
  (maybe-error-make err))

(defun dummy-request-token (client-id client-secret nickname)
  "return dummy token for test"
  (maybe-make `((access_token . "aacceess")
		(refresh_token . "rreeffrr"))))

(defun dummy-refresh-token (client-id client-secret refresh-token nickname)
  "return dummy token for test (only access_token"
  (maybe-make `((access_token . "aacceess"))))

(defun dummy-events-list (&rest argv)
  "return dummy events list for test"
  (maybe-make '((items . [(
			  (id . "test")
			  (description . "Hello word")
			  (start . ((date . "2018-04-01")))
			  (end . ((date . "2018-04-02")))
			  (updated . "2018-01-01T01:02:03Z")
			  (description . "Hello word")
			  (summary . "hee"))
			 (
			  (id . "test2")
			  (description . "Poo boo")
			  (start . ((dateTime . "2018-04-01T08:00:00+0800")))
			  (end . ((dateTime . "2018-04-02T20:00:00+0800")))
			  (updated . "2018-01-01T01:02:03Z")
			  (description . "Hello word")
			  (summary . "hee"))]))))

(ert-deftest test-org-sgcal/update-token-error ()
  (should (equal (with-temp-buffer
  		   (insert "* My self for test\n"
  			   "  :PROPERTIES:\n"
  			   "  :CLIENT-ID: test-client-id\n"
  			   "  :CLIENT-SECRET: test-client-secret\n"
  			   "  :END:\n"
  			   "** main-cal\n"
  			   "   :PROPERTIES:\n"
  			   "   :CALENDAR-ID: test_cid\n"
  			   "   :END:\n")
  		   (org-sgcal-clear-tokens)
  		   (maybe-error-get
  		    (car (org-sgcal--update-token-alist (lambda (&rest argv)
  							  (maybe-error-make :testErr)
  							  ) #'dummy-refresh-token))))
  		 :testErr))
  (should (equal (with-temp-buffer
		   (insert "* My self for test\n"
			   "  :PROPERTIES:\n"
			   "  :CLIENT-ID: test-client-id\n"
			   "  :CLIENT-SECRET: test-client-secret\n"
			   "  :END:\n"
			   "** main-cal\n"
			   "   :PROPERTIES:\n"
			   "   :CALENDAR-ID: test_cid\n"
			   "   :END:\n")
		   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
		   (maybe-error-get
		    (car (org-sgcal--update-token-alist
			  #'dummy-request-token
			  (lambda (&rest argv)
			    (maybe-error-make :testErr))))))
		 :testErr))
  (should (equal (with-temp-buffer
  		   (insert "* My self for test\n"
  			   "** main-cal\n")
  		   (org-sgcal-clear-tokens)
  		   (car (maybe-error-get
			 (car (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)))))
  		 :tokenHeadingFormatErr))
  (should (equal (with-temp-buffer
  		   (insert "* \n"
  			   "  :PROPERTIES:\n"
  			   "  :CLIENT-ID: test-client-id\n"
  			   "  :CLIENT-SECRET: test-client-secret\n"
  			   "  :END:\n"
  			   "** main-cal\n"
  			   "   :PROPERTIES:\n"
  			   "   :CALENDAR-ID: test_cid\n"
  			   "   :END:\n")
  		   (org-sgcal-clear-tokens)
  		   (maybe-error-get
  		    (car (org-sgcal--update-token-alist (lambda (&rest argv)
  							  (maybe-error-make :testErr)
  							  ) #'dummy-refresh-token))))
  		 :noApiHeadingErr))
  )

(ert-deftest test-org-sgcal/create-headline ()
  "test org-sgcal--create-headline can 
create correct org string"
  (should
   (equal (org-element-interpret-data
	   (org-sgcal--create-headline '("test" 1 "TODO")
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      '(nil nil nil 4 4 2018)
				      '(nil nil nil 3 5 2018)))
	  (concat "* TODO test\n"
                  "DEADLINE: " (convert-time-to-org-string '(0 0 nil 3 5 2018 nil)) 
                  " SCHEDULED: " (convert-time-to-org-string '(0 0 nil 4 4 2018 nil)) 
                  "\n"
                  ":PROPERTIES:\n"
                  ":proper1:  1234\n"
                  ":proper2:  abcde\n"
                  ":END:\n"
                  "Oh my god!\n")))
  (should
   (equal (org-element-interpret-data
           (org-sgcal--create-headline '("test" 1 "TODO")
                                       nil
                                       "Oh my god!"
                                       '(nil nil nil 4 4 2018)
                                       '(nil nil nil 3 5 2018)))
          (concat "* TODO test\n"
                  "DEADLINE: " (convert-time-to-org-string '(0 0 nil 3 5 2018 nil)) 
                  " SCHEDULED: " (convert-time-to-org-string '(0 0 nil 4 4 2018 nil)) 
                  "\n"
                  "Oh my god!\n")))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal--create-headline '("test" 1 "TODO")
                                       '(("proper1" . 1234) ("proper2" . "abcde"))
                                       "Oh my god!"
                                       nil
                                       '(nil nil nil 3 5 2018)))
	  (concat "* TODO test\n"
                  "DEADLINE: "
                  (convert-time-to-org-string '(0 0 nil 3 5 2018 nil)) 
                  "\n"
                  ":PROPERTIES:\n"
                  ":proper1:  1234\n"
                  ":proper2:  abcde\n"
                  ":END:\n"
                  "Oh my god!\n")))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal--create-headline '("test" 1 "TODO")
                                       '(("proper1" . 1234) ("proper2" . "abcde"))
                                       "Oh my god!"
                                       nil
                                       nil))
	  (concat "* TODO test\n"
                  ":PROPERTIES:\n"
                  ":proper1:  1234\n"
                  ":proper2:  abcde\n"
                  ":END:\n"
                  "Oh my god!\n")))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal--create-headline '("test" 1 nil)
                                       '(("proper1" . 1234) ("proper2" . "abcde"))
                                       "Oh my god!"
                                       nil
                                       nil))
	  (concat "* test\n"
                  ":PROPERTIES:\n"
                  ":proper1:  1234\n"
                  ":proper2:  abcde\n"
                  ":END:\n"
                  "Oh my god!\n")))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal--create-headline '("test" 1 "TODO")
                                       '(("proper1" . 1234) ("proper2" . "abcde"))
                                       "Oh my god!"
                                       '(nil 40 3 4 4 2018)
                                       '(nil 50 4 3 5 2018)))
	  (concat "* TODO test\n"
                  "DEADLINE: "
                  (convert-time-to-org-string '(0 50 4 3 5 2018 nil)) 
                  " SCHEDULED: "
                  (convert-time-to-org-string '(0 40 3 4 4 2018 nil)) 
                  "\n"
                  ":PROPERTIES:\n"
                  ":proper1:  1234\n"
                  ":proper2:  abcde\n"
                  ":END:\n"
                  "Oh my god!\n"))))


(ert-deftest test-org-sgcal/replace-element ()
  "test org-sgcal-replace-element can
replace headline currectly"
  (should
   (equal
    (with-temp-buffer
      (insert "* TODO test\n"
	      "DEADLINE: "
	      (convert-time-to-org-string '(0 0 nil 3 5 2018 nil)) 
	      " SCHEDULED: "
	      (convert-time-to-org-string '(0 0 nil 4 4 2018 nil)) 
	      "\n"
	      ":PROPERTIES:\n"
	      ":proper1:  1234\n"
	      ":proper2:  abcde\n"
	      ":END:\n"
	      "Oh my god!\n"
	      "* DONE test3\n"
	      "DEADLINE: "
	      (convert-time-to-org-string '(0 0 nil 3 7 2018 nil)) 
	      " SCHEDULED: "
	      (convert-time-to-org-string '(0 0 nil 4 4 2018 nil)) 
	      "\n"
	      ":PROPERTIES:\n"
	      ":proper1:  weqre\n"
	      ":proper2:  bbb\n"
	      ":END:\n"
	      "Oh my godness!\n")
      (let ((ele (org-element-parse-buffer)))
	(org-element-map ele
	    'headline
	  (lambda (h) 
	    (org-sgcal--replace-element
	     h (org-sgcal--create-headline '("test" 1 "DONE")))
	    t) nil t) )
      (buffer-string))
    
    (concat "* DONE test\n"
            "* DONE test3\n"
            "DEADLINE: "
            (convert-time-to-org-string '(0 0 nil 3 7 2018 nil)) 
            " SCHEDULED: "
            (convert-time-to-org-string '(0 0 nil 4 4 2018 nil)) 
            "\n"
            ":PROPERTIES:\n"
            ":proper1:  weqre\n"
            ":proper2:  bbb\n"
            ":END:\n"
            "Oh my godness!\n"))))

(ert-deftest test-org-sgcal/parse-event-list ()
  
  "Test for org-sgcal--parse-event-list"
  (should (equal
	   (org-element-interpret-data (org-sgcal--parse-event-list '((items . [(
										 (id . "test")
										 (description . "Hello word")
										 (start . ((date . "2018-04-01")))
										 (end . ((date . "2018-04-02")))
                                                                                 (updated . "2018-01-01T01:02:03Z")
										 (description . "Hello word")
										 (summary . "hee"))])) 2))
           
	   (concat "** hee\n"
                   "DEADLINE: "
                   (convert-time-to-org-string '(0 0 nil 2 4 2018 nil)) 
                   " SCHEDULED: "
                   (convert-time-to-org-string '(0 0 nil 1 4 2018 nil)) 
                   "\n"
                   ":PROPERTIES:\n"
                   ":ID:       test\n"
                   ":UPDATED:  2018-01-01T01:02:03Z\n"
                   ":END:\n"
                   "Hello word\n")))
  (should (equal
	   (org-element-interpret-data (org-sgcal--parse-event-list '((items . [(
										 (id . "test")
										 (description . "Hello word")
										 (start . ((dateTime . "2018-04-01T14:30:00+0800")))
										 (end . ((dateTime . "2018-04-02T18:20:00+0800")))
                                                                                 (updated . "2018-01-01T01:02:03Z")
										 (description . "Hello word")
										 (summary . "hee"))])) 2))

	   (concat "** hee\n"
                   "DEADLINE: "
                   (convert-time-to-org-string '(0 20 18 2 4 2018 nil)) 
                   " SCHEDULED: "
                   (convert-time-to-org-string '(0 30 14 1 4 2018 nil)) 
                   "\n"
                   ":PROPERTIES:\n"
                   ":ID:       test\n"
                   ":UPDATED:  2018-01-01T01:02:03Z\n"
                   ":END:\n"
                   "Hello word\n"))))

(ert-deftest test-org-sgcal/headline-map ()
  "test for `org-sgcal--headline-map'"
  (should (equal
           (with-temp-buffer
             (insert "* RD Team\n"
		     "  :PROPERTIES:\n"
		     "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
		     "  :CLIENT-SECRET: 12341283461278561\n"
		     "  :END:\n"
		     "** sub title\n"
		     "   :PROPERTIES:\n"
		     "   :CALENDAR-ID: abcde\n"
		     "   :END:\n"
		     "** sub title2\n"
		     "   :PROPERTIES:\n"
		     "   :CALENDAR-ID: eeeee\n"
		     "   :END:\n")
             (let ((ele (org-element-parse-buffer)))
               (org-sgcal--headline-map
                1 ele (lambda (h1)
                        (setq h1 (org-element-put-property h1 :title "Biggg"))))
               (org-element-interpret-data ele)))
           (concat "* Biggg\n"
                   ":PROPERTIES:\n"
                   ":CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                   ":CLIENT-SECRET: 12341283461278561\n"
                   ":END:\n"
                   "** sub title\n"
                   ":PROPERTIES:\n"
                   ":CALENDAR-ID: abcde\n"
                   ":END:\n"
                   "** sub title2\n"
                   ":PROPERTIES:\n"
                   ":CALENDAR-ID: eeeee\n"
                   ":END:\n")))
  (should (equal
           (with-temp-buffer
             (insert "* RD Team\n"
                     ":PROPERTIES:\n"
                     "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                     "  :CLIENT-SECRET: 12341283461278561\n"
                     "  :END:\n"
                     "** sub title\n"
                     "   :PROPERTIES:\n"
                     "   :CALENDAR-ID: abcde\n"
                     "   :END:\n"
                     "** sub title2\n"
                     "   :PROPERTIES:\n"
                     "   :CALENDAR-ID: eeeee\n"
                     "   :END:\n")
             (let ((ele (org-element-parse-buffer)))
               (org-sgcal--headline-map
                2 ele (lambda (h1 h2)
                        (setq h1 (org-element-put-property h1 :title "Biggg"))
                        (setq h2 (org-element-put-property h2 :title "Smalll"))))
               (org-element-interpret-data ele)))
           (concat
            "* Biggg\n"
            ":PROPERTIES:\n"
            ":CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
            ":CLIENT-SECRET: 12341283461278561\n"
            ":END:\n"
            "** Smalll\n"
            ":PROPERTIES:\n"
            ":CALENDAR-ID: abcde\n"
            ":END:\n"
            "** Smalll\n"
            ":PROPERTIES:\n"
            ":CALENDAR-ID: eeeee\n"
            ":END:\n"))))

(ert-deftest test-org-sgcal/convert-time-string ()
  "test for org-sgcal--convert-time-string"
  (should (equal (org-sgcal--convert-time-string "2018-04-01T17:00:00+08:00")
                 "2018-04-01T17:00:00+0800"))
  (should (equal (org-sgcal--convert-time-string "2018-04-01T17:00:00Z")
                 "2018-04-01T17:00:00Z")))


(ert-deftest test-org-sgcal/token-and-fetch ()
  "test for update-tokens-alist and update-level3-headlines"
  (should (equal (with-temp-buffer
		   (insert "* My self for test\n"
			   "  :PROPERTIES:\n"
			   "  :CLIENT-ID: test-client-id\n"
			   "  :CLIENT-SECRET: test-client-secret\n"
			   "  :END:\n"
			   "** main-cal\n"
			   "   :PROPERTIES:\n"
			   "   :CALENDAR-ID: test_cid\n"
			   "   :END:\n")
		   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
		   (org-sgcal--update-level3-headlines #'dummy-events-list)
		   (buffer-string))
		 (concat "* My self for test\n"
			 ":PROPERTIES:\n"
			 ":CLIENT-ID: test-client-id\n"
			 ":CLIENT-SECRET: test-client-secret\n"
			 ":END:\n"
			 "** main-cal\n"
			 "  :PROPERTIES:\n"
			 "  :CALENDAR-ID: test_cid\n"
			 "  :END:\n"
			 "*** hee\n"
			 "   DEADLINE: <2018-04-02 一> SCHEDULED: <2018-04-01 日>\n"
			 "   :PROPERTIES:\n"
			 "   :ID:       test\n"
			 "   :UPDATED:  2018-01-01T01:02:03Z\n"
			 "   :END:\n"
			 "   Hello word\n"
			 "*** hee\n"
			 "   DEADLINE: <2018-04-02 一 20:00> SCHEDULED: <2018-04-01 日 08:00>\n"
			 "   :PROPERTIES:\n"
			 "   :ID:       test2\n"
			 "   :UPDATED:  2018-01-01T01:02:03Z\n"
			 "   :END:\n"
			 "   Poo boo\n")))
  (should (equal (with-temp-buffer
		   (insert "* My self for test\n"
			   "  :PROPERTIES:\n"
			   "  :CLIENT-ID: test-client-id\n"
			   "  :CLIENT-SECRET: test-client-secret\n"
			   "  :END:\n"
			   "** \n"
			   "   :PROPERTIES:\n"
			   "   :CALENDAR-ID: test_cid\n"
			   "   :END:\n")
		   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
		   (maybe-error-get
		    (car
		     (maybe-error-flatten (org-sgcal--update-level3-headlines #'dummy-events-list)))))
		 :noCalHeadingErr))
  (should (equal (with-temp-buffer
		   (insert "* My self for test\n"
			   "  :PROPERTIES:\n"
			   "  :CLIENT-ID: test-client-id\n"
			   "  :CLIENT-SECRET: test-client-secret\n"
			   "  :END:\n"
			   "** main-cal\n"
			   "   :PROPERTIES:\n"
                           "   :COLOR-ID: (TODO 1)\n"
			   "   :CALENDAR-ID: test_cid\n"
			   "   :END:\n")
		   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
		   (org-sgcal--update-level3-headlines #'dummy-events-list)
		   (buffer-string))
		 (concat "* My self for test\n"
			 ":PROPERTIES:\n"
			 ":CLIENT-ID: test-client-id\n"
			 ":CLIENT-SECRET: test-client-secret\n"
			 ":END:\n"
			 "** main-cal\n"
			 "  :PROPERTIES:\n"
			 "  :CALENDAR-ID: test_cid\n"
                         "  :COLOR-ID: (TODO 1)\n"
			 "  :END:\n"
			 "*** hee\n"
			 "   DEADLINE: <2018-04-02 一> SCHEDULED: <2018-04-01 日>\n"
			 "   :PROPERTIES:\n"
			 "   :ID:       test\n"
			 "   :UPDATED:  2018-01-01T01:02:03Z\n"
			 "   :END:\n"
			 "   Hello word\n"
			 "*** hee\n"
			 "   DEADLINE: <2018-04-02 一 20:00> SCHEDULED: <2018-04-01 日 08:00>\n"
			 "   :PROPERTIES:\n"
			 "   :ID:       test2\n"
			 "   :UPDATED:  2018-01-01T01:02:03Z\n"
			 "   :END:\n"
			 "   Poo boo\n")))
  )


(ert-deftest test-org-sgcal/search-up ()
    "test for search-up"
    (should (equal (with-temp-buffer
		     (org-mode)
		     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n")
		     (org-previous-visible-heading 1)
		     (org-sgcal--search-up))
		   '(:name "test headline3"
			   :todo "TODO"
			   :start (0 34 12 10 4 2018 nil)
			   :end (0 34 13 10 4 2018 nil)
			   :id "test-id" :updated "2018-04-11T23:46:09.411Z"
			   :contents nil
			   :color-id (TODO 2 DONE 3)
			   :cid "teststest@email.com"
			   :title "test headline1"
			   :client-id "test-client-id"
			   :client-secret "test-secret")))
    
    (should (equal (with-temp-buffer
		     (org-mode)
		     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
		     (org-previous-visible-heading 1)
		     (org-sgcal--search-up))
		   '(:name "test headline3"
			   :todo "TODO"
			   :start (0 34 12 10 4 2018 nil)
			   :end (0 34 13 10 4 2018 nil)
			   :id "test-id" :updated "2018-04-11T23:46:09.411Z"
			   :contents "abcdefg\n"
			   :color-id (TODO 2 DONE 3)
			   :cid "teststest@email.com"
			   :title "test headline1"
			   :client-id "test-client-id"
			   :client-secret "test-secret"))))

(defun dummy-fun-for-apply-at-point (cid atoken client-secret
					 eid start end smry loc desc color-id)
  "only for test"
  (maybe-make`(,cid ,atoken ,client-secret ,eid ,start ,end ,smry ,loc ,desc ,color-id)))

(ert-deftest test-org-sgcal/apply-at-point ()
  "test for apply-at-point"
  (should (equal
	   (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (org-previous-visible-heading 1)
	     (maybe-get (org-sgcal--apply-at-point #'dummy-fun-for-apply-at-point))
	     )
	   '("teststest@email.com"
	     "aacceess"
	     "test-secret" "test-id"
	     "2018-04-10T12:34:00Z"
	     "2018-04-10T13:34:00Z"
	     "TODO test headline3" nil "abcdefg\n" 2))))

(ert-deftest test-org-sgcal/delete-at-point-and-apply ()
  "test for delete-at-point-and-apply"
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (org-sgcal--delete-at-point-and-apply (lambda (&rest argv)
						     (return-maybe-error ':httpErr401)
						     ) #'return-t)
	     (buffer-string))
	  (concat "* test headline1\n"
		  "  :PROPERTIES:\n"
		  "  :CLIENT-ID: test-client-id\n"
		  "  :CLIENT-SECRET: test-secret\n"
		  "  :END:\n"
		  "\n"
		  "** test headline2\n"
		  "   :PROPERTIES:\n"
		  "   :CALENDAR-ID: teststest@email.com\n"
		  "   :COLOR-ID: (TODO 2 DONE 3)\n"
		  "   :END:\n"
		  "\n"
		  "*** TODO test headline3\n"
		  "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
		  "    :PROPERTIES:\n"
		  "    :ID:       test-id\n"
		  "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
		  "    :END:\n"
		  "abcdefg\n")))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (maybe-error-get (org-sgcal--delete-at-point-and-apply
			       (lambda (&rest argv)
				 (return-maybe-error ':httpErr401)
				 ) #'return-t))
	     )
	  :httpErr401))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (org-sgcal--delete-at-point-and-apply #'return-t #'return-t)
	     (buffer-string))
	  (concat "* test headline1\n"
		  "  :PROPERTIES:\n"
		  "  :CLIENT-ID: test-client-id\n"
		  "  :CLIENT-SECRET: test-secret\n"
		  "  :END:\n"
		  "\n"
		  "** test headline2\n"
		  "   :PROPERTIES:\n"
		  "   :CALENDAR-ID: teststest@email.com\n"
		  "   :COLOR-ID: (TODO 2 DONE 3)\n"
		  "   :END:\n"
		  "\n")))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1 no token\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     
	     (maybe-error-get
	      (org-sgcal--delete-at-point-and-apply (lambda (&rest argv)
						      (return-maybe-error ':httpErr401)
						      )
						    #'return-t)))
	  :notokenErr))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (maybe-error-get (org-sgcal--delete-at-point-and-apply #'return-t #'return-t)))
	  :headingFormatErr)))

(defun dummy-post-event (&rest argv)
  "dummy post event"
  (maybe-make '((summary . "OK I am good")
	       (id . "Id is here")
	       (description . "Hey Hey")
	       (start . ((date . "2018-04-03")))
	       (end . ((date . "2018-04-05")))
	       (updated . "2018-04-04T00:05:30Z"))))

(ert-deftest test-org-sgcal/apply-and-update-at-point ()
  "test apply-and-update-at-point"
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (org-sgcal--apply-and-update-at-point #'dummy-post-event)
	     (buffer-string))
	  
	  (concat "* test headline1\n"
		  "  :PROPERTIES:\n"
		  "  :CLIENT-ID: test-client-id\n"
		  "  :CLIENT-SECRET: test-secret\n"
		  "  :END:\n"
		  "\n"
		  "** test headline2\n"
		  "   :PROPERTIES:\n"
		  "   :CALENDAR-ID: teststest@email.com\n"
		  "   :COLOR-ID: (TODO 2 DONE 3)\n"
		  "   :END:\n"
		  "\n"
		  "*** OK I am good\n"
		  "    DEADLINE: <2018-04-05 四> SCHEDULED: <2018-04-03 二>\n"
		  "    :PROPERTIES:\n"
		  "    :ID:       Id is here\n"
		  "    :UPDATED:  2018-04-04T00:05:30Z\n"
		  "    :END:\n"
		  "    Hey Hey\n")))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1 no token\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "    DEADLINE: <2018-04-10 二 13:34> SCHEDULED: <2018-04-10 二 12:34>\n"
			     "    :PROPERTIES:\n"
			     "    :ID:       test-id\n"
			     "    :UPDATED:  2018-04-11T23:46:09.411Z\n"
			     "    :END:\n"
			     "abcdefg\n")
	     (maybe-error-get (org-sgcal--apply-and-update-at-point #'dummy-post-event)))
	  
	  :notokenErr))
  (should
   (equal (with-temp-buffer
	     (org-mode)
	     (insert "* test headline1\n"
			     "  :PROPERTIES:\n"
			     "  :CLIENT-ID: test-client-id\n"
			     "  :CLIENT-SECRET: test-secret\n"
			     "  :END:\n"
			     "\n"
			     "** test headline2\n"
			     "   :PROPERTIES:\n"
			     "   :CALENDAR-ID: teststest@email.com\n"
			     "   :COLOR-ID: (TODO 2 DONE 3)\n"
			     "   :END:\n"
			     "\n"
			     "*** TODO test headline3\n"
			     "abcdefg\n")
	     (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
	     (maybe-error-get (org-sgcal--apply-and-update-at-point #'dummy-post-event)))
	  
	  :headingFormatErr)))

(ert-deftest test-org-sgcal/maybe-error-flatten ()
  "test for maybe-error-flatten"
  (should
   (equal (format "%s" (maybe-error-flatten '(((maybe-error . a) (maybe-error . b)) (maybe-error . c))))
	  (format "%s" '((maybe-error . a) (maybe-error . b) (maybe-error . c))))))


(ert-deftest test-org-sgcal/apply-at-tags ()
  (should (equal (with-temp-buffer
                   (org-mode)
                   (insert "* heading2 for test\n"
                           "  :PROPERTIES:\n"
                           "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                           "  :CLIENT-SECRET: 12341283461278561\n"
                           "  :END:\n"
                           "** sub title\n"
                           "   :PROPERTIES:\n"
                           "   :CALENDAR-ID: abcde\n"
                           "   :END:\n"
                           "*** event_1                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "*** event_2                                                          :DELETE:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       asdfghjkl\n"
                           "    :UPDATED:  1234981234\n"
                           "    :END:\n"
                           "*** event_3                                               :test1:test2:test3:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456abc\n"
                           "    :UPDATED:  12039481723abc\n"
                           "    :END:\n"
                           "*** event_4                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456\n"
                           "    :UPDATED:  120394817230498\n"
                           "    :END:\n"
                           "    \n")
                   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
                   (org-sgcal--apply-at-tags #'dummy-post-event #'return-t)
                   (buffer-string))
                 (concat
                  "* heading2 for test\n"
                  "  :PROPERTIES:\n"
                  "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                  "  :CLIENT-SECRET: 12341283461278561\n"
                  "  :END:\n"
                  "** sub title\n"
                  "   :PROPERTIES:\n"
                  "   :CALENDAR-ID: abcde\n"
                  "   :END:\n"
                  "*** OK I am good\n"
                  "    DEADLINE: <2018-04-05 四> SCHEDULED: <2018-04-03 二>\n"
                  "    :PROPERTIES:\n"
                  "    :ID:       Id is here\n"
                  "    :UPDATED:  2018-04-04T00:05:30Z\n"
                  "    :END:\n"
                  "    Hey Hey\n"
                  "*** event_3                                               :test1:test2:test3:\n"
                  "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                  "    :PROPERTIES:\n"
                  "    :ID:       123456abc\n"
                  "    :UPDATED:  12039481723abc\n"
                  "    :END:\n"
                  "*** OK I am good\n"
                  "    DEADLINE: <2018-04-05 四> SCHEDULED: <2018-04-03 二>\n"
                  "    :PROPERTIES:\n"
                  "    :ID:       Id is here\n"
                  "    :UPDATED:  2018-04-04T00:05:30Z\n"
                  "    :END:\n    Hey Hey\n\n")
                 ))
  (should (equal (with-temp-buffer
                   (org-mode)
                   (insert "* \n"
                           "  :PROPERTIES:\n"
                           "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                           "  :CLIENT-SECRET: 12341283461278561\n"
                           "  :END:\n"
                           "** sub title\n"
                           "   :PROPERTIES:\n"
                           "   :CALENDAR-ID: abcde\n"
                           "   :END:\n"
                           "*** event_1                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "*** event_2                                                          :DELETE:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       asdfghjkl\n"
                           "    :UPDATED:  1234981234\n"
                           "    :END:\n"
                           "*** event_3                                               :test1:test2:test3:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456abc\n"
                           "    :UPDATED:  12039481723abc\n"
                           "    :END:\n"
                           "*** event_4                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456\n"
                           "    :UPDATED:  120394817230498\n"
                           "    :END:\n"
                           "    \n")
                   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
                   (maybe-error-get
                    (car
                     (maybe-error-flatten
                      (org-sgcal--apply-at-tags #'dummy-post-event #'return-t)))))
                 :noApiHeadingErr))

  (should (equal (with-temp-buffer
                   (org-mode)
                   (insert "* Test heading\n"
                           "  :PROPERTIES:\n"
                           "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                           "  :CLIENT-SECRET: 12341283461278561\n"
                           "  :END:\n"
                           "** \n"
                           "   :PROPERTIES:\n"
                           "   :CALENDAR-ID: abcde\n"
                           "   :END:\n"
                           "*** event_1                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "*** event_2                                                          :DELETE:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       asdfghjkl\n"
                           "    :UPDATED:  1234981234\n"
                           "    :END:\n"
                           "*** event_3                                               :test1:test2:test3:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456abc\n"
                           "    :UPDATED:  12039481723abc\n"
                           "    :END:\n"
                           "*** event_4                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456\n"
                           "    :UPDATED:  120394817230498\n"
                           "    :END:\n"
                           "    \n")
                   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
                   (maybe-error-get
                    (car
                     (maybe-error-flatten
                      (org-sgcal--apply-at-tags #'dummy-post-event #'return-t)))))
                 :noCalHeadingErr))

  (should (equal (with-temp-buffer
                   (org-mode)
                   (insert "* Test heading\n"
                           "  :PROPERTIES:\n"
                           "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                           "  :CLIENT-SECRET: 12341283461278561\n"
                           "  :END:\n"
                           "** subheading\n"
                           "   :PROPERTIES:\n"
                           "   :CALENDAR-ID: abcde\n"
                           "   :END:\n"
                           "*** event_1                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "*** event_2                                                          :DELETE:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       asdfghjkl\n"
                           "    :UPDATED:  1234981234\n"
                           "    :END:\n"
                           "*** event_3                                               :test1:test2:test3:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456abc\n"
                           "    :UPDATED:  12039481723abc\n"
                           "    :END:\n"
                           "*** test                                                          :UPDATE:\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456\n"
                           "    :UPDATED:  120394817230498\n"
                           "    :END:\n"
                           "    \n")
                   (org-sgcal--update-token-alist #'dummy-request-token #'dummy-refresh-token)
                   (maybe-error-get
                    (car
                     (maybe-error-flatten
                      (org-sgcal--apply-at-tags #'dummy-post-event #'return-t)))))
                 :headingFormatErr))
  
  (should (equal (with-temp-buffer
                   (org-mode)
                   (insert "* test heading\n"
                           "  :PROPERTIES:\n"
                           "  :CLIENT-ID: asdlfkjadkjfhasjkdhfas\n"
                           "  :CLIENT-SECRET: 12341283461278561\n"
                           "  :END:\n"
                           "** sub title\n"
                           "   :PROPERTIES:\n"
                           "   :CALENDAR-ID: abcde\n"
                           "   :END:\n"
                           "*** event_1                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "*** event_2                                                          :DELETE:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       asdfghjkl\n"
                           "    :UPDATED:  1234981234\n"
                           "    :END:\n"
                           "*** event_3                                               :test1:test2:test3:\n"
                           "    DEADLINE: <2018-04-19 四> SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456abc\n"
                           "    :UPDATED:  12039481723abc\n"
                           "    :END:\n"
                           "*** event_4                                                          :UPDATE:\n"
                           "    SCHEDULED: <2018-04-18 三>\n"
                           "    :PROPERTIES:\n"
                           "    :ID:       123456\n"
                           "    :UPDATED:  120394817230498\n"
                           "    :END:\n"
                           "    \n")
                   (org-sgcal-clear-tokens)
                   (car
                    (maybe-error-get
                     (car
                      (maybe-error-flatten
                       (org-sgcal--apply-at-tags #'dummy-post-event #'return-t))))))
                 :fetchAllErr))
  )
