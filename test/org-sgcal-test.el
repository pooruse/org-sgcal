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
      (setcar (nthcdr 2 date-time) 0)
      (format-time-string
       (car org-time-stamp-formats)
       (apply #'encode-time
              date-time)))))

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
      (insert (concat "* TODO test\n"
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
                      "Oh my godness!\n"))
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
             (insert (concat "* RD Team\n"
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
                             "   :END:\n"))
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
