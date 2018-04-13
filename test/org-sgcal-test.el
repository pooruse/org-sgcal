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


(ert-deftest test-org-sgcal/create-headline ()
  "test org-sgcal-create-headline can 
create correct org string"
  (should
   (equal (org-element-interpret-data
	   (org-sgcal-create-headline '("test" 1 "TODO")
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      '(nil nil nil 4 4 2018)
				      '(nil nil nil 3 5 2018)))
	  "* TODO test
DEADLINE: <2018-05-03 四> SCHEDULED: <2018-04-04 三>
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
"))
  (should
     (equal (org-element-interpret-data
	     (org-sgcal-create-headline '("test" 1 "TODO")
					nil
					"Oh my god!"
					'(nil nil nil 4 4 2018)
					'(nil nil nil 3 5 2018)))
	    "* TODO test
DEADLINE: <2018-05-03 四> SCHEDULED: <2018-04-04 三>
Oh my god!
"
	    ))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal-create-headline '("test" 1 "TODO")
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      nil
				      '(nil nil nil 3 5 2018)))
	  "* TODO test
DEADLINE: <2018-05-03 四>
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
"
	  ))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal-create-headline '("test" 1 "TODO")
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      nil
				      nil))
	  "* TODO test
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
"))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal-create-headline '("test" 1 nil)
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      nil
				      nil))
	  "* test
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
"
	  ))
  (should
   (equal (org-element-interpret-data
	   (org-sgcal-create-headline '("test" 1 "TODO")
				      '(("proper1" . 1234) ("proper2" . "abcde"))
				      "Oh my god!"
				      '(nil 40 3 4 4 2018)
				      '(nil 50 4 3 5 2018)))
	  "* TODO test
DEADLINE: <2018-05-03 四 04:50> SCHEDULED: <2018-04-04 三 03:40>
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
")))


(ert-deftest test-org-sgcal/replace-element ()
  "test org-sgcal-replace-element can
replace headline currectly"
  (should
   (equal
    (with-temp-buffer
      (insert "* TODO test
DEADLINE: <2018-05-03 四> SCHEDULED: <2018-04-04 三>
:PROPERTIES:
:proper1:  1234
:proper2:  abcde
:END:
Oh my god!
* DONE test3
DEADLINE: <2018-07-03 四> SCHEDULED: <2018-04-04 三>
:PROPERTIES:
:proper1:  weqre
:proper2:  bbb
:END:
Oh my godness!
"
	      )
      (let ((ele (org-element-parse-buffer)))
	(org-element-map ele
	    'headline
	  (lambda (h) 
	    (org-sgcal-replace-element
	     h (org-sgcal-create-headline '("test" 1 "DONE")))
	    t) nil t) )
      (buffer-string))

    "* DONE test
* DONE test3
DEADLINE: <2018-07-03 四> SCHEDULED: <2018-04-04 三>
:PROPERTIES:
:proper1:  weqre
:proper2:  bbb
:END:
Oh my godness!
")))
 
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

	   "** hee
DEADLINE: <2018-04-02 一> SCHEDULED: <2018-04-01 日>
:PROPERTIES:
:ID:       test
:UPDATED:  2018-01-01T01:02:03Z
:END:
Hello word
"))
  (should (equal
	   (org-element-interpret-data (org-sgcal--parse-event-list '((items . [(
										 (id . "test")
										 (description . "Hello word")
										 (start . ((dateTime . "2018-04-01T14:30:00+0800")))
										 (end . ((dateTime . "2018-04-02T18:20:00+0800")))
                                                                                 (updated . "2018-01-01T01:02:03Z")
										 (description . "Hello word")
										 (summary . "hee"))])) 2))

	   "** hee
DEADLINE: <2018-04-02 一 18:20> SCHEDULED: <2018-04-01 日 14:30>
:PROPERTIES:
:ID:       test
:UPDATED:  2018-01-01T01:02:03Z
:END:
Hello word
")))

(ert-deftest test-org-sgcal/org-sgcal-headline-map ()
  "test for org-sgcal-headline-map"
  (should (equal
           (with-temp-buffer
             (insert "* RD Team
  :PROPERTIES:
  :CLIENT-ID: asdlfkjadkjfhasjkdhfas
  :CLIENT-SECRET: 12341283461278561
  :END:
** sub title
   :PROPERTIES:
   :CALENDAR-ID: abcde
   :END:
** sub title2
   :PROPERTIES:
   :CALENDAR-ID: eeeee
   :END:")
             (let ((ele (org-element-parse-buffer)))
               (org-sgcal-headline-map
                1 ele (lambda (h1)
                        (setq h1 (org-element-put-property h1 :title "Biggg"))))
               (org-element-interpret-data ele)))
           "* Biggg
:PROPERTIES:
:CLIENT-ID: asdlfkjadkjfhasjkdhfas
:CLIENT-SECRET: 12341283461278561
:END:
** sub title
:PROPERTIES:
:CALENDAR-ID: abcde
:END:
** sub title2
:PROPERTIES:
:CALENDAR-ID: eeeee
:END:
"))
  (should (equal
           (with-temp-buffer
             (insert "* RD Team
  :PROPERTIES:
  :CLIENT-ID: asdlfkjadkjfhasjkdhfas
  :CLIENT-SECRET: 12341283461278561
  :END:
** sub title
   :PROPERTIES:
   :CALENDAR-ID: abcde
   :END:
** sub title2
   :PROPERTIES:
   :CALENDAR-ID: eeeee
   :END:")
             (let ((ele (org-element-parse-buffer)))
               (org-sgcal-headline-map
                2 ele (lambda (h1 h2)
                        (setq h1 (org-element-put-property h1 :title "Biggg"))
                        (setq h2 (org-element-put-property h2 :title "Smalll"))))
               (org-element-interpret-data ele)))
           "* Biggg
:PROPERTIES:
:CLIENT-ID: asdlfkjadkjfhasjkdhfas
:CLIENT-SECRET: 12341283461278561
:END:
** Smalll
:PROPERTIES:
:CALENDAR-ID: abcde
:END:
** Smalll
:PROPERTIES:
:CALENDAR-ID: eeeee
:END:
"))
  )

(ert-deftest test-org-sgcal/convert-time-string ()
  "test for org-sgcal--convert-time-string"

  (should (equal (org-sgcal--convert-time-string "2018-04-01T17:00:00+08:00")
                 "2018-04-01T17:00:00+0800"))
  (should (equal (org-sgcal--convert-time-string "2018-04-01T17:00:00Z")
                 "2018-04-01T17:00:00Z")))
