(defun test-org-element ()
  "print current buffer org struct in *scratch"
  (interactive)
  (let ((ele (org-element-at-point)))
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
				      '(("proper1" 1234) ("proper2" "abcde"))
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
				      '(("proper1" 1234) ("proper2" "abcde"))
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
				      '(("proper1" 1234) ("proper2" "abcde"))
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
				      '(("proper1" 1234) ("proper2" "abcde"))
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
				      '(("proper1" 1234) ("proper2" "abcde"))
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
 
