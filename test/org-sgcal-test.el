(defvar test-org-string
  "#+ATTR_PONPON: watermelon
* TODO Test
  abcdefg
** sub-test2
   :PROPERTIES:
   :test_prop: bbb
   :END:

* Test2
  :PROPERTIES:
  :test_prop: aaa
  :END:
  POPOPOP")


(defvar test-org-string2
  "* headddd")

(defvar test-org-string3
  "")

(defvar test-org-element1
  (let* ((hea (org-element-create 'headline))
	 (hea (org-element-put-property hea :title "testelement"))
	 (hea (org-element-put-property hea :level 1))
	 (hea (org-element-set-contents hea "testcontents")))
    hea))

(defun test-org-element ()
  "test org element "
  (interactive)
  (let ((ele (org-element-at-point)))
    (org-sgcal-replace-element ele test-org-element1)))


(defun test-org-element2 ()
  "print current buffer org struct in *scratch"
  (interactive)
  (let ((ele (org-element-parse-buffer)))
    (test-helper ele)))

(defun get-current-buffer ()
  (current-buffer))

(defun test-helper (ele)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (newline)
    (insert (format "%s" (pp ele)))))
