;; boost-test.el --- an interface to the Boost.Test framework.
;;
;; Copyright (C) 2012 Sven Goericke <sven.goericke * gmail com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "boost-test--<descriptive-name>".

(eval-when-compile (require 'cl))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-run ()
  "Runs the test specified"
  (interactive)
  (let ((test-prog (read-file-name "Test program:"))
		(buf (get-buffer-create (generate-new-buffer-name "*test-result*"))))
	(let ((proc (start-process test-prog buf test-prog "--output_format=XML --log_level=test_suite")))
	  (let ((sentinel (lambda (process signal)
						(unwind-protect
							(save-excursion
							  (set-buffer (process-buffer process))
							  (boost-test--process-result (buffer-substring 1 (point-max))))
						  (kill-buffer (process-buffer process))))))
		(set-process-sentinel proc sentinel)))))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--process-result (result)

  (let* ((buf (get-buffer-create "boost::test results"))
		 (root (with-temp-buffer
				 (insert (concat "<Root>" result "</Root>"))
				 (xml-parse-region (point-min) (point-max))))

		 (result_root (car root))
		 (test_log (car (xml-get-children result_root 'TestLog)))
		 (test_result (car (xml-get-children result_root 'TestResult))))

	(set-buffer buf)
	(erase-buffer)

	(boost-test--parse-log-node test_log buf)
	(insert "\n")
	(boost-test--parse-result-node test_result buf)
	(switch-to-buffer buf)))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--parse-log-node (node buffer)
  "Parse the TestLog part of the xml"
  (cond ((null node) nil)
		((listp node) (let ((elem (xml-node-name node))
							(children (xml-node-children node)))

						(cond ((string= "TestLog" elem) nil)

							  ((string= "FatalError" elem) (let ((attr (xml-node-attributes node)))
															 (insert (format "%s: " elem))
															 (insert (format "%s "(car (last node))))
															 (insert (format "[%s:%s:]\n"
																			 (cdr (assq 'file attr))
																			 (cdr (assq 'line attr))))))

							  (t (cond ((stringp (car (last node))) (insert
																	 (format "%s: %s\n" elem (car (last node)))))
									   (t (let ((attr (xml-node-attributes node)))
											(insert (format "%s: %s\n" elem (cdr (assq 'name attr)))))))))
			 			(mapcar (lambda (x) (boost-test--parse-log-node x buffer)) children)))))
						
						
;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--parse-result-node (node buffer)
  "Parse the TestResult part of the xml"

  (when (listp node)
	(let ((root (car (xml-get-children node 'TestSuite)))
		  (attr (xml-node-attributes (car (xml-get-children node 'TestSuite))))
		  (attr-count 0))

	  (insert (format "%s result summary:\n" (car root)))

	  (while (< attr-count (length (xml-node-attributes root)))
		(insert (format "\t %s: %s\n"
						(car (elt attr attr-count))
						(cdr (elt attr attr-count))))
		(setq attr-count (1+ attr-count))))))

(provide 'boost-test)