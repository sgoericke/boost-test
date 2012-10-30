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

;;(eval-when-compile (require 'cl))

(defun process--result(test result)
  (let ((buf (get-buffer-create (concat "test results for: " (process-name test))))
		(xml (concat "<Result>" result "</Result>")))
	(message "%s" xml)
	(set-buffer buf)
	(erase-buffer)
	(insert xml)
	(let ((root (xml-parse-region (point-min) (point-max))))
	  (message "%s" (car root))
	  (switch-to-buffer buf))))
;;	(let ((root (xml-parse-region (point-min) (point-max))))
;;	  (erase-buffer)
;;	  (message "%s" root))))
;;	  (switch-to-buffer buf))))


(defun boost-test-run ()
  "Runs the test specified"
  (interactive)
  (let ((test-prog (read-file-name "Test program:")))
	(let ((proc (start-process test-prog nil test-prog "--output_format=XML --log_level=test_suite")))

	  (set-process-filter proc 'process--result))))

(provide 'boost-test)