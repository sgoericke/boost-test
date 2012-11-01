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

(defvar boost-test-mode-hook nil)

(defvar boost-test-program nil)

(defvar boost-test--command-history nil)

(defun boost-test--read-test-command (command)
  "Read which program to start"
  (read-shell-command "Run test program:" command
                      (if (equal (car boost-test--command-history) command)
                          '(boost-test--command-history . 1)
                        'boost-test--command-history)))


;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-run ()
  "Runs the test specified"
  (interactive)
  (when (equal boost-test-program nil)
    (setq boost-test-program (read-file-name "Specify test program:")))

  (let ((test-prog (boost-test--read-test-command (eval boost-test-program)))
        (buf (get-buffer-create (generate-new-buffer-name "*test-result*"))))

    (setq default-directory (file-name-directory test-prog))
;;  (let ((proc (start-process test-prog buf test-prog "--output_format=XML --log_level=test_suite")))
    (let ((proc (start-process test-prog buf test-prog "--output_format=XML --log_level=all --repor_level=detailed")))
      (let ((sentinel (lambda (process signal)
                        (unwind-protect
                            (save-excursion
                              (set-buffer (process-buffer process))
                              (boost-test--process-result (buffer-substring 1 (point-max))))
                          (kill-buffer (process-buffer process))))))
        (set-process-sentinel proc sentinel)))))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test--process-result (result)

  (let* ((buf (get-buffer-create "*boost::test results*"))
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
    (switch-to-buffer buf)
    (boost-test-mode)))

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

      (insert (format "|%s|\n" (make-string 60 ?-)))
      (insert (format "| %s %S result summary:\n" (car root) (cdr (assq 'name attr))))
      (insert (format "|%s|\n" (make-string 60 ?-)))

        (insert (format "| %-20s | %-35s |\n" "name" (cdr (assq 'name attr))))
        (insert (format "| %-20s | %-35s |\n" "result" (cdr (assq 'result attr))))
        (insert (format "| %-20s | %-35s |\n" "assertions_passed" (cdr (assq 'assertions_passed attr))))
        (insert (format "| %-20s | %-35s |\n" "assertions_failed" (cdr (assq 'assertions_passed attr))))
        (insert (format "| %-20s | %-35s |\n" "expected_failures" (cdr (assq 'expected_failures attr))))
        (insert (format "| %-20s | %-35s |\n" "test_cases_passed" (cdr (assq 'test_cases_passed attr))))
        (insert (format "| %-20s | %-35s |\n" "test_cases_failed" (cdr (assq 'test_cases_failed attr))))
        (insert (format "| %-20s | %-35s |\n" "test_cases_skipped" (cdr (assq 'test_cases_skipped attr))))
        (insert (format "| %-20s | %-35s |\n" "test_cases_aborted" (cdr (assq 'test_cases_aborted attr))))

        ;; (insert "\n")

        ;; (while (< attr-count (length (xml-node-attributes root)))
        ;;   (insert (format "| %-20s | %-35s |\n"
        ;;                 (car (elt attr attr-count))
        ;;                 (cdr (elt attr attr-count))))
        ;;   (setq attr-count (1+ attr-count)))

      (insert (format "|%s|\n" (make-string 60 ?-))))))

;;---------------------------------------------------------------------------------------------------------------------
(defun boost-test-mode nil
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'boost-test-mode)
  (setq mode-name "Boost.Test")
  (run-mode-hooks 'boost-test-mode-hook))

(provide 'boost-test)