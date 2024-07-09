;;; flymake-jsonlint.el --- JSON linter with jsonlint  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Kjær Jørgensen (shaohme) <me@lagy.org>
;;
;; Author: Martin Kjær Jørgensen <me@lagy.org>
;; Created: 27 October 2022
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/shaohme/flymake-jsonlint
;;; Commentary:

;; This package adds JSON syntax checker to flymake.
;; Make sure python>=3.9 compatible executable is on your PATH.

;; flymake-jsonlint expect `python' to produce stdout like:
;; Expecting ':' delimiter: line 1 column 9 (char 8)

;; SPDX-License-Identifier: GPL-3.0-or-later

;; flymake-jsonlint is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; flymake-jsonlint is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with flymake-jsonlint.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'flymake)

(defgroup flymake-jsonlint nil
  "Jsonlint backend for Flymake."
  :prefix "flymake-jsonlint-"
  :group 'tools)

(defcustom flymake-jsonlint-python-program
  "python"
  "Name of `python' executable."
  :type 'string)

(defvar-local flymake-jsonlint--proc nil)

(defun flymake-jsonlint (report-fn &rest _args)
  "Flymake backend for python report using REPORT-FN."
  (if (not flymake-jsonlint-python-program)
      (error "No python program name set"))
  (let ((flymake-jsonlint--executable-python-path (executable-find flymake-jsonlint-python-program)))
    (if (or (null flymake-jsonlint--executable-python-path)
            (not (file-executable-p flymake-jsonlint--executable-python-path)))
        (error "Could not find '%s' executable" flymake-jsonlint-python-program))
    (when (process-live-p flymake-jsonlint--proc)
      (kill-process flymake-jsonlint--proc)
      (setq flymake-jsonlint--proc nil))
    (let ((source (current-buffer)))
      (save-restriction
        (widen)
        (setq
         flymake-jsonlint--proc
         (make-process
          :name "flymake-jsonlint" :noquery t :connection-type 'pipe
          :buffer (generate-new-buffer " *flymake-jsonlint*")
          :command (list flymake-jsonlint--executable-python-path "-m" "json.tool")
          :sentinel
          (lambda (proc _event)
            (when (eq 'exit (process-status proc))
              (unwind-protect
                  (if (with-current-buffer source (eq proc flymake-jsonlint--proc))
                      (with-current-buffer (process-buffer proc)
                        (goto-char (point-min))
                        (let ((diags))
                          (while (re-search-forward "^\\(.+\\): line \\([0-9]+\\) column \\([0-9]+\\).*$" nil t)
                            (let ((region (flymake-diag-region source (string-to-number (match-string 2)) (string-to-number (match-string 3)))))
                              ;; expect `region' to only have 2 values (start . end)
                              (push (flymake-make-diagnostic source
                                                             (car region)
                                                             (cdr region)
                                                             :error
                                                             (match-string 1)) diags)))
                          (funcall report-fn (reverse diags))))
                    (flymake-log :warning "Canceling obsolete check %s"
                                 proc))
                (kill-buffer (process-buffer proc)))))))
        (process-send-region flymake-jsonlint--proc (point-min) (point-max))
        (process-send-eof flymake-jsonlint--proc)))))

;;;###autoload
(defun flymake-jsonlint-setup ()
  "Enable jsonlint flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-jsonlint nil t))

(provide 'flymake-jsonlint)
;;; flymake-jsonlint.el ends here
