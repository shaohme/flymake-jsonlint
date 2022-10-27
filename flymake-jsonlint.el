;;; flymake-jsonlint.el --- JSON linter with jsonlint  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Martin Kjær Jørgensen (shaohme) <me@lagy.org>
;;
;; Author: Martin Kjær Jørgensen <me@lagy.org>
;; Created: 27 October 2022
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/shaohme/flymake-jsonlint
;;; Commentary:

;; This package adds JSON syntax checker jsonlint.
;; Make sure 'jsonlint' binary is on your path.
;; Installation instructions https://www.npmjs.com/package/@prantlf/jsonlint

;; flymake-jsonlint expect `jsonlint' to produce stdout like:
;; line 2, col 8, found: 'STRING' - expected: 'EOF', '}', ':', ',', ']'.

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

(defcustom flymake-jsonlint-program
  "jsonlint"
  "Name of `jsonlint' executable."
  :type 'string)

(defvar-local flymake-jsonlint--proc nil)

(defun flymake-jsonlint (report-fn &rest _args)
  "Flymake backend for jsonlint report using REPORT-FN."
  (if (not flymake-jsonlint-program)
      (error "No jsonlint program name set"))
  (let ((flymake-jsonlint--executable-path (executable-find flymake-jsonlint-program)))
    (if (or (null flymake-jsonlint--executable-path)
            (not (file-executable-p flymake-jsonlint--executable-path)))
        (error "Could not find '%s' executable" flymake-jsonlint-program))
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
          :command (list flymake-jsonlint--executable-path "-c")
          :sentinel
          (lambda (proc _event)
            (when (eq 'exit (process-status proc))
              (unwind-protect
                  (if (with-current-buffer source (eq proc flymake-jsonlint--proc))
                      (with-current-buffer (process-buffer proc)
                        (goto-char (point-min))
                        (let ((diags))
                          (while (search-forward-regexp "^.*line \\([0-9]+\\), col \\([0-9]+\\), \\(.+\\)$" nil t)
                            (let ((region (flymake-diag-region source (string-to-number (match-string 1)) (string-to-number (match-string 2)))))
                              ;; expect `region' to only have 2 values (start . end)
                              (push (flymake-make-diagnostic source
                                                             (car region)
                                                             (cdr region)
                                                             :error
                                                             (match-string 3)) diags)))
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
