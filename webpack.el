;;; webpack.el --- Integration with webpack for emacs

;; Copyright © 2015 Rustem Muslimov
;;
;; Author:     Rustem Muslimov <r.muslimov@gmail.com>
;; Version:    0.1.0
;; Keywords:   webpack, js
;; Package-Requires: ((f "0.17.2") (s "1.9.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Make web-development easier with webpack.el

(require 'f)
(require 's)

(defvar webpack-config-filename
  "webpack.config.js")

(defvar webpack--extract-cli-function
  "var wb=require('./%s'); console.log(JSON.stringify(wb.resolve));")

(defvar webpack--containing-path
  nil "Webpack config file containing path.")

(defvar webpack--resolve-config
  nil "Webpack config that we use lives here.")

(defun webpack--get-config (PATH)
  "Find webpack configureation in PATH"
  (let ((default-directory PATH)
        (webpack-js-cmd (format webpack--extract-cli-function webpack-config-filename)))
    (with-temp-buffer
      (call-process "node" nil t nil "-e" webpack-js-cmd)
      (json-read-from-string (buffer-string)))))

(defun webpack-setup-config ()
  "Setup config defvar before doing job."
  (interactive)
  (setq webpack--containing-path (vc-git-root (buffer-file-name)))
  (setq webpack--resolve-config
        (webpack--get-config webpack--containing-path))
  (format "Webpack config found at: %s."
          (f-join webpack--containing-path webpack-config-filename)))

(defun webpack--get-user-request ()
  "Understand what user wants, returns plist with info. "
  (let ((requested-path
         (save-excursion
           (line-beginning-position)
           (search-forward "'")
           (setq beg (point))
           (search-forward "'")
           (buffer-substring-no-properties beg (- (point) 1)))))
    (list :requested-path requested-path :requested-word (current-word))))

(defun webpack--find-existing-path (requested-path)
  "Iter over existing pathes and check if REQUESTED-PATH exists."
  (let* ((config-roots (cdr (assoc 'root webpack--resolve-config)))
         (config-aliases (cdr (assoc 'alias webpack--resolve-config)))

         (root-candidates
          (mapcar (lambda (x) (f-join x requested-path)) config-roots))
         (first-path (car (s-split "/" requested-path)))
         (last-path (s-join "/" (cdr (s-split "/" requested-path))))
         )

    ;; try to find aliases first
    (-if-let
        (match (assoc (intern first-path) config-aliases))
        (let ((candidate (f-join (cdr match) last-path)))
          (if (f-exists? candidate) candidate
            (-if-let* ((candidate-js (format "%s.js" candidate))
                       (f-exists? candidate-js))
                candidate-js
            (throw 'incorrect-configuration "Webpack config is incorrect! "))
          ))
      ;; or in root path otherwise
      "TODO: searching in root"

      ))
  )

(defun webpack-goto-definition ()
  "Main ninja method."
  (interactive)
  (when (eq webpack--resolve-config nil)
    (webpack--setup-config))

  (let* ((request (webpack--get-user-request))
         (requested-path (plist-get request :requested-path))
         (requested-word (plist-get request :requested-word))
         (found-file (webpack--find-existing-path requested-path)))

    (find-file found-file)
    (if (and (s-starts-with? "{" requested-word) (s-ends-with? "{" requested-word))
        (search-forward (format "class %s extends" requested-word))
      (search-forward "export default"))))

(provide 'webpack)
