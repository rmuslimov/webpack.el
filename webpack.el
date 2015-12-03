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

;; (defconst webpack-PROJECT_PATH nil)
(defconst webpack-PROJECT_PATH
  "~/projects/airborne"
  "User facing PROJECT_PATH variable.")

(defvar webpack-project-path nil "Internal parsed value.")

(defconst webpack-config-filename
  "webpack.config.js")

(defun webpack--extract-cli-function (CONFIG_NAME)
  (format
   "var wb=require('%s/%s'); console.log(JSON.stringify(wb.resolve));"
   webpack-project-path CONFIG_NAME))

(defvar webpack--resolve-config
  nil "Webpack extracted config that we use lives here.")

(defun webpack--get-config ()
  "Find webpack configuration."
  (let ((default-directory webpack-project-path)
        (process-environment
          (cons (format "NODE_PATH=%s" (f-join webpack-project-path "node_modules"))
                process-environment)
         )
        (webpack-js-cmd (format (webpack--extract-cli-function webpack-config-filename))))
    (with-temp-buffer
      (call-process "node" nil t nil "-e" webpack-js-cmd)
      (json-read-from-string (buffer-string)))))

(defun webpack-setup-config ()
  "Setup config defvar before doing job."
  (when (s-blank? webpack-project-path)
    (setq webpack-project-path (or webpack-PROJECT_PATH (vc-git-root (buffer-file-name)))))
  (when (s-starts-with? "~" webpack-project-path)
    (setq webpack-project-path (expand-file-name webpack-project-path)))

  (setq webpack--resolve-config (webpack--get-config))
  (message
   (format "Webpack config found at: %s."
           (f-join webpack-project-path webpack-config-filename))))

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
      )))

(defun webpack--find-declaration (WORD FILENAME)
  "Find declaration of WORD in FILENAME."
  (when (eq webpack--resolve-config nil)
    (webpack-setup-config))

  (let* ((default-directory "~/projects/webpack.el/")
         (process-environment
          (cons (format "NODE_PATH=%s" (f-join webpack-project-path "node_modules"))
                process-environment))
         (absfilepath (f-join webpack-project-path FILENAME))
         (extracted-ast-info
          (with-temp-buffer
            (call-process "node" nil t nil "extract.js" absfilepath  WORD)
            (buffer-string)))
         (astinfo (json-read-from-string extracted-ast-info)))

	(progn
	  ;; do search if possible
	  (when (equal (cdr (assoc 'status astinfo)) "success")
		(let* ((ast-declaredType (cdr (assoc 'declaredType astinfo)))
			   (ast-identifier (cdr (assoc 'identifier astinfo)))
			   (ast-identifier-name (cdr (assoc 'name ast-identifier))))
		  (cond
		   ;; import statement
		   ((equal ast-declaredType "ImportDefaultSpecifier")
			(let* ((imports (webpack--imports-by-specifiers))
				   (details (cdr (assoc ast-identifier-name imports))))
			  (webpack--find-declaration
			   (cdr (assoc 'imported details))
			   (webpack--find-existing-path (cdr (assoc 'source details)))
			   )))
		   ;; internal statement
		   ((equal ast-declaredType "ClassDeclaration")
			(let* ((location (cdr (assoc 'loc ast-identifier)))
				   (column (cdr (assoc 'column (cdr (assoc 'start location)))))
				   (line (cdr (assoc 'line (cdr (assoc 'start location))))))
			  (progn
				(find-file FILENAME)
				(goto-line line)
				(move-to-column column))
			  ))
		   ;; unknown
		   astinfo)
		  )))
	))

(defun webpack--current-word ()
  (s-replace-all '(("<" . "") (">" . "")) (current-word)))

(defun webpack-goto-definition ()
  "Main ninja method."
  (interactive)
  (when (eq webpack--resolve-config nil)
    (webpack-setup-config))

  (let* ((request (webpack--get-user-request))
         (requested-path (plist-get request :requested-path))
         (requested-word (plist-get request :requested-word))
         (found-file (webpack--find-existing-path requested-path)))

    (princ
     (webpack--find-declaration (webpack--current-word) (buffer-file-name)))
    ;; (find-file found-file)
    ;; (if (and (s-starts-with? "{" requested-word) (s-ends-with? "{" requested-word))
    ;;     (search-forward (format "class %s extends" requested-word))
    ;;   (search-forward "export default")))
	))

(defun webpack--imports-by-specifiers ()
  "Get information about imports, and create alist with them"
  '(("SpecialRatesSection" .
	 ((source . "midoffice/components/special-rates/SpecialRatesSection")
	  (imported . "SpecialRatesSection")
	  (kind . "named"))))
  )

;; (setq testv (webpack--import-by-specifiers))
;; (cdr (assoc 'name (cdr (assoc "SpecialRatesSection" testv))))

;; (webpack--find-declaration
;;  "SpecialRatesSection"
;;  (expand-file-name
;;   "~/projects/airborne/static/midoffice/js/companies/sections/SpecialRates.js"))

;; (webpack--find-declaration
;;  "SpecialRates"
;;  (expand-file-name
;;   "~/projects/airborne/static/midoffice/js/companies/sections/SpecialRates.js"))

;; (webpack--find-declaration
;;  "React"
;;  (expand-file-name
;;   "~/projects/airborne/static/midoffice/js/companies/sections/SpecialRates.js"))

;; (webpack--find-declaration
;;  "SpecialRatesSection"
;;  (expand-file-name
;;   "~/projects/airborne/static/midoffice/js/components/special-rates/SpecialRatesSection.js"))

;; (cdr
;;  (assoc
;;   'local
;;   (aref
;;    (cdr
;; 	(assoc 'specifiers
;; 		   (aref (cdr (assoc 'imported response)) 1))) 0)))

(provide 'webpack)  ;;; webpack.el ends here
