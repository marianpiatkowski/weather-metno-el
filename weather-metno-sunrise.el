;;; weather-metno-sunrise.el --- Sun/Moon rise/set from met.no in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020- Marian Piatkowski <marianpiatkowski@web.de>

;; Author: Marian Piatkowski <marianpiatkowski@web.de>
;; URL: https://github.com/marianpiatkowski/weather-metno-el
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; weather-metno-el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; weather-metno-el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with weather-metno-el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See http://api.met.no/weatherapi/documentation
;; and http://api.met.no/license_data.html

;;; Code:

(require 'weather-metno)

(defvar weather-metno--sunrise-data nil
  "Store sunrise data from `xml-parse-region'.")

(defun weather-metno-kill-sunrise-buffer ()
  (interactive)
  (kill-buffer "*Sunrise*"))

(defvar weather-metno-sunrise-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'weather-metno-kill-sunrise-buffer)
    (define-key map "g" 'weather-metno-sunrise)
    map)
  "Keymap for `weather-metno-sunrise-mode'.")

(eval-when-compile (require 'easymenu))
(easy-menu-define weather-metno-sunrise-mode weather-metno-sunrise-mode-map
  "Menu for Weather Metno Sunrise."
  '("Sun/Moon rise/set"
    ["Update" weather-metno-sunrise
     :help "Fetch new data from met.no"]
    ["Quit" weather-metno-kill-sunrise-buffer
     :help "Quit"]))

(define-derived-mode weather-metno-sunrise-mode special-mode
  "metno-sunrise"
  "Major mode for showing sun/moon rise/set.

\\{weather-metno-sunrise-mode-map}"
  :group 'weather-metno)

(defun weather-metno-sunrise-url (lat lon &optional height)
  "Create the url from LAT, LON and MSL to be used by `weather-metno-sunrise'."
  (format "%ssunrise/%s/?lat=%s&lon=%s&date=%s&offset=+%s"
          weather-metno-url weather-metno-forecast-version lat lon
          (format-time-string "%Y-%m-%d")
          (format-seconds "%02h:%02m" (car (current-time-zone)))))

(defun weather-metno--format-sunrise-time (time-string)
  "Parse a RFC3339 compliant TIME-STRING."
  (let ((d (weather-metno--parse-time-string time-string)))
    (format "%02d:%02d:%02d" (nth 2 d) (nth 1 d) (nth 0 d))))

(defun weather-metno--calculate-time-difference (time-string1 time-string2)
  "Calculate time difference from two RFC3339 compliant TIME-STRINGS."
  (format-seconds "%Y, %D, %H, %M, %z%S"
                  (time-to-seconds (time-subtract (apply 'encode-time (weather-metno--parse-time-string time-string1))
                                                  (apply 'encode-time (weather-metno--parse-time-string time-string2))))))

(defun weather-metno-insert-moonphase (value)
  "Insert Moon Phase icon in the current buffer at point."
  (let ((moonphase-filename (format "%smoon-phases/icons8/cute-color/%s/"
                                    (file-name-directory (symbol-file 'weather-metno-sunrise-url))
                                    (if (> weather-metno-location-latitude 0) "northern" "southern"))))
    (cond ((and (<= 0 value) (<= value 1))
           (setq moonphase-filename (concat moonphase-filename "new-moon.png")))
          ((and (< 1 value) (<= value 24))
           (setq moonphase-filename (concat moonphase-filename "waxing-crescent-moon.png")))
          ((and (< 24 value) (<= value 26))
           (setq moonphase-filename (concat moonphase-filename "first-quarter-moon.png")))
          ((and (< 26 value) (<= value 49))
           (setq moonphase-filename (concat moonphase-filename "waxing-gibbous-moon.png")))
          ((and (< 49 value) (<= value 51))
           (setq moonphase-filename (concat moonphase-filename "full-moon.png")))
          ((and (< 51 value) (<= value 74))
           (setq moonphase-filename (concat moonphase-filename "waning-gibbous-moon.png")))
          ((and (< 74 value) (<= value 76))
           (setq moonphase-filename (concat moonphase-filename "last-quarter-moon.png")))
          ((and (< 76 value) (<= value 99))
           (setq moonphase-filename (concat moonphase-filename "waning-crescent-moon.png")))
          ((and (< 99 value) (<= value 100))
           (setq moonphase-filename (concat moonphase-filename "new-moon.png"))))
    (when (file-exists-p moonphase-filename)
      (insert-image (create-image moonphase-filename)))))

;;; ======== formatting functions for items in *Sunrise* buffer ========

(defun weather-metno-sunrise--format-moonphase (node)
  "Format attributes of moon phase."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Moon phase\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Value "
                         (xml-get-attribute node 'value))
  (weather-metno-insert-moonphase (string-to-number (xml-get-attribute node 'value)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-moonshadow (node)
  "Format attributes of moon shadow."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Moon shadow\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Azimuth "
                         (xml-get-attribute node 'azimuth))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-moonposition (node)
  "Format attributes of moon position."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Moon position\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Azimuth "
                         (xml-get-attribute node 'azimuth))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Range "
                         (xml-get-attribute node 'range))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Phase "
                         (xml-get-attribute node 'phase))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-moonrise (node)
  "Format attributes of moonrise."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Moonrise\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-solarmidnight (node)
  "Format attributes of solar midnight."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Solar midnight\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-high_moon (node)
  "Format attributes of high moon."
  (weather-metno--insert 'font-lock-function-name-face
                         "** High Moon\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-sunrise (node)
  "Format attributes of sunrise."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Sunrise\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-moonset (node)
  "Format attributes of moonset."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Moonset\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-solarnoon (node)
  "Format attributes of solar noon."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Solar noon\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-sunset (node)
  "Format attributes of sunset."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Sunset\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-low_moon (node)
  "Format attributes of low moon."
  (weather-metno--insert 'font-lock-function-name-face
                         "** Low Moon\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Time "
                         (weather-metno--format-sunrise-time (xml-get-attribute node 'time)))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Elevation "
                         (xml-get-attribute node 'elevation))
  (insert "\n")
  (weather-metno--insert 'font-lock-keyword-face
                         "*** Desc "
                         (xml-get-attribute node 'desc))
  (insert "\n"))

(defun weather-metno-sunrise--format-daytime (node)
  "Format daytime."
  (let* ((sunset-node (car (xml-get-children node 'sunset)))
         (sunrise-node (car (xml-get-children node 'sunrise)))
         )
    (unless (or (null (cdr sunset-node)) (null (cdr sunrise-node)))
      (weather-metno--insert 'font-lock-function-name-face
                             "** Daytime "
                             (weather-metno--calculate-time-difference (xml-get-attribute sunset-node 'time)
                                                                       (xml-get-attribute sunrise-node 'time)))
      (insert "\n"))))

;;; ======== end of formatting functions ========

;;;###autoload
(defun weather-metno-sunrise-build-buffer ()
  "Display sunrise, moonrise, sunset, moonset etc."
  (interactive)
  (with-current-buffer (get-buffer-create "*Sunrise*")
    (save-excursion
      (let ((inhibit-read-only t))
        (remove-images (point-min) (point-max))

        (weather-metno-sunrise-mode)
        (erase-buffer)
        (goto-char (point-min))
        (weather-metno--insert 'weather-metno-header
                               (concat "Sun/moon rise/set for "
                                       weather-metno-location-name
                                       "\n"))
        (weather-metno--insert 'weather-metno-date
                               "* For "
                               (format-time-string "%A %Y-%m-%d") "\n")
        (let* ((astrodata (car weather-metno--sunrise-data))
               (location-node (car (xml-get-children astrodata 'location)))
               (time-node (car (xml-get-children location-node 'time)))
               )
          (dolist (entry
                   ;; remove dummy child nodes that contain "\n  " as a substring
                   (-remove (lambda (entry) (cl-search "\n  " entry)) (xml-node-children time-node)))
            (let ((formatter (intern (concat "weather-metno-sunrise--format-"
                                             (symbol-name (car entry))))))
              (if (fboundp formatter)
                  (funcall formatter entry)
                (insert (format "Unknown entry %s\n" entry)))))
          ;; calculate daytime as well
          (weather-metno-sunrise--format-daytime time-node))
        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (switch-to-buffer "*Sunrise*"))

;;;###autoload
(defun weather-metno-sunrise ()
  "Update sunrise, moonrise, sunset, moonset etc. data."
  (interactive)
  (let ((url (weather-metno-sunrise-url weather-metno-location-latitude
                                        weather-metno-location-longitude)))
    (url-retrieve url
                  (lambda (status start-time)
                    (message "The request completed in %f seconds"
                             (float-time (time-subtract nil start-time)))
                    (save-excursion
                      (goto-char (point-min))
                      (unless (search-forward "\n\n" nil t)
                        (kill-buffer)
                        (error "Error in http reply"))
                      (let ((headers (buffer-substring (point-min) (point))))
                        (unless (string-match-p
                                 (concat "^HTTP/1.1 "
                                         "\\(200 OK\\|203 "
                                         "Non-Authoritative Information\\)")
                                 headers)
                          (kill-buffer)
                          (error "Unable to fetch data"))
                        (url-store-in-cache (current-buffer))
                        (setq weather-metno--sunrise-data (xml-parse-region (point) (point-max)))
                        (kill-buffer))
                      (weather-metno-sunrise-build-buffer))
                    )
                  `(,(current-time))
                  'silent
                  'inhibit-cookies))
  )

(provide 'weather-metno-sunrise)

;;; weather-metno-sunrise.el ends here
