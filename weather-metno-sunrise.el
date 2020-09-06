;;; weather-metno-sunrise.el --- Sun/Moon rise/set from met.no in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Marian Piatkowski <marianpiatkowski@web.de>

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

(defun weather-metno-sunrise (&optional no-switch)
  "Display sunrise, moonrise, sunset, moonset etc."
  (interactive)
  (with-current-buffer (get-buffer-create "*Sunrise*")
    (save-excursion
      (let ((inhibit-read-only t))
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
        (let ((url (weather-metno-sunrise-url weather-metno-location-latitude weather-metno-location-longitude)))
          (url-retrieve url
                        (lambda (status start-time)
                          (message "The request is completed in %f seconds"
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
                              (kill-buffer)))
                          )
                        `(,(current-time))
                        'silent
                        'inhibit-cookies))
        ;; TODO this must go into the let* body for extracting contents from the XML tree
        (weather-metno--insert 'font-lock-function-name-face
                               "** Moon phase")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Moon shadow")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Moon position")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Solar midnight")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** High Moon")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Sunrise")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Moonset")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Solar noon")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Low Moon")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Sunset")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Moonrise")
        (insert "\n")
        (weather-metno--insert 'font-lock-function-name-face
                               "** Daytime (difference between sunset and sunrise)")
        (insert "\n")
        ;; end of TODO block
        (insert (format "%s" weather-metno--sunrise-data))
        (insert (propertize "\n\n\n"))
        (let* ((astrodata (car weather-metno--sunrise-data))
               ;; from the single node 'astrodata' get child 'location'
               (location (car (xml-get-children astrodata 'location)))
               ;; from location get child 'time' and denote it by 'times'
               (times (car (xml-get-children location 'time)))
               ;; from time (aka times) get child 'sunrise'
               (sunrise (car (xml-get-children times 'sunrise)))
               ;; get attributes from node 'sunrise'
               (sunrise-attrs (xml-node-attributes sunrise))
               ;; get the time of sunrise
               (sunrise-time (cdr (assq 'time sunrise-attrs)))
               )
          (message "%s" sunrise-time)
          (insert (propertize sunrise-time)))




        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (unless no-switch
    (switch-to-buffer "*Sunrise*")))

(provide 'weather-metno-sunrise)

;;; weather-metno-sunrise.el ends here
