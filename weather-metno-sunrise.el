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

;;;###autoload
(defun weather-metno-sunrise (&optional no-switch)
  "Display sunrise, moonrise, sunset, moonset etc.
If NO-SWITCH is non-nil then do not switch to weather forecast buffer."
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
                              (kill-buffer)))
                          )
                        `(,(current-time))
                        'silent
                        'inhibit-cookies))
        (let* ((astrodata (car weather-metno--sunrise-data))
               ;; from the single node 'astrodata' get child 'location'
               (location (car (xml-get-children astrodata 'location)))
               ;; from location get child 'time' and denote it by 'times'
               (times (car (xml-get-children location 'time)))
               ;; ---
               ;; from time (aka times) get child 'moonphase'
               (moonphase (car (xml-get-children times 'moonphase)))
               ;; get attributes from node 'moonphase'
               (moonphase-attrs (xml-node-attributes moonphase))
               ;; get the time of moonphase
               (moonphase-time (cdr (assq 'time moonphase-attrs)))
               ;; get the value of moonphase
               (moonphase-value (cdr (assq 'value moonphase-attrs)))
               ;; get the desc of moonphase
               (moonphase-desc (cdr (assq 'desc moonphase-attrs)))
               ;; ---
               (moonshadow (car (xml-get-children times 'moonshadow)))
               (moonshadow-attrs (xml-node-attributes moonshadow))
               (moonshadow-time (cdr (assq 'time moonshadow-attrs)))
               (moonshadow-elevation (cdr (assq 'elevation moonshadow-attrs)))
               (moonshadow-azimuth (cdr (assq 'azimuth moonshadow-attrs)))
               (moonshadow-desc (cdr (assq 'desc moonshadow-attrs)))
               (moonposition (car (xml-get-children times 'moonposition)))
               (moonposition-attrs (xml-node-attributes moonposition))
               (moonposition-time (cdr (assq 'time moonposition-attrs)))
               (moonposition-elevation (cdr (assq 'elevation moonposition-attrs)))
               (moonposition-azimuth (cdr (assq 'azimuth moonposition-attrs)))
               (moonposition-range (cdr (assq 'range moonposition-attrs)))
               (moonposition-phase (cdr (assq 'phase moonposition-attrs)))
               (moonposition-desc (cdr (assq 'desc moonposition-attrs)))
               (solarmidnight (car (xml-get-children times 'solarmidnight)))
               (solarmidnight-attrs (xml-node-attributes solarmidnight))
               (solarmidnight-time (cdr (assq 'time solarmidnight-attrs)))
               (solarmidnight-elevation (cdr (assq 'elevation solarmidnight-attrs)))
               (solarmidnight-desc (cdr (assq 'desc solarmidnight-attrs)))
               (high_moon (car (xml-get-children times 'high_moon)))
               (high_moon-attrs (xml-node-attributes high_moon))
               (high_moon-time (cdr (assq 'time high_moon-attrs)))
               (high_moon-elevation (cdr (assq 'elevation high_moon-attrs)))
               (high_moon-desc (cdr (assq 'desc high_moon-attrs)))
               (sunrise (car (xml-get-children times 'sunrise)))
               (sunrise-attrs (xml-node-attributes sunrise))
               (sunrise-time (cdr (assq 'time sunrise-attrs)))
               (sunrise-desc (cdr (assq 'desc sunrise-attrs)))
               (moonset (car (xml-get-children times 'moonset)))
               (moonset-attrs (xml-node-attributes moonset))
               (moonset-time (cdr (assq 'time moonset-attrs)))
               (moonset-desc (cdr (assq 'desc moonset-attrs)))
               (solarnoon (car (xml-get-children times 'solarnoon)))
               (solarnoon-attrs (xml-node-attributes solarnoon))
               (solarnoon-time (cdr (assq 'time solarnoon-attrs)))
               (solarnoon-elevation (cdr (assq 'elevation solarnoon-attrs)))
               (solarnoon-desc (cdr (assq 'desc solarnoon-attrs)))
               (low_moon (car (xml-get-children times 'low_moon)))
               (low_moon-attrs (xml-node-attributes low_moon))
               (low_moon-time (cdr (assq 'time low_moon-attrs)))
               (low_moon-elevation (cdr (assq 'elevation low_moon-attrs)))
               (low_moon-desc (cdr (assq 'desc low_moon-attrs)))
               (sunset (car (xml-get-children times 'sunset)))
               (sunset-attrs (xml-node-attributes sunset))
               (sunset-time (cdr (assq 'time sunset-attrs)))
               (sunset-desc (cdr (assq 'desc sunset-attrs)))
               (moonrise (car (xml-get-children times 'moonrise)))
               (moonrise-attrs (xml-node-attributes moonrise))
               (moonrise-time (cdr (assq 'time moonrise-attrs)))
               (moonrise-desc (cdr (assq 'desc moonrise-attrs)))
               )
          (unless (null (cdr moonphase))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Moon phase")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Value "
                                   moonphase-value)
            (weather-metno-insert-moonphase (string-to-number moonphase-value))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   moonphase-desc)
            (insert "\n"))
          (unless (null (cdr moonshadow))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Moon shadow")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   moonshadow-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Azimuth "
                                   moonshadow-azimuth)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   moonshadow-desc)
            (insert "\n"))
          (unless (null (cdr moonposition))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Moon position")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   moonposition-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Azimuth "
                                   moonposition-azimuth)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Range "
                                   moonposition-range)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Phase "
                                   moonposition-phase)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   moonposition-desc)
            (insert "\n"))
          (unless (null (cdr solarmidnight))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Solar midnight")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time solarmidnight-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   solarmidnight-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   solarmidnight-desc)
            (insert "\n"))
          (unless (null (cdr high_moon))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** High Moon")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time high_moon-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   high_moon-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   high_moon-desc)
            (insert "\n"))
          (unless (null (cdr sunrise))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Sunrise")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time sunrise-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   sunrise-desc)
            (insert "\n"))
          (unless (null (cdr moonset))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Moonset")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time moonset-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   moonset-desc)
            (insert "\n"))
          (unless (null (cdr solarnoon))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Solar noon")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time solarnoon-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   solarnoon-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   solarnoon-desc)
            (insert "\n"))
          (unless (null (cdr low_moon))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Low Moon")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time low_moon-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Elevation "
                                   low_moon-elevation)
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   low_moon-desc)
            (insert "\n"))
          (unless (null (cdr sunset))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Sunset")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time sunset-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   sunset-desc)
            (insert "\n"))
          (unless (null (cdr moonrise))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Moonrise")
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Time "
                                   (weather-metno--format-sunrise-time moonrise-time))
            (insert "\n")
            (weather-metno--insert 'font-lock-keyword-face
                                   "*** Desc "
                                   moonrise-desc)
            (insert "\n"))
          (unless (or (null (cdr sunset)) (null (cdr sunrise)))
            (weather-metno--insert 'font-lock-function-name-face
                                   "** Daytime "
                                   (weather-metno--calculate-time-difference sunset-time sunrise-time))
            (insert "\n")))
        )) ;; end of let and save-excursion
    (goto-char (point-min)))
  (unless no-switch
    (switch-to-buffer "*Sunrise*")))

(provide 'weather-metno-sunrise)

;;; weather-metno-sunrise.el ends here
