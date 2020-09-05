(require 'weather-metno)

(defvar weather-metno--sunrise-data nil
  "Store sunrise data from `xml-parse-region'.")

(defun weather-metno-sunrise-url (lat lon &optional height)
  "Create the url from LAT, LON and MSL to be used by `weather-metno-sunrise'."
  ;; TODO offset only works for integer values correctly
  (format "%ssunrise/%s/?lat=%s&lon=%s&date=%s&offset=+%s:00"
          weather-metno-url weather-metno-forecast-version lat lon
          (format-time-string "%Y-%m-%d")
          (format "%02d" (/ (car (current-time-zone)) 3600))))

(defun weather-metno-sunrise (&optional no-switch)
  "Display sunrise, moonrise, sunset, moonset etc."
  (interactive)
  (with-current-buffer (get-buffer-create "*Sunrise*")
    (save-excursion
      (let ((inhibit-read-only t))
        (weather-metno-forecast-mode)
        (erase-buffer)
        (goto-char (point-min))
        ;; see weather-metno--insert
        (insert (propertize "** Hello Sunrise\n"))
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
