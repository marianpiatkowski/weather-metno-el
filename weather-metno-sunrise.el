(require 'weather-metno)

(defvar weather-metno--sunrise-data nil
  "Sunrise data test variable.")

(defun weather-metno-sunrise-receive ()
  "Fetch sunrise and more from met.no for
`weather-metno-location-latitude', `weather-metno-location-longitude',
and `weather-metno-location-msl'."
  )

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
        ;; test for data fetch from url
        (let ((url "https://api.met.no/weatherapi/sunrise/2.0/?lat=49.42&lon=8.67&date=2020-09-02&offset=+02:00"))
          (url-retrieve url
                        (lambda (status start-time)
                          (message "The request is completed in %f seconds"
                                   (float-time (time-subtract nil start-time)))
                          (setq weather-metno--sunrise-data (buffer-string))
                          ;; (prog1 (buffer-string)
                          ;;   (insert " Klaus")
                          ;;   (kill-buffer)))
                          ;;(display-buffer (current-buffer)))
                          )
                        `(,(current-time))
                        'silent
                        'inhibit-cookies))
        (insert (propertize weather-metno--sunrise-data))
        (insert (propertize "\n\n\n"))



;; NOTE This is a hardcoded test...
;;      We had to replace " by \" in the string.
(let* ((xml
        "HTTP/1.1 200 OK
Server: nginx/1.10.3 (Ubuntu)
Date: Wed, 02 Sep 2020 21:38:09 GMT
Content-Type: text/xml
Content-Length: 817
Connection: keep-alive
Expires: Wed, 02 Sep 2020 21:40:26 GMT
Last-Modified: Wed, 02 Sep 2020 21:33:49 GMT
X-Backend-Host: b_ybs_api3_a3_api_met_no
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET
Access-Control-Allow-Headers: Origin
Content-Encoding: gzip
Vary: Accept, Accept-Encoding
X-Varnish: 408722066 408656994
Age: 260
Via: 1.1 varnish (Varnish/6.2)
Accept-Ranges: bytes

<?xml version=\"1.0\" encoding=\"utf-8\"?>
<astrodata xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://schema.api.met.no/schemas/astrodata-2.0.xsd\">
<meta licenseurl=\"https://api.met.no/license_data.html\"/>
  <location latitude=\"49.4\" longitude=\"8.6\" height=\"0\">
  <time date=\"2020-09-02\">
   <moonphase time=\"2020-09-02T00:00:00+02:00\" value=\"49.048441599\" desc=\"LOCAL MOON STATE * MOON PHASE= 49.0 (waxing gibbous)\"/>
   <moonshadow time=\"2020-09-02T00:00:00+02:00\" elevation=\"83.404591576\" azimuth=\"154.879758691\" desc=\"LOCAL MOON STATE * SHADOW ANGLES (azi=154.9,ele=83.4)\"/>
   <moonposition time=\"2020-09-02T00:00:00+02:00\" elevation=\"23.836385641\" azimuth=\"158.700316698\" range=\"395712.120175112\" phase=\"49.048441599\" desc=\"LOCAL MOON POSITION Elv: 23.836 deg, Azi: 158.700, Rng: 395712.1 km\"/>
   <solarmidnight time=\"2020-09-02T01:25:36+02:00\" elevation=\"-32.770656665\" desc=\"LOCAL DIURNAL MINIMUM SOLAR ELEVATION (Min= -32.77066)\"/>
   <high_moon time=\"2020-09-02T01:25:57+02:00\" elevation=\"26.749819281\" desc=\"LOCAL DIURNAL MAXIMUM MOON ELEVATION (Max= 26.74982)\"/>
   <moonset time=\"2020-09-02T06:31:17+02:00\" desc=\"LOCAL DIURNAL MOON SET\"/>
   <sunrise time=\"2020-09-02T06:43:30+02:00\" desc=\"LOCAL DIURNAL SUN RISE\"/>
   <solarnoon time=\"2020-09-02T13:24:53+02:00\" elevation=\"48.247122284\" desc=\"LOCAL DIURNAL MAXIMUM SOLAR ELEVATION (Max= 48.24712)\"/>
   <low_moon time=\"2020-09-02T13:40:58+02:00\" elevation=\"-51.926485949\" desc=\"LOCAL DIURNAL MINIMUM MOON ELEVATION (Min= -51.92649)\"/>
   <sunset time=\"2020-09-02T20:05:47+02:00\" desc=\"LOCAL DIURNAL SUN SET\"/>
   <moonrise time=\"2020-09-02T20:42:41+02:00\" desc=\"LOCAL DIURNAL MOON RISE\"/>
  </time>
  <time date=\"2020-09-03\">
   <moonposition time=\"2020-09-03T00:00:00+02:00\" elevation=\"24.938602523\" azimuth=\"146.216777809\" range=\"398261.254584113\" phase=\"52.182959159\" desc=\"LOCAL MOON POSITION Elv: 24.939 deg, Azi: 146.217, Rng: 398261.3 km\"/>
  </time>
  </location>
</astrodata>"
            )
       (root (with-temp-buffer
	       (insert xml)
	       (xml-parse-region (point-min) (point-max))))
       ;; no we have the single node called 'astrodata'
       (astrodata (car root))
       ;; from astrodata get child 'location'
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
