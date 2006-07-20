;;; sensors-verbose-display.scm: sensors verbose display procedures
;;; authors: Balamurugan <bala@zresearch.com>

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA

(use-modules (ice-9 format))

(define (sensors-display-verbose-full-record sdr-record sensor-reading)
  (let ((record-id  (assoc-ref sdr-record "record_id"))
	(sensor-name    (assoc-ref sdr-record "sensor_name"))
	(group-name     (assoc-ref sdr-record "group_name"))
	(sensor-number  (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(lower-critical        (assoc-ref sdr-record "lower_critical_threshold"))
	(upper-critical        (assoc-ref sdr-record "upper_critical_threshold"))
	(lower-non-critical    (assoc-ref sdr-record "lower_non_critical_threshold"))
	(upper-non-critical    (assoc-ref sdr-record "upper_non_critical_threshold"))
	(lower-non-recoverable (assoc-ref sdr-record "lower_non_recoverable_threshold"))
	(upper-non-recoverable (assoc-ref sdr-record "upper_non_recoverable_threshold"))
	(is-lower-critical (assoc-ref sdr-record "readable_lower_critical_threshold"))
	(is-upper-critical (assoc-ref sdr-record "readable_upper_critical_threshold"))
	(is-lower-non-critical (assoc-ref sdr-record 
					  "readable_lower_non_critical_threshold"))
	(is-upper-non-critical (assoc-ref sdr-record 
					  "readable_upper_non_critical_threshold"))
	(is-lower-non-recoverable (assoc-ref sdr-record 
					     "readable_lower_non_recoverable_threshold"))
	(is-upper-non-recoverable (assoc-ref sdr-record 
					     "readable_upper_non_recoverable_threshold"))
	(sensor-min-reading (assoc-ref sdr-record "sensor_min_reading"))
	(sensor-max-reading (assoc-ref sdr-record "sensor_max_reading"))
	(normal-min (assoc-ref sdr-record "normal_min"))
	(normal-max (assoc-ref sdr-record "normal_max"))
	(nominal-reading (assoc-ref sdr-record "nominal_reading"))
	(unit-string   (assoc-ref sdr-record "unit_string"))
	(current-reading     (assoc-ref sensor-reading "current_reading"))
	(event-message-list  (assoc-ref sensor-reading "event_message_list")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (if (= event-reading-type 1)
	(begin
	  (if is-lower-critical
	      (format #t "Lower Critical Threshold: ~f ~a~%" lower-critical unit-string)
	      (format #t "Lower Critical Threshold: ~a~%" "NA"))
	  (if is-upper-critical
	      (format #t "Upper Critical Threshold: ~f ~a~%" upper-critical unit-string)
	      (format #t "Upper Critical Threshold: ~a~%" "NA"))
	  (if is-lower-non-critical
	      (format #t "Lower Non-Critical Threshold: ~f ~a~%" lower-non-critical unit-string)
	      (format #t "Lower Non-Critical Threshold: ~a~%" "NA"))
	  (if is-upper-non-critical
	      (format #t "Upper Non-Critical Threshold: ~f ~a~%" upper-non-critical unit-string)
	      (format #t "Upper Non-Critical Threshold: ~a~%" "NA"))
	  (if is-lower-non-recoverable
	      (format #t "Lower Non-Recoverable Threshold: ~f ~a~%" 
		      lower-non-recoverable unit-string)
	      (format #t "Lower Non-Recoverable Threshold: ~a~%" "NA"))
	  (if is-upper-non-recoverable
	      (format #t "Upper Non-Recoverable Threshold: ~f ~a~%" 
		      upper-non-recoverable unit-string)
	      (format #t "Upper Non-Recoverable Threshold: ~a~%" "NA"))
	  (format #t "Sensor Min. Reading: ~f ~a~%" sensor-min-reading unit-string)
	  (format #t "Sensor Max. Reading: ~f ~a~%" sensor-max-reading unit-string)
	  (format #t "Normal Min.: ~f ~a~%" normal-min unit-string)
	  (format #t "Normal Max.: ~f ~a~%" normal-max unit-string)
	  (format #t "Nominal reading: ~f ~a~%" nominal-reading unit-string)
	  (if (boolean? sensor-reading)
	      (format #t "Sensor Reading: ~a~%" "NA")
	      (format #t "Sensor Reading: ~f ~a~%" current-reading unit-string))))
    (if (boolean? sensor-reading)
	(format #t "Sensor Status: [~a]~%~%" "Unknown")
	(if (null? event-message-list)
	    (format #t "Sensor Status: [~a]~%~%" "OK")
	    (begin 
	      (format #t "Sensor Status:")
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline)
	      (newline))))))

(define (sensors-display-verbose-compact-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(sensor-name        (assoc-ref sdr-record "sensor_name"))
	(group-name         (assoc-ref sdr-record "group_name"))
	(sensor-number      (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(event-message-list      (assoc-ref sensor-reading "event_message_list")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (if (boolean? sensor-reading)
	(format #t "Sensor Status: [~a]~%~%" "Unknown")
	(if (null? event-message-list)
	    (format #t "Sensor Status: [~a]~%~%" "OK")
	    (begin 
	      (format #t "Sensor Status:")
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline)
	      (newline))))))


(define (sensors-display-verbose-event-only-record sdr-record sensor-reading)
  (let ((record-id          (assoc-ref sdr-record "record_id"))
	(sensor-name        (assoc-ref sdr-record "sensor_name"))
	(group-name         (assoc-ref sdr-record "group_name"))
	(sensor-number      (assoc-ref sdr-record "sensor_number"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(event-message-list      (assoc-ref sensor-reading "event_message_list")))
    (format #t "Record ID: ~d~%" record-id)
    (format #t "Sensor Name: ~a~%" sensor-name)
    (format #t "Group Name: ~a~%" group-name)
    (format #t "Sensor Number: ~d~%" sensor-number)
    (format #t "Event/Reading Type Code: ~ah~%" (get-hex-string event-reading-type))
    (if (boolean? sensor-reading)
	(format #t "Sensor Status: [~a]~%~%" "Unknown")
	(if (null? event-message-list)
	    (format #t "Sensor Status: [~a]~%~%" "OK")
	    (begin 
	      (format #t "Sensor Status:")
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline)
	      (newline))))))

(define (sensors-display-verbose sdr-record sensor-reading)
  (let ((record-type (assoc-ref sdr-record "record_type")))
    (cond 
     ((= record-type FI-SDR-FULL-RECORD)
      (begin (sensors-display-verbose-full-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-COMPACT-RECORD)
      (begin (sensors-display-verbose-compact-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-EVENT-ONLY-RECORD)
      (begin (sensors-display-verbose-event-only-record sdr-record sensor-reading) #t))
     (else #f))))
