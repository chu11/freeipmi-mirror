;;; sensors-simple-display.scm: sensors simple display procedures
;;; authors: Balamurugan <bala@research.com>

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

; (use-modules (ice-9 format))

(define (sensors-display-simple-full-record sdr-record sensor-reading)
  (let ((record-id  (assoc-ref sdr-record "record_id"))
	(sensor-name       (assoc-ref sdr-record "sensor_name"))
	(group-name        (assoc-ref sdr-record "group_name"))
	(event-reading-type (assoc-ref sdr-record "event_reading_type"))
	(lower-critical    (assoc-ref sdr-record "lower_critical_threshold"))
	(upper-critical    (assoc-ref sdr-record "upper_critical_threshold"))
	(is-lower-critical (assoc-ref sdr-record "readable_lower_critical_threshold"))
	(is-upper-critical (assoc-ref sdr-record "readable_upper_critical_threshold"))
	(unit-short-string (assoc-ref sdr-record "unit_short_string"))
	(current-reading    (assoc-ref sensor-reading "current_reading"))
	(event-message-list (assoc-ref sensor-reading "event_message_list")))
    (format #t "~d: ~a (~a):" record-id sensor-name group-name)
    (if (= event-reading-type 1)
	(begin
	  (if (boolean? sensor-reading)
	      (format #t " NA")
	      (format #t " ~f ~a" current-reading unit-short-string))
	  (if is-lower-critical
	      (format #t " (~f/" lower-critical)
	      (format #t " (NA/"))
	  (if is-upper-critical
	      (format #t "~f):" upper-critical)
	      (format #t "NA):"))
	  (if (boolean? sensor-reading)
	      (format #t " [~a]~%" "Unknown")
	      (if (null? event-message-list)
		  (format #t " [~a]~%" "OK")
		  (begin 
		    (for-each (lambda (event-message)
				(format #t " [~a]" event-message))
			      event-message-list)
		    (newline)))))
	(if (null? event-message-list)
	    (format #t " [~a]~%" "OK")
	    (begin 
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline))))))

(define (sensors-display-simple-compact-record sdr-record sensor-reading)
  (let ((record-id  (assoc-ref sdr-record "record_id"))
	(sensor-name   (assoc-ref sdr-record "sensor_name"))
	(group-name    (assoc-ref sdr-record "group_name"))
	(event-message-list (assoc-ref sensor-reading "event_message_list")))
    (format #t "~d: ~a (~a):" record-id sensor-name group-name)
    (if (boolean? sensor-reading)
	(format #t " [~a]~%" "Unknown")
	(if (null? event-message-list)
	    (format #t " [~a]~%" "OK")
	    (begin 
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline))))))

(define (sensors-display-simple-event-only-record sdr-record sensor-reading)
  (let ((record-id  (assoc-ref sdr-record "record_id"))
	(sensor-name   (assoc-ref sdr-record "sensor_name"))
	(group-name    (assoc-ref sdr-record "group_name"))
	(event-message-list (assoc-ref sensor-reading "event_message_list")))
    (format #t "~d: ~a (~a):" record-id sensor-name group-name)
    (if (boolean? sensor-reading)
	(format #t " [~a]~%" "Unknown")
	(if (null? event-message-list)
	    (format #t " [~a]~%" "OK")
	    (begin 
	      (for-each (lambda (event-message)
			  (format #t " [~a]" event-message))
			event-message-list)
	      (newline))))))

(define (sensors-display-simple sdr-record sensor-reading)
  (let ((record-type (assoc-ref sdr-record "record_type")))
    (cond 
     ((= record-type FI-SDR-FULL-RECORD)
      (begin (sensors-display-simple-full-record sdr-record sensor-reading) #t))
     ((= record-type FI-SDR-COMPACT-RECORD)
      (begin (sensors-display-simple-compact-record sdr-record sensor-reading) #t))
     (else #f))))
