;;; sensors.scm: IPMI Sensors
;;; authors: Balamurugan <bala.a@californiadigital.com>

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

(fi-load "sensors-common.scm")
(fi-load "sdr.scm")
(fi-load "sensors-simple-display.scm")
(fi-load "sensors-verbose-display.scm")
(fi-load "sensors-very-verbose-display.scm")
(fi-load "sensors-alias.scm")

(define (sensors-display-group-sensors sdr-record-list group-name 
				       verbose-count all-option)
  (for-each 
   (lambda (sdr-record)
     (let ((sdr-group-name (assoc-ref sdr-record "group_name"))
	   (record-id (assoc-ref sdr-record "record_id")))
       (if (and (not all-option) (sensors-ignored? record-id))
	   '()
	   (if (string? sdr-group-name)
	       (if (or (string-ci=? sdr-group-name group-name) 
		       (member sdr-group-name (sensors-get-aliased-groups group-name)))
		   (cond 
		    ((= verbose-count 0)
		     (sensors-display-simple sdr-record (fi-get-sensor-reading 
							 sdr-record)))
		    ((= verbose-count 1)
		     (sensors-display-verbose sdr-record (fi-get-sensor-reading 
							  sdr-record)))
		    ((>= verbose-count 2)
		     (sensors-display-very-verbose sdr-record 
						   (fi-get-sensor-reading 
						    sdr-record)))))))))
   sdr-record-list))

(define (sensors-display-sensor-list sdr-record-list sensor-list 
				     verbose-count all-option)
  (for-each 
   (lambda (sdr-record)
     (let ((record-id (assoc-ref sdr-record "record_id")))
       (if (and (not all-option) (sensors-ignored? record-id))
	   '()
	   (if (member record-id sensor-list)
	       (cond 
		((= verbose-count 0)
		 (sensors-display-simple sdr-record (fi-get-sensor-reading sdr-record)))
		((= verbose-count 1)
		 (sensors-display-verbose sdr-record (fi-get-sensor-reading sdr-record)))
		((>= verbose-count 2)
		 (sensors-display-very-verbose sdr-record 
					       (fi-get-sensor-reading sdr-record))))))))
   sdr-record-list))

(define (sensors-display-sensors cmd-args)
  (let ((verbose-count   (sensors-get-verbose-option cmd-args))
	(all-option      (sensors-get-all-option cmd-args))
	(group-name      (sensors-get-group-option cmd-args))
	(sensor-list     (sensors-get-sensors-option cmd-args))
	(sdr-record-list #f))
    (if (init-sdr-cache)
	(set! sdr-record-list c-sdr-record-list)
	(set! sdr-record-list '()))
    (if (string? group-name)
	(sensors-display-group-sensors sdr-record-list group-name 
				       verbose-count all-option))
    (if (not (null? sensor-list))
	(sensors-display-sensor-list sdr-record-list sensor-list 
				     verbose-count all-option))
    (if (and (boolean? group-name) (null? sensor-list))
	(for-each 
	 (lambda (sdr-record)
	   (let ((record-id (assoc-ref sdr-record "record_id")))
	     (if (and (not all-option) (sensors-ignored? record-id))
		 '()
		 (cond 
		  ((= verbose-count 0)
		   (sensors-display-simple sdr-record (fi-get-sensor-reading sdr-record)))
		  ((= verbose-count 1)
		   (sensors-display-verbose sdr-record (fi-get-sensor-reading sdr-record)))
		  ((>= verbose-count 2)
		   (sensors-display-very-verbose sdr-record 
						 (fi-get-sensor-reading sdr-record)))))))
	 sdr-record-list))))

(define (sensors-main cmd-args)
  (cond 
   ((sensors-get-help-option cmd-args)
    (sensors-display-help))
   ((sensors-get-usage-option cmd-args)
    (sensors-display-usage))
   ((sensors-get-version-option cmd-args)
    (sensors-display-version))
   ((sensors-get-sdr-info-option cmd-args)
    (sensors-display-sdr-info (fi-get-sdr-repo-info)))
   ((sensors-get-flush-cache-option cmd-args)
    (sensors-flush-cache))
   ((sensors-get-list-group-option cmd-args)
    (sensors-display-group-list))
   (else 
    (sensors-display-sensors cmd-args))))

(define (sensors args)
  "fish sensors main"
  (set! args (list->strlist args))
  (catch 'misc-error
	 (lambda ()
	   (sensors-main (append '("sensors") args)))
	 (lambda (k args . opts)
	   (display "sensors: error: ")
	   (display (cadr opts))
	   (newline))))

(fi-register-command! 
 '("sensors" 
   "sensors --version --usage --help --verbose --sdr-info --flush-cache --list-groups --group=GROUP-NAME --sensors \"SENSORS-LIST\"\n\tDisplay IPMI Sensors.\n"))

; (catch 'misc-error 
;        (lambda ()
; 	 (let* ((cmd-args    (sensors-argp-parse (fi-command-line)))
; 		(sensor-list (sensors-get-sensors-option cmd-args))
; 		(group-name  (sensors-get-group-option cmd-args)))
; 	   (cond 
; 	    ((sensors-get-extra-option cmd-args)
; 	     (begin 
; 	       (set! sensors-exit-status 1)
; 	       (display sensors-program-short-name (current-error-port))
; 	       (display ": error: invalid option "
; 			(current-error-port))
; 	       (display (sensors-get-extra-option cmd-args)
; 			(current-error-port))
; 	       (display "\n" (current-error-port))))
; 	    ((list? (member #f (map number? sensor-list)))
; 	     (begin 
; 	       (set! sensors-exit-status 1)
; 	       (display sensors-program-short-name (current-error-port))
; 	       (display ": error: non-numeric sensor number\n"
; 			(current-error-port))))
; 	    ((and (string? group-name) 
; 		  (not (member #t 
; 			       (map (lambda (arg) (string-ci=? group-name arg)) 
; 				    (fi-sensors-get-group-list))))
; 		  (not (sensors-alias? group-name)))
; 	     (begin 
; 	       (set! sensors-exit-status 1)
; 	       (display sensors-program-short-name (current-error-port))
; 	       (display ": error: unknown group name\n"
; 			(current-error-port))))
; 	    (else 
; 	     (sensors-main cmd-args)))))
;        (lambda (k args . opts)
; 	 (set! sensors-exit-status 1)
; 	 (display sensors-program-short-name (current-error-port))
; 	 (display ": error: " (current-error-port))
; 	 (display (cadr opts) (current-error-port))
; 	 (display "\n" (current-error-port))))
; (fi-exit sensors-exit-status)

