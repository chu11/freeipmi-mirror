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
(fi-load "ipmi-sensors-conf.scm")

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
    (if (list? sensor-list)
	(sensors-display-sensor-list sdr-record-list sensor-list 
				     verbose-count all-option))
    (if (and (boolean? group-name) (boolean? sensor-list))
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
   ((sensors-get-flush-cache-option cmd-args)
    (sensors-flush-cache))
   (else 
    (and (fi-ipmi-open cmd-args)
	 (begin 
	   (cond 
	    ((sensors-get-sdr-info-option cmd-args)
	     (sensors-display-sdr-info (fi-get-sdr-repo-info)))
	    ((sensors-get-list-group-option cmd-args)
	     (sensors-display-group-list))
	    (else 
	     (sensors-display-sensors cmd-args)))
	   (fi-ipmi-close))))))

(define (sensors args)
  "fish sensors main"
  (let ((cmd-args (sensors-argp (append (list "sensors") 
				    (list->strlist args)))))
    (if (list? cmd-args)
	(sensors-main cmd-args))))

(fi-register-command! 
 (list "sensors" 
       (string-append 
	"sensors [--no-probing] [--driver-type=IPMIDRIVER]\n"
	"        [--driver-address=DRIVERADDR] [--driver-device=DEVICE]\n"
	"        [--hostname=IPMIHOST] [--username=USERNAME]\n"
	"        [--password=PASSWORD] [--auth-type=AUTHTYPE]\n"
	"        [--priv-level=PRIVILEGE-LEVEL] [--verbose] [--sdr-info]\n"
	"        [--flush-cache] [--list-groups] [--all] [--group=GROUP]\n"
	"        [--sensors=SENSORS-LIST] [--help] [--usage] [--version]\n"
	"\n"
	"        Displays current readings of sensor chips through BMC.")))
