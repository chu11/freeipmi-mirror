;;; sensors-common.scm: sensors common procedures
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

(use-modules (ice-9 getopt-long))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-14))
(use-modules (ice-9 format))

(define FI-SDR-FULL-RECORD                        #x01)
(define FI-SDR-COMPACT-RECORD                     #x02)
(define FI-SDR-EVENT-ONLY-RECORD                  #x03)
(define FI-SDR-ENTITY-ASSO-RECORD                 #x08)
(define FI-SDR-DEV-ENTITY-ASSO-RECORD             #x09)
(define FI-SDR-GEN-DEV-LOCATOR-RECORD             #x10)
(define FI-SDR-FRU-DEV-LOCATOR-RECORD             #x11)
(define FI-SDR-MGMT-CNTRLR-DEV-LOCATOR-RECORD     #x12)
(define FI-SDR-MGMT-CNTRLR-CONFIRMATION-RECORD    #x13)
(define FI-SDR-BMC-MSG-CHANNEL-INFO-RECORD        #x14)
(define FI-SDR-OEM-RECORD                         #xC0)

(define sensors-sdr-cache-file     (fi-sensors-get-default-cache-filename))
(define sensors-program-short-name "ipmi-sensors")
(define sensors-exit-status        0)
(define sensors-ignored-list       '())

(define (sensors-ignore! ignored-list)
  "ignore this list of sensors"
  (set! sensors-ignored-list (append sensors-ignored-list ignored-list)))

(define (sensors-ignored? sensor-id)
  "check if this SENSOR-ID is ignored"
  (list? (member sensor-id sensors-ignored-list)))

(define (sensors-display-usage)
  (display sensors-program-short-name)
  (display " --help --usage --version --verbose --all \\\n")
  (display (make-string (string-length sensors-program-short-name) #\space))
  (display " --sdr-info --flush-cache --list-groups \\\n")
  (display (make-string (string-length sensors-program-short-name) #\space))
  (display " --group=GROUP-NAME --sensors=SENSORS-LIST\n")
  (display "\tDisplays current readings of sensor chip through BMC.\n"))

(define (sensors-display-help)
  (display "IPMI Sensors displays current readings of sensor chip through BMC.\n\n")
  (display "Options:\n") 
  (display "  -u, --usage            Show usage.\n") 
  (display "  -h, --help             Show help.\n") 
  (display "  -V, --version          Show version.\n") 
  (display "  -v, --verbose          Increase verbosity in output.\n") 
  (display "      -vv                Very verbose in output.\n")
  (display "  -i, --sdr-info         Show SDR Information.\n") 
  (display "  -f, --flush-cache      Flush sensor cache.\n") 
  (display "  -l, --list-groups      List sensor groups.\n") 
  (display "  -a, --all              Display all sensors.(override sensors ignore-list)\n")
  (display "  -g GROUP-NAME, --group=GROUP-NAME    Show sensors belongs to given group.\n")
  (display "  -s SENSORS-LIST, --sensors=SENSORS-LIST    Show listed sensors.\n"))

(define (sensors-display-version)
  (display "IPMI Sensors version ")
  (display (fi-version))
  (newline))

(define (sensors-argp-parse args)
  (let* ((sensors-cmd-args '())
	 (optional-args '())
	 (option-spec '((help        (single-char #\h) (value #f))
			(usage       (single-char #\u) (value #f))
			(version     (single-char #\V) (value #f))
			(verbose     (single-char #\v) (value #f))
			(sdr-info    (single-char #\i) (value #f))
			(flush-cache (single-char #\f) (value #f))
			(list-groups (single-char #\l) (value #f))
                        (all         (single-char #\a) (value #f))
			(group       (single-char #\g) (value #t))
			(sensors     (single-char #\s) (value #t))))
	 (options (getopt-long args option-spec)))
    ;; -h, --help option (0)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'help    #f))))
    ;; -u, --usage option (1)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'usage   #f))))
    ;; -V, --version option (2)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'version #f))))
    ;; -v, --verbose option (3)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (let ((vcount 0))
					   (for-each (lambda (arg)
						       (if (equal? (car arg) 'verbose)
							   (set! vcount 
								 (+ vcount 1))))
						     options)
					   vcount))))
    ;; -i, --sdr-info option (4)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'sdr-info    #f))))
    ;; -f, --flush-cache option (5)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'flush-cache #f))))
    ;; -l, --list-groups option (6)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'list-groups #f))))
    ;; -a, --all option (7)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'all         #f))))
    ;; -g, --group option (8)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (option-ref options 'group       #f))))
    ;; -s, --sensors option (9)
    (set! sensors-cmd-args (append sensors-cmd-args 
				   (list (map string->number
					      (string-tokenize
					       (option-ref options 'sensors "")
					       (char-set-complement
						(char-set-adjoin char-set:whitespace 
								 #\,)))))))
    ;; extra options (10)
    (set! optional-args  (option-ref options '() '()))
    (if (not (null? optional-args))
	(set! sensors-cmd-args (append sensors-cmd-args 
				       (list optional-args))))
    sensors-cmd-args))

(define (sensors-get-help-option cmd-args)
  (list-ref cmd-args 0))

(define (sensors-get-usage-option cmd-args)
  (list-ref cmd-args 1))

(define (sensors-get-version-option cmd-args)
  (list-ref cmd-args 2))

(define (sensors-get-verbose-option cmd-args)
  (list-ref cmd-args 3))

(define (sensors-get-sdr-info-option cmd-args)
  (list-ref cmd-args 4))

(define (sensors-get-flush-cache-option cmd-args)
  (list-ref cmd-args 5))

(define (sensors-get-list-group-option cmd-args)
  (list-ref cmd-args 6))

(define (sensors-get-all-option cmd-args)
  (list-ref cmd-args 7))

(define (sensors-get-group-option cmd-args)
  (list-ref cmd-args 8))

(define (sensors-get-sensors-option cmd-args)
  (list-ref cmd-args 9))

(define (sensors-get-extra-option cmd-args)
  (if (= (length cmd-args) 11)
      (list-ref cmd-args 10)
      #f))

(define (sensors-display-alias)
  (if (defined? 'sensors-alias-list)
      (for-each 
       (lambda (alias) 
	 (begin 
	   (display (symbol->string (car alias)))
	   (display " = ")
	   (for-each 
	    (lambda (group-name)
	      (begin 
		(display "\[")
		(display group-name)
		(display "\] ")))
	    (cdr alias)))
	 (newline)) 
       sensors-alias-list)))

(define (sensors-display-group-list)
  (display "System groups:\n")
  (for-each 
   (lambda (group-name)
     (display group-name)
     (newline))
   (fi-sensors-get-group-list))
  (display "\nAliases:\n")
  (sensors-display-alias))

(define (get-hex-string n)
  (let ((str (string-upcase (number->string n 16))))
    (if (= (string-length str) 1)
	(string-append "0" str)
	str)))
