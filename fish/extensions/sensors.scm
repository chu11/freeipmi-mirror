;;; sensors.scm: this initialization file is automatically loaded by default
;;; author: Bala. A <bala.a@californiadigital.com>
;;;         M.P.Anand Babu <ab@gnu.org.in>

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
;;; 
;;; /usr/share/fish/extensions/init.scm will be automatically
;;; loaded during the startup. this file inturn loads the standard
;;; fish extensions and also does other external customizations.
;;; To overload this init.scm file, place your own init.scm in ur
;;; ~/.fish/extensions folder.
;; (define sensors-group-alias-list '((cpu . (Processor Board "Slot Connector"))
;; 			   (power . ("Power Supply"))))

(use-modules (ice-9 getopt-long))

;; Global variables
(define sensors-sdr-cache-file (fi-sensors-get-default-cache-filename))
(define sensors-program-short-name "sensors")
(define sensors-exit-status 0)
;(define sensors-cache-found #t)
(define sensors-conf-file (string-append (fi-get-sysconfig-dir)
					 "/fish/sensors-conf.scm"))

;; Load configuration file
(fi-load "sensors-utils.scm")
(fi-load sensors-conf-file)

(define (sensors-display-usage)
  (display "sensors --usage --help --version --verbose --all --sdr-info --flush-cache --list-groups --prof --group=GROUP-NAME --sensors=\"SENSORS-LIST\"\n\tDisplay IPMI Sensors.\n"))

(define (sensors-display-help)
  (begin 
    (display "IPMI Sensors is used to display the current readings of all sensor chips through BMC.\n\n")
    (display "Options:\n") 
    (display "  -u, --usage            Usage message\n") 
    (display "  -h, --help             Show help\n") 
    (display "  -V, --version          Show version\n") 
    (display "  -v, --verbose          Verbose sensor output\n") 
    (display "      -vv                Very verbose sensor output\n")
    (display "  -a, --all              Display all sensors, override ignore list\n")
    (display "  -i, --sdr-info         Show SDR Info\n") 
    (display "  -f, --flush-cache      Flush sensor cache\n") 
    (display "  -l, --list-groups      List sensor groups\n") 
    (display "  -p, --prof             Profile sensor\n") 
    (display "  -g GROUP-NAME, --group=GROUP-NAME             List sensors from group\n")
    (display "  -s \"SENSORS-LIST\", --sensors=\"SENSORS-LIST\"    List given sensors\n")))

(define (sensors-display-version)
  (display (string-append "IPMI Sensors version " 
			  (fi-version)))
  (newline))

(define (sensors-cache-display _sensor-id _all-wanted)
  "display the current cache sensors in default mode"
  (if (or _all-wanted (not (sensors-ignored? _sensor-id)))
      (fi-sensors-cache-display)
      #f))

(define (sensors-cache-verbose-display _sensor-id _all-wanted)
  "display the current cache sensors in verbose mode"
  (if (or _all-wanted (not (sensors-ignored? _sensor-id)))
      (fi-sensors-cache-verbose-display)
      #f))

(define (sensors-cache-very-verbose-display _sensor-id _all-wanted)
  "display the current cache sensors in very verbose mode"
  (if (or (not (sensors-ignored? _sensor-id)))
      (fi-sensors-cache-very-verbose-display)
      #f))

(define (sensors-flush-cache)
  (catch 'system-error 
	 (lambda ()
	   (display "flushing cache... ")
	   (delete-file sensors-sdr-cache-file)
	   (display "done\n")
	   #t)
	 (lambda error-info 
	   (display "FAILED\n")
	   #f)))

(define (sensors-create-cache)
  (catch 'system-error 
	 (lambda ()
	   (system (string-append "mkdir -p " (dirname sensors-sdr-cache-file)))
	   (let 
	       ((fd (open-file sensors-sdr-cache-file "r")))
	     (close fd))
	   #t)
	 (lambda error-info 
	   (let ((tmp-sdr-cache-file (string-append sensors-sdr-cache-file 
						    ".swp")))
	     (begin
	       (display 
		"Creating sensors data repository cache. This may take a while... \n" 
		(current-error-port))
	       (if (access? tmp-sdr-cache-file F_OK)
		   (begin 
		     (display (string-append sensors-program-short-name
					     ": error: cache repo locked. "
					     "One more instance of \"sensors\" creating cache simultaneously! -OR- " 
					     "Lock file [" 
					     tmp-sdr-cache-file 
					     "] is stale\n")
			      (current-error-port))
		     ;(set! sensors-cache-found #f)
		     (set! sensors-exit-status 1)
		     #f)
		   (begin 
		     (if (fi-sensors-cache-create tmp-sdr-cache-file)
			 (begin 
			   (rename-file tmp-sdr-cache-file sensors-sdr-cache-file)
			   #t)
			 (begin 
			   ;(set! sensors-cache-found #f)
			   (set! sensors-exit-status 1)
			   (display "Cache creation FAILED\n" (current-error-port))
			   (catch 'system-error
				  (lambda ()
				    (delete-file tmp-sdr-cache-file))
				  (lambda error-info
				    (let ((errno (system-error-errno error-info)))
				      (display "Cache cleanup error: ")
				      (display (strerror errno))
				      #f)))
			   #f)))))))))

(define (sensors-sdr-info-display)
  (let ((sdr-info (fi-sdr-get-repo-info)))
    (if (not (null? sdr-info))
	(begin 
	  (display "SDR Version ") (display (list-ref sdr-info 0)) (newline)
	  (display (list-ref sdr-info 1)) (display " records available\n")
	  (display (list-ref sdr-info 2)) (display " bytes free in SDR\n")
	  (display "Most recent record addition on ") (display (list-ref sdr-info 3))
	  (display "Most recent record deletion on ") (display (list-ref sdr-info 4))))))

(define (sensors-display-alias)
  (if (defined? 'sensors-group-alias-list)
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
       sensors-group-alias-list)))

(define (sensors-display-group-list)
  (display "System groups:\n")
  (for-each 
   (lambda (group-name)
     (display group-name)
     (newline))
   (fi-sensors-get-group-list))
  (display "\nAliases:\n")
  (sensors-display-alias))

; (define (sensors-expand-alias group)
;   (if (list? (member group (fi-sensors-get-group-list)))
;       (list group)
;       (if (defined? 'sensors-group-alias-list)
; 	  (if (assq-ref sensors-group-alias-list (string->symbol group))
; 	      (assq-ref sensors-group-alias-list (string->symbol group))
; 	      (list))
; 	  (list))))

(define (sensors-same-group? grpname)
  (if (string=? grpname (fi-sensors-cache-get-current-group))
      #t
      (if (list? (assq-ref sensors-group-alias-list (string->symbol grpname)))
	  (if (list? (member (fi-sensors-cache-get-current-group) (assq-ref sensors-group-alias-list (string->symbol grpname))))
	      #t
	      #f)
	  #f)))

(define (sensors-valid-group? grpname)
  (if (list? (member grpname (fi-sensors-get-group-list)))
      #t
      (if (list? (assq-ref sensors-group-alias-list (string->symbol grpname)))
	  #t
	  #f)))

(define (sensors-group-display _group-name _verbose-wanted _verbose-count _prof-wanted _all-wanted)
  (fi-sensors-cache-load sensors-sdr-cache-file)
  (let ((prof-value 0)
	(sensors-displayed #f))
    (begin 
      (for-each 
       (lambda (sensor-id)
	 (fi-sensors-cache-seek sensor-id)
	 (if (sensors-same-group? _group-name)
	     (begin 
	       (if _prof-wanted 
		   (begin 
		     (set! prof-value (fi-kcs-get-poll-count))
		     (cond 
		      ((= _verbose-count 0)
		       (if (sensors-cache-display sensor-id _all-wanted)
			   (begin 
			     (set! sensors-displayed #t)
			     (display "kcs-poll-count: ")
			     (display (- (fi-kcs-get-poll-count) prof-value))
			     (newline) (newline) (force-output))))
		      ((= _verbose-count 1)
		       (if (sensors-cache-verbose-display sensor-id _all-wanted)
			   (begin 
			     (set! sensors-displayed #t)
			     (display "kcs-poll-count: ")
			     (display (- (fi-kcs-get-poll-count) prof-value))
			     (newline) (newline) (newline) (force-output))))
		      (else 
		       (if (sensors-cache-very-verbose-display sensor-id _all-wanted)
			   (begin 
			     (set! sensors-displayed #t)
			     (display "kcs-poll-count: ")
			     (display (- (fi-kcs-get-poll-count) prof-value))
			     (newline) (newline) (newline) (force-output))))))
		   (cond 
		    ((= _verbose-count 0)
		     (if (sensors-cache-display sensor-id _all-wanted)
			 (set! sensors-displayed #t)))
		    ((= _verbose-count 1)
		     (if (sensors-cache-verbose-display sensor-id _all-wanted)
			 (begin 
			   (newline) (newline) (force-output)
			   (set! sensors-displayed #t))))
		    (else 
		     (if (sensors-cache-very-verbose-display sensor-id _all-wanted)
			 (begin 
			   (newline) (newline) (force-output)
			   (set! sensors-displayed #t)))))))))
       (range 1 (fi-sensors-cache-get-total-records)))
      (if (not sensors-displayed)
	  (display (string-append sensors-program-short-name 
				  ": error: No sensors found\n")))))
  (fi-sensors-cache-unload))


; (define (sensors-group-display.buggy _group-name _verbose-wanted _verbose-count _prof-wanted)
;   (fi-sensors-cache-load sensors-sdr-cache-file)
;   (let ((prof-value 0)
; 	(sensor-displayed #f)
; 	(group-displayed #f))
;     (begin
;       (for-each 
;        (lambda (sensor-id)
; 	 (if _prof-wanted 
; 	     (set! prof-value (fi-kcs-get-poll-count)))
; 	 (fi-sensors-cache-seek sensor-id)
; 	 (if (list? (member (fi-sensors-cache-get-current-group) 
; 			    (sensors-expand-alias _group-name)))
; 	     (begin 
; 	       (if _verbose-wanted 
; 		   (if (= _verbose-count 1)
; 		       (set! sensor-displayed (sensors-cache-verbose-display sensor-id))
; 		       (set! sensor-displayed 
; 			     (sensors-cache-very-verbose-display sensor-id)))
; 		   (begin 
; 		     (set! sensor-displayed (sensors-cache-display sensor-id))
; 		     (if (= sensors-exit-status 0)
; 			 (set! sensors-exit-status (fi-get-sensors-errno)))))
; 	       (if (and _prof-wanted sensor-displayed)
; 		   (begin (display "kcs-poll-count: ")
; 			  (display (- (fi-kcs-get-poll-count) prof-value))
; 			  (newline)
; 			  (newline)))
; 	       (if (and _verbose-wanted sensor-displayed)
; 		   (newline))
; 	       (force-output)))
; 	 (if (and (not group-displayed) sensor-displayed)
; 	     (set! group-displayed #t)))
;        (range 1 (fi-sensors-cache-get-total-records)))
;       (if (not group-displayed)
; 	  (begin 
; 	    (if (null? (sensors-expand-alias _group-name)) 
; 		(display (string-append 
; 			  sensors-program-short-name 
; 			  ": error: Invalid sensor group [" _group-name "]\n")
; 			 (current-error-port))
; 		(display (string-append 
; 			  sensors-program-short-name 
; 			  ": error: No sensors found\n")
; 			 (current-error-port)))
; 	    (force-output)
; 	    (set! sensors-exit-status 1)))))
;   (fi-sensors-cache-unload))

(define (sensors-display _sensor-list _verbose-wanted _verbose-count _prof-wanted _all-wanted)
  (fi-sensors-cache-load sensors-sdr-cache-file)
  (let ((prof-value 0)
	(sensor-displayed #f)
	(failed-sensor-list '())
	(out-of-range-sensor-list '()))
    (begin 
      (for-each 
       (lambda (sensor-id)
	 (if _prof-wanted 
	     (set! prof-value (fi-kcs-get-poll-count)))
	 (if (or (< sensor-id 1) 
		 (> sensor-id (fi-sensors-cache-get-total-records)))
	     (set! out-of-range-sensor-list 
		   (append out-of-range-sensor-list (list sensor-id)))
	     (begin 
	       (fi-sensors-cache-seek sensor-id)
	       (if _verbose-wanted 
		   (if (= _verbose-count 1)
		       (set! sensor-displayed (sensors-cache-verbose-display sensor-id _all-wanted))
		       (set! sensor-displayed (sensors-cache-very-verbose-display sensor-id _all-wanted)))
		   (begin 
		     (set! sensor-displayed (sensors-cache-display sensor-id _all-wanted))
		     (if (= sensors-exit-status 0)
			 (set! sensors-exit-status (fi-get-sensors-errno)))))
	       (if (and _prof-wanted sensor-displayed)
		   (begin (display "kcs-poll-count: ")
			  (display (- (fi-kcs-get-poll-count) prof-value))
			  (newline)
			  (newline)))
	       (if (and _verbose-wanted sensor-displayed)
		   (newline))
	       (force-output)
	       (if (not sensor-displayed)
		   (set! failed-sensor-list 
			 (append failed-sensor-list 
				 (list (any->symbol sensor-id))))))))
       _sensor-list)
      (if (not (null? failed-sensor-list))
	  (begin 
	    (display (string-append sensors-program-short-name 
				    ": error: Current reading not available for sensors [" 
				    (list->asv (list->strlist failed-sensor-list) " ") 
				    "]. ")
		     (current-error-port))
	    (set! sensors-exit-status 1)
	    (if (<= _verbose-count 1)
		(display "You may try \"-vv\" option for more info"))
	    (newline)))
      (if (not (null? out-of-range-sensor-list))
	  (begin 
	    (display (string-append sensors-program-short-name 
				    ": error: Sensors [" 
				    (list->asv (list->strlist 
						out-of-range-sensor-list) " ") 
				    "] out of range\n")
		     (current-error-port))
	    (set! sensors-exit-status 1)))
      (force-output)))
  (fi-sensors-cache-unload))

(define (sensors-display-all-records _verbose-wanted _verbose-count _prof-wanted _all-wanted)
  (fi-sensors-cache-load sensors-sdr-cache-file)
  (let ((prof-value 0)
	(sensor-displayed #f))
    (begin 
      (for-each (lambda (sensor-id)
		  (if _prof-wanted 
		      (set! prof-value (fi-kcs-get-poll-count)))
		  (if _verbose-wanted 
		      (if (= _verbose-count 1)
			  (set! sensor-displayed (sensors-cache-verbose-display sensor-id _all-wanted))
			  (set! sensor-displayed (sensors-cache-very-verbose-display sensor-id _all-wanted)))
		      (begin 
			(set! sensor-displayed (sensors-cache-display sensor-id _all-wanted))
			(if (= sensors-exit-status 0)
			    (set! sensors-exit-status (fi-get-sensors-errno)))))
		  (if (and _prof-wanted sensor-displayed)
		      (begin (display "kcs-poll-count: ")
			     (display (- (fi-kcs-get-poll-count) prof-value))
			     (newline)
			     (newline)))
		  (if (and _verbose-wanted sensor-displayed)
		      (newline))
		  (force-output)
		  (fi-sensors-cache-next))
		(range 1 (fi-sensors-cache-get-total-records)))))
  (fi-sensors-cache-unload))

(define (sensors-main args)
  (let* ((option-spec '((usage       (single-char #\u) (value #f))
			(help        (single-char #\h) (value #f))
			(version     (single-char #\V) (value #f))
			(verbose     (single-char #\v) (value #f))
                        (all         (single-char #\a) (value #f))
			(sdr-info    (single-char #\i) (value #f))
			(flush-cache (single-char #\f) (value #f))
			(list-groups (single-char #\l) (value #f))
			(prof        (single-char #\p) (value #f))
			(group       (single-char #\g) (value #t))
			(sensors     (single-char #\s) (value #t))))
	 (options (getopt-long args option-spec))
	 (usage-wanted      (option-ref options 'usage #f))
	 (help-wanted       (option-ref options 'help #f))
         (all-wanted        (option-ref options 'all #f))
	 (version-wanted    (option-ref options 'version #f))
	 (verbose-wanted    (option-ref options 'verbose #f))
	 (verbose-count     (if (option-ref options 'verbose #f)
				(let ((vcount 0))
				  (map (lambda (arg)
					 (if (equal? (car arg) 'verbose)
					     (set! vcount (+ vcount 1))))
				       options)
				  vcount)
				0))
	 (sdr-info-wanted    (option-ref options 'sdr-info #f))
	 (flush-cache-wanted (option-ref options 'flush-cache #f))
	 (list-groups-wanted (option-ref options 'list-groups #f))
	 (prof-wanted        (option-ref options 'prof #f))
	 (group-name         (option-ref options 'group ""))
	 ;;(sensor-list        (sentence->tokens (option-ref options 'sensors "")))
	 (sensor-list        (sentence->tokens (string-replace 
						(option-ref options 'sensors "") 
						#\, #\space)))
	 (args               (option-ref options '() '())))
    (cond 
     ;; argument type check
     ((list? (member #f (map number? sensor-list)))
      (begin (display (string-append sensors-program-short-name 
			      ": error: Invalid argument [" 
			      (list->asv (list->strlist sensor-list) " ") 
			      "] to --sensors option\n")
	       (current-error-port))
	     (set! sensors-exit-status 1)))
     (usage-wanted
      (sensors-display-usage))
     (help-wanted
      (sensors-display-help))
     (version-wanted
      (sensors-display-version))
     (sdr-info-wanted 
      (sensors-sdr-info-display))
     (flush-cache-wanted
      (sensors-flush-cache))
     (list-groups-wanted 
      (sensors-display-group-list))
     ((and (not (string-null? group-name)) (not (sensors-valid-group? group-name)))
      (display (string-append sensors-program-short-name 
			      ": error: invalid groupname [" group-name "]\n")))
     (else 
      (if (sensors-create-cache)
      ;(if sensors-cache-found 
	  (begin 
	    (if (not (string-null? group-name))
		(sensors-group-display group-name verbose-wanted 
				       verbose-count prof-wanted all-wanted))
	    (if (not (null? sensor-list))
		(sensors-display sensor-list verbose-wanted verbose-count prof-wanted all-wanted))
	    (if (and (string-null? group-name) (null? sensor-list))
		(sensors-display-all-records verbose-wanted verbose-count prof-wanted all-wanted))))))))


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
