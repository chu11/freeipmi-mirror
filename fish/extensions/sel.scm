;;; sel.scm: System Event Logger
;;; authors: M.P.Anand Babu <ab@gnu.org.in> 
;;; Balamurugan <bala.a@californiadigital.com>

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
;;; sel.scm should be automatically loaded thru init.scm

(use-modules (ice-9 getopt-long))

(define sel-exit-status 0)

(define (sel-display-usage)
  (begin 
    (display "Usage: ipmi-sel [-ic?V] [-D IPMIDRIVER] [-h IPMIHOST] [-u USERNAME]\n")
    (display "                [-p PASSWORD] [-a AUTHTYPE] [-l PRIVILEGE-LEVEL]\n")
    (display "                [-d REC-LIST] [-x [FILE]] [--no-probing]\n")
    (display "                [--driver-type=IPMIDRIVER]\n")
    (display "                [--driver-address=DRIVERADDR] [--driver-device=DEVICE]\n")
    (display "                [--packet-retry-timeout=TIMEOUT] [--packet-retry-max=COUNT]\n")
    (display "                [--hostname=IPMIHOST] [--username=USERNAME]\n")
    (display "                [--password=PASSWORD] [--auth-type=AUTHTYPE]\n")
    (display "                [--priv-level=PRIVILEGE-LEVEL] [--info]\n")
    (display "                [--delete=REC-LIST] [--delete-all] [--hex-dump=[FILE]]\n")
    (display "                [--help] [--usage] [--version]\n")))

(define (sel-display-help)
  (begin 
    (display "Usage: ipmi-sel [OPTION...] \n")
    (display "IPMI System Event Logger is used to view and delete SEL entries.\n")
    (display "\n")
    (display "      --no-probing           Do not probe IPMI devices.\n")
    (display "  -D, --driver-type=IPMIDRIVER   Use this IPMIDRIVER instead of auto selection.\n")
    (display "                              Allowed values are KCS, SMIC, SSIF and LAN.\n")
    (display "      --driver-address=DRIVERADDR\n")
    (display "                             Use this DRIVERADDR address instead of probed\n")
    (display "                             one.\n")
    (display "      --driver-device=DEVICE Use this DEVICE for IPMI driver\n")
    (display "  --packet-retry-timeout=TIMEOUT\n")
    (display "                         Use TIMEOUT when reading LAN packets in UDM.\n")
    (display "  --packet-retry-max=COUNT   Use COUNT retries when reading LAN packets get\n")
    (display "                         timed out in UDM.\n")
    (display "  -h, --host=IPMIHOST        Connect to IPMIHOST.\n")
    (display "  -u, --username=USERNAME    Use USERNAME instead of NULL.  Maximum USERNAME\n")
    (display "                             length is 16.\n")
    (display "  -p, --password=PASSWORD    Use PASSWORD instead of NULL.  Maximum PASSWORD\n")
    (display "                             length is 16.\n")
    (display "  -a, --auth-type=AUTHTYPE   Use AUTHTYPE instead of NONE.  Allowed values are\n")
    (display "                             NONE, MD2, MD5, PLAIN and OEM.\n")
    (display "  -l, --priv-level=PRIVILEGE-LEVEL\n")
    (display "                             Use this PRIVILEGE-LEVEL instead of USER.  Allowed\n")
    (display "                             values are CALLBACK, USER, OPERATOR, ADMIN and\n")
    (display "                             OEM.\n")
    (display "  -i, --info                 Show general information about SEL.\n")
    (display "  -d, --delete=REC-LIST      Delete given SEL records entry.\n")
    (display "  -c, --delete-all           Delete all SEL entries.\n")
    (display "  -x, --hex-dump[=FILE]      Hex-dump SEL entries optionally to FILE.\n")
    (display "  -?, --help                 Give this help list.\n")
    (display "      --usage                Give a short usage message.\n")
    (display "  -V, --version              Print program version.\n")
    (display "\n")
    (display "Mandatory or optional arguments to long options are also mandatory or optional\n")
    (display "for any corresponding short options.\n")
    (display "\n")
    (display "Report bugs to <freeipmi-devel@gnu.org>.\n")))

(define (sel-display-version)
  (begin 
    (display (string-append "IPMI System Event Logger [ipmi-sel-" (fi-version) "]\n"))
    (display "Copyright (C) 2003-2005 FreeIPMI Core Team\n")
    (display "This program is free software; you may redistribute it under the terms of\n")
    (display "the GNU General Public License.  This program has absolutely no warranty.\n")))

(define (sel-argp args)
  (catch 'misc-error 
	 (lambda () 
	   (let* ((sel-cmd-args '())
		  (option-spec '((no-probing     (single-char #\202) (value #f))
				 (driver-type    (single-char #\D)   (value #t))
				 (driver-address (single-char #\203) (value #t))
				 (driver-device  (single-char #\204) (value #t))
				 (packet-retry-timeout  (single-char #\205) (value #t))
				 (packet-retry-max      (single-char #\206) (value #t))
				 (host           (single-char #\h)   (value #t))
				 (username       (single-char #\u)   (value #t))
				 (password       (single-char #\p)   (value #t))
				 (auth-type      (single-char #\a)   (value #t))
				 (priv-level     (single-char #\l)   (value #t))
				 (help           (single-char #\?)   (value #f))
				 (usage          (single-char #\377) (value #f))
				 (version        (single-char #\V)   (value #f))
				 (info           (single-char #\i)   (value #f))
				 (delete         (single-char #\d)   (value #t))
				 (delete-all     (single-char #\c)   (value #f))
				 (hex-dump       (single-char #\x)   (value optional))
				 (delete-range   (single-char #\r)   (value #t))))
				 ;;(delete-event-id (single-char #\e) (value #t))))
		  (options (getopt-long args option-spec))
		  (no-probing     (option-ref options 'no-probing     #f))
		  (driver-type    (option-ref options 'driver-type    #f))
		  (driver-address (option-ref options 'driver-address #f))
		  (driver-device  (option-ref options 'driver-device  #f))
		  (retry-timeout  (option-ref options 'packet-retry-timeout #f))
		  (retry-max      (option-ref options 'packet-retry-max     #f))
		  (host           (option-ref options 'host           #f))
		  (username       (option-ref options 'username       #f))
		  (password       (option-ref options 'password       #f))
		  (auth-type      (option-ref options 'auth-type      #f))
		  (priv-level     (option-ref options 'priv-level     #f))
		  (help-wanted    (option-ref options 'help           #f))
		  (usage-wanted   (option-ref options 'usage          #f))
		  (version-wanted (option-ref options 'version        #f))
		  (info-wanted    (option-ref options 'info           #f))
		  (delete-list    (option-ref options 'delete         #f))
		  (delete-all     (option-ref options 'delete-all     #f))
		  (hex-dump-name  (option-ref options 'hex-dump       #f))
		  (delete-range   (option-ref options 'delete-range   #f))
		  (delete-event-id (option-ref options 'delete-event-id #f))
		  (extra-args     (option-ref options '()             #f)))
	     ;; extra arguments
	     (if (and (not (null? extra-args)) (list? sel-cmd-args))
		 (begin 
		   (display "Usage: ipmi-sel [OPTION...] \n"
			    (current-error-port))
		   (display "Try `ipmi-sel --help' or `ipmi-sel --usage' for more information.\n"
			    (current-error-port))
		   (set! sel-exit-status 64)
		   (set! sel-cmd-args #f)))
	     ;; --no-probing (0)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list no-probing))))
	     ;; --driver-type (1)
	     (if (and (string? driver-type) (list? sel-cmd-args))
		 (cond 
		  ((string-ci=? driver-type "lan")
		   (set! driver-type 1))
		  ((string-ci=? driver-type "kcs")
		   (set! driver-type 2))
		  ((string-ci=? driver-type "smic")
		   (set! driver-type 3))
		  ((string-ci=? driver-type "bt")
		   (set! driver-type 4))
		  ((string-ci=? driver-type "ssif")
		   (set! driver-type 5))
		  (else 
		   (begin 
		     (display "Usage: bmc-info [OPTION...] \n"
			      (current-error-port))
		     (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
			      (current-error-port))
		     (set! bmc-info-exit-status 64)
		     (set! sel-cmd-args #f))))
		 (set! driver-type 0))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list driver-type))))
	     ;; --driver-address (2)
	     (if (and (string? driver-address) (list? sel-cmd-args))
		 (begin 
		   (set! driver-address (string->number driver-address))
		   (if (boolean? driver-address)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list driver-address))))
	     ;; --driver-device (3)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list driver-device))))
	     ;; --packet-retry-timeout (4)
	     (if (and (string? retry-timeout) (list? sel-cmd-args))
		 (begin 
		   (set! retry-timeout (string->number retry-timeout))
		   (if (boolean? retry-timeout)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list retry-timeout))))
	     ;; --packet-retry-max (5)
	     (if (and (string? retry-max) (list? sel-cmd-args))
		 (begin 
		   (set! retry-max (string->number retry-max))
		   (if (boolean? retry-max)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list retry-max))))
	     ;; --host (6)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list host))))
	     ;; --username (7)
	     (if (and (string? username) (list? sel-cmd-args))
		 (begin 
		   (if (> (string-length username) 16)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list username))))
	     ;; --password (8)
	     (if (and (string? password) (list? sel-cmd-args))
		 (begin 
		   (if (> (string-length password) 16)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list password))))
	     ;; --auth-type (9)
	     (if (and (string? auth-type) (list? sel-cmd-args))
		 (cond 
		  ((string-ci=? auth-type "none")
		   (set! auth-type 0))
		  ((string-ci=? auth-type "md2")
		   (set! auth-type 1))
		  ((string-ci=? auth-type "md5")
		   (set! auth-type 2))
		  ((string-ci=? auth-type "plain")
		   (set! auth-type 4))
		  ((string-ci=? auth-type "oem")
		   (set! auth-type 5))
		  (else 
		   (begin 
		     (display "Usage: bmc-info [OPTION...] \n"
			      (current-error-port))
		     (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
			      (current-error-port))
		     (set! bmc-info-exit-status 64)
		     (set! sel-cmd-args #f))))
		 (set! auth-type 0))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list auth-type))))
	     ;; --priv-level (10)
	     (if (and (string? priv-level) (list? sel-cmd-args))
		 (cond 
		  ((string-ci=? priv-level "callback")
		   (set! priv-level 1))
		  ((string-ci=? priv-level "user")
		   (set! priv-level 2))
		  ((string-ci=? priv-level "operator")
		   (set! priv-level 3))
		  ((string-ci=? priv-level "admin")
		   (set! priv-level 4))
		  ((string-ci=? priv-level "oem")
		   (set! priv-level 5))
		  (else 
		   (begin 
		     (display "Usage: bmc-info [OPTION...] \n"
			      (current-error-port))
		     (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
			      (current-error-port))
		     (set! bmc-info-exit-status 64)
		     (set! sel-cmd-args #f))))
		 (set! priv-level 2))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list priv-level))))
	     ;; --help (11)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list help-wanted))))
	     ;; --usage (12)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list usage-wanted))))
	     ;; --version (13)
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list version-wanted))))
	     ;; --info (14) SEL specific
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list info-wanted))))
	     ;; --delete-list (15) SEL specific
	     (if (and (string? delete-list) (list? sel-cmd-args))
		 (begin 
		   (set! delete-list (sentence->tokens (string-replace 
							delete-list 
							#\, #\space)))
		   (if (or (list? (member #f (map number? delete-list)))
			   (null? delete-list))
		       (begin 
			 (display "Usage: ipmi-sel [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sel --help' or `ipmi-sel --usage' for more information.\n"
				  (current-error-port))
			 (set! sel-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list delete-list))))
	     ;; --delete-all (16) SEL specific
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list delete-all))))
	     ;; --hex-dump-name (17) SEL specific
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list hex-dump-name))))

	     ;; --delete-range (18) SEL specific
	     (if (and (string? delete-range) (list? sel-cmd-args))
		 (begin 
		   (set! delete-range (sentence->tokens (string-replace 
							delete-range 
							#\- #\space)))
		   (if (or (list? (member #f (map number? delete-range)))
			   (null? delete-range)
			   (not (= (length delete-range) 2))
			   (> (car delete-range) (cadr delete-range)))
		       (begin 
			 (display "Usage: ipmi-sel [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sel --help' or `ipmi-sel --usage' for more information.\n"
				  (current-error-port))
			 (set! sel-exit-status 64)
			 (set! sel-cmd-args #f)))))
	     (if (list? sel-cmd-args)
		 (set! sel-cmd-args (append sel-cmd-args 
					    (list delete-range))))

	     ; --delete-event-id (19) SEL specific
	     (if (and (string? delete-event-id)
		      (list? sel-cmd-args)
		      (not (string->number delete-event-id)))
		 (begin
		   (display "Usage: ipmi-sel [OPTION...] \n"
			    (current-error-port))
		   (display "Try `ipmi-sel --help' or `ipmi-sel --usage' for more information.\n"
			    (current-error-port))
		   (set! sel-exit-status 64)
		   (set! sel-cmd-args #f)))
	     (if (list? sel-cmd-args)
		 (begin
		   (and (string? delete-event-id)
			(set! delete-event-id (string->number
					       delete-event-id)))
		   (set! sel-cmd-args (append sel-cmd-args
					    (list delete-event-id)))))

	   sel-cmd-args))
	 (lambda (k args . opts)
	   (display "sel: error: " (current-error-port))
	   (display (cadr opts) (current-error-port))
	   (display "\n" (current-error-port))
	   (display "Usage: ipmi-sel [OPTION...] \n"
		    (current-error-port))
	   (display "Try `ipmi-sel --help' or `ipmi-sel --usage' for more information.\n"
		    (current-error-port))
	   (set! sel-exit-status 64)
	   #f)))

(define (sel-get-help-option cmd-args)
  (list-ref cmd-args 11))

(define (sel-get-usage-option cmd-args)
  (list-ref cmd-args 12))

(define (sel-get-version-option cmd-args)
  (list-ref cmd-args 13))

(define (sel-get-info-option cmd-args)
  (list-ref cmd-args 14))

(define (sel-get-delete-list-option cmd-args)
  (list-ref cmd-args 15))

(define (sel-get-delete-all-option cmd-args)
  (list-ref cmd-args 16))

(define (sel-get-hex-dump-option cmd-args)
  (list-ref cmd-args 17))

(define (sel-get-delete-range-option cmd-args)
  (list-ref cmd-args 18))

(define (sel-get-delete-event-id-option cmd-args)
  (list-ref cmd-args 19))

(define (sel-display-entry sel)
  (display (list-ref sel 0)) (display ":")
  (display (list-ref sel 1)) (display ":")
  (display (list-ref sel 2)) (display ":")
  (display (list-ref sel 3)) (newline)
  (force-output))

(define (sel-display-all-entry)
  (letrec 
      ((sel-display-entry 
	(lambda (sel)
	  (if (not (null? sel))
	      (begin 
		(display (list-ref sel 0))
		(and (string? (list-ref sel 1)) 
		     (begin (display ":") (display (list-ref sel 1))))
		(and (string? (list-ref sel 2)) 
		     (begin (display ":") (display (list-ref sel 2))))
		(and (string? (list-ref sel 3)) 
		     (begin (display ":") (display (list-ref sel 3))))
		(and (string? (list-ref sel 4)) 
		     (begin (display ":") (display (list-ref sel 4))))
		(and (string? (list-ref sel 5)) 
		     (begin (display ":") (display (list-ref sel 5))))
		(newline)
		;(display (strftime "%d-%b-%Y %H:%M:%S" (localtime (list-ref sel 1)))) 
		(force-output)
		(sel-display-entry (fi-sel-get-next-entry)))))))
    (sel-display-entry (fi-sel-get-first-entry))))

(define (sel-get-record-list)
  (letrec
      ((remaining-records (lambda ()
			    (let ((entry (fi-sel-get-next-entry-raw)))
			      (cond ((null? entry) '())
				    (else (append (list entry)
						  (remaining-records))))))))
    (let ((first-entry (fi-sel-get-first-entry-raw)))
      (append (list (if (null? first-entry) '() first-entry))
	  (if (null? first-entry) '() (remaining-records))))))

(define (sel-record-foreach call-me)
  (map call-me (sel-get-record-list)))

(define (sel-delete-record-list delete-list)
  (if (not (null? delete-list))
      (begin 
	(if (not (fi-sel-delete-entry (car delete-list)))
	    (set! sel-exit-status 1))
	(sel-delete-record-list (cdr delete-list)))))

(define (sel-hex-dump)
  (let ((info (fi-sel-get-info-binary)))
    (sel-display-info gmtime info))
  (let loop ((first-entry (fi-sel-get-first-entry-hex)))
    (if (string? first-entry)
        (begin
          (display first-entry)
          (loop (fi-sel-get-next-entry-hex))))))

(define (sel-display-info time-splitter info)
  (if info
      (let ((last-add-time-string
             (strftime "%m/%d/%Y - %H:%M:%S" (time-splitter (list-ref info 3))))
            (last-erase-time-string
             (strftime "%m/%d/%Y - %H:%M:%S" (time-splitter (list-ref info 4)))))
        (simple-format
         #t (string-append
             "Version                     IPMI v~A.~A\n"
             "Number of Entries           ~A\n"
             "Last Add Time               ~A\n"
             "Last Erase Time             ~A\n"
             "Free Space Remaining        ~A\n\n")
         (list-ref info 0) (list-ref info 1)
         (list-ref info 2)
         last-add-time-string
         last-erase-time-string
         (list-ref info 5)))))

(define (sel-display-flags info)
  (if info
      (begin
        (display "Overflow Flag                                     ")
        (display (if (list-ref info 6) "Yes\n" "No\n"))
        (display "Delete SEL Command Supported                      ")
        (display (if (list-ref info 7) "Yes\n" "No\n"))
        (display "Partial Add SEL Enty Command Supported            ")
        (display (if (list-ref info 8) "Yes\n" "No\n"))
        (display "Reserve SEL Command Supported                     ")
        (display (if (list-ref info 9) "Yes\n" "No\n"))
        (display "Get SEL Allocation Information Command Supported  ")
        (display (if (list-ref info 10) "Yes\n" "No\n")))))

(define (range->list tuple_list)
  (cond ((= (car tuple_list) (cadr tuple_list)) (cdr tuple_list))
        (else (append (list (car tuple_list))
                      (range->list (list (+ 1 (car tuple_list))
                                         (cadr tuple_list)))))))

(define (sel-main cmd-args)
  (cond 
   ((sel-get-help-option cmd-args)
    (sel-display-help))
   ((sel-get-usage-option cmd-args)
    (sel-display-usage))
   ((sel-get-version-option cmd-args)
    (sel-display-version))
   (else 
    (and (fi-ipmi-open cmd-args)
	 (begin 
	   (cond 
	    ((sel-get-info-option cmd-args)
	     (let ((info (fi-sel-get-info-binary)))
	       (sel-display-info localtime info)
	       (sel-display-flags info)))
	    ((sel-get-hex-dump-option cmd-args)
	     (sel-hex-dump))
	    ((string? (sel-get-hex-dump-option cmd-args))
	     (with-output-to-file (sel-get-hex-dump-option cmd-args) 
	       sel-hex-dump))
	    ((sel-get-delete-all-option cmd-args)
	     (fi-sel-clear))
	    ((sel-get-delete-list-option cmd-args)
	     (sel-delete-record-list (sel-get-delete-list-option cmd-args)))
            ((sel-get-delete-range-option cmd-args)
             (sel-delete-record-list (range->list 
				      (sel-get-delete-range-option cmd-args))))
	    ((sel-get-delete-event-id-option cmd-args)
	     (sel-record-foreach (lambda (a-sel)
				   (and (= (length a-sel) 16)
					(= (list-ref a-sel 11) 
					   (sel-get-delete-event-id-option 
					    cmd-args))
					(fi-sel-delete-entry 
					 (+ (list-ref a-sel 0)
					    (* 256 (list-ref a-sel 1))))))))
	    (else 
	     (sel-display-all-entry)))
	   (fi-ipmi-close))))))

(define (sel args)
  "fish sel main"
  (let ((cmd-args (sel-argp (append (list "sel") 
				    (list->strlist args)))))
    (if (list? cmd-args)
	(sel-main cmd-args))))

(fi-register-command! 
 (list "sel" 
       (string-append 
	"sel [--no-probing] [--driver-type=IPMIDRIVER]\n"
	"    [--driver-address=DRIVERADDR] [--driver-device=DEVICE]\n"
	"    [--packet-retry-timeout=TIMEOUT] [--packet-retry-max=COUNT]\n"
	"    [--hostname=IPMIHOST] [--username=USERNAME] [--password=PASSWORD]\n"
	"    [--auth-type=AUTHTYPE] [--priv-level=PRIVILEGE-LEVEL] [--info]\n"
	"    [--delete=REC-LIST] [--delete-all] [--hex-dump=[FILE]] [--help]\n"
	"    [--usage] [--version]\n"
	"\n"
	"    Used to view and delete SEL entries.")))

