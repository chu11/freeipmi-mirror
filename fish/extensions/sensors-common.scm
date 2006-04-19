;;; sensors-common.scm: sensors common procedures
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
  (begin 
    (display "Usage: ipmi-sensors [-ifLv?V] [-D IPMIDRIVER] [-h IPMIHOST]\n")
    (display "                    [-u USERNAME] [-p PASSWORD] [-a AUTHTYPE]\n")
    (display "                    [-l PRIVILEGE-LEVEL] [-g GROUP] [-s SENSORS-LIST]\n")
    (display "                    [--no-probing] [--driver-type=IPMIDRIVER]\n")
    (display "                    [--driver-address=DRIVERADDR]\n")
    (display "                    [--driver-device=DEVICE]\n")
    (display "                    [--packet-retry-timeout=TIMEOUT]\n")
    (display "                    [--packet-retry-max=COUNT] [--hostname=IPMIHOST]\n")
    (display "                    [--username=USERNAME] [--password=PASSWORD]\n")
    (display "                    [--auth-type=AUTHTYPE]\n")
    (display "                    [--username=USERNAME] [--password=PASSWORD]\n")
    (display "                    [--auth-type=AUTHTYPE]\n")
    (display "                    [--priv-level=PRIVILEGE-LEVEL] [--verbose]\n")
    (display "                    [--sdr-info] [--flush-cache] [--list-groups]\n")
    (display "                    [--all] [--group=GROUP] [--sensors=SENSORS-LIST]\n")
    (display "                    [--help] [--usage] [--version]\n")))

(define (sensors-display-help)
  (begin 
    (display "Usage: ipmi-sensors [OPTION...] \n")
    (display "ipmi-sensors displays current readings of sensor chips through BMC.\n")
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
    (display "  -v, --verbose              Increase verbosity in output.  More -v\n")
    (display "                             more verbosity.\n") 
    (display "  -i, --sdr-info             Show SDR Information.\n") 
    (display "  -f, --flush-cache          Flush sensor cache.\n") 
    (display "  -L, --list-groups          List sensor groups.\n") 
    (display "      --all                  Display all sensors (Ignore sensors ignore-list).\n")
    (display "  -g, --group=GROUP          Show sensors belongs to this GROUP.\n")
    (display "  -s, --sensors=SENSORS-LIST Show listed sensors.\n")
    (display "  -?, --help                 Give this help list.\n")
    (display "      --usage                Give a short usage message.\n")
    (display "  -V, --version              Print program version.\n")
    (display "\n")
    (display "Mandatory or optional arguments to long options are also mandatory or optional\n")
    (display "for any corresponding short options.\n")
    (display "\n")
    (display "Report bugs to <freeipmi-devel@gnu.org>.\n")))

(define (sensors-display-version)
  (begin 
    (display (string-append "IPMI Sensors [ipmi-sensors-" (fi-version) "]\n"))
    (display "Copyright (C) 2003-2006 FreeIPMI Core Team\n")
    (display "This program is free software; you may redistribute it under the terms of\n")
    (display "the GNU General Public License.  This program has absolutely no warranty.\n")))

(define (sensors-argp args)
  (catch 'misc-error 
	 (lambda () 
	   (let* ((sensors-cmd-args '())
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
				 (verbose        (single-char #\v)   (value #f))
				 (sdr-info       (single-char #\i)   (value #f))
				 (flush-cache    (single-char #\f)   (value #f))
				 (list-groups    (single-char #\L)   (value #f))
				 (all            (single-char #\376) (value #f))
				 (group          (single-char #\g)   (value #t))
				 (sensors        (single-char #\s)   (value #t))))
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
		  (verbose-wanted (option-ref options 'verbose        #f))
		  (sdr-info       (option-ref options 'sdr-info       #f))
		  (flush-cache    (option-ref options 'flush-cache    #f))
		  (list-groups    (option-ref options 'list-groups    #f))
		  (all-wanted     (option-ref options 'all            #f))
		  (group-name     (option-ref options 'group          #f))
		  (sensors-list   (option-ref options 'sensors        #f))
		  (extra-args     (option-ref options '()             #f)))
	     ;; extra arguments
	     (if (and (not (null? extra-args)) (list? sensors-cmd-args))
		 (begin 
		   (display "Usage: ipmi-sensors [OPTION...] \n"
			    (current-error-port))
		   (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
			    (current-error-port))
		   (set! sensors-exit-status 64)
		   (set! sensors-cmd-args #f)))
	     ;; --no-probing (0)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list no-probing))))
	     ;; --driver-type (1)
	     (if (and (string? driver-type) (list? sensors-cmd-args))
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
		     (display "Usage: ipmi-sensors [OPTION...] \n"
			      (current-error-port))
		     (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
			      (current-error-port))
		     (set! sensor-exit-status 64)
		     (set! sensors-cmd-args #f))))
		 (set! driver-type 0))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list driver-type))))
	     ;; --driver-address (2)
	     (if (and (string? driver-address) (list? sensors-cmd-args))
		 (begin 
		   (set! driver-address (string->number driver-address))
		   (if (boolean? driver-address)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list driver-address))))
	     ;; --driver-device (3)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list driver-device))))
	     ;; --packet-retry-timeout (4)
	     (if (and (string? retry-timeout) (list? sensors-cmd-args))
		 (begin 
		   (set! retry-timeout (string->number retry-timeout))
		   (if (boolean? retry-timeout)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list retry-timeout))))
	     ;; --packet-retry-max (5)
	     (if (and (string? retry-max) (list? sensors-cmd-args))
		 (begin 
		   (set! retry-max (string->number retry-max))
		   (if (boolean? retry-max)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list retry-max))))
	     ;; --host (6)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list host))))
	     ;; --username (7)
	     (if (and (string? username) (list? sensors-cmd-args))
		 (begin 
		   (if (> (string-length username) 16)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list username))))
	     ;; --password (8)
	     (if (and (string? password) (list? sensors-cmd-args))
		 (begin 
		   (if (> (string-length password) 16)
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensor-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list password))))
	     ;; --auth-type (9)
	     (if (and (string? auth-type) (list? sensors-cmd-args))
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
		     (display "Usage: ipmi-sensors [OPTION...] \n"
			      (current-error-port))
		     (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
			      (current-error-port))
		     (set! sensor-exit-status 64)
		     (set! sensors-cmd-args #f))))
		 (set! auth-type 0))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list auth-type))))
	     ;; --priv-level (10)
	     (if (and (string? priv-level) (list? sensors-cmd-args))
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
		     (display "Usage: ipmi-sensors [OPTION...] \n"
			      (current-error-port))
		     (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
			      (current-error-port))
		     (set! sensor-exit-status 64)
		     (set! sensors-cmd-args #f))))
		 (set! priv-level 2))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list priv-level))))
	     ;; --help (11)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list help-wanted))))
	     ;; --usage (12)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list usage-wanted))))
	     ;; --version (13)
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list version-wanted))))
	     ;; -v, --verbose option (14) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! verbose-wanted (let ((vcount 0))
					(for-each (lambda (arg)
						    (if (equal? (car arg) 'verbose)
							(set! vcount 
							      (+ vcount 1))))
						  options)
					vcount)))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list verbose-wanted))))
	     ;; -i, --sdr-info option (15) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list sdr-info))))
	     ;; -f, --flush-cache option (16) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list flush-cache))))
	     ;; -l, --list-groups option (17) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list list-groups))))
	     ;; -a, --all option (18) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list all-wanted))))
	     ;; -g, --group option (19) sensor specific
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list group-name))))
	     ;; -s, --sensors option (20) sensor specific
	     (if (and (string? sensors-list) (list? sensors-cmd-args))
		 (begin 
		   (set! sensors-list (sentence->tokens (string-replace 
							 sensors-list 
							 #\, #\space)))
		   (if (or (list? (member #f (map number? sensors-list)))
			   (null? sensors-list))
		       (begin 
			 (display "Usage: ipmi-sensors [OPTION...] \n"
				  (current-error-port))
			 (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
				  (current-error-port))
			 (set! sensors-exit-status 64)
			 (set! sensors-cmd-args #f)))))
	     (if (list? sensors-cmd-args)
		 (set! sensors-cmd-args (append sensors-cmd-args 
						(list sensors-list))))
	     sensors-cmd-args))
	 (lambda (k args . opts)
	   (display "ipmi-sensors: error: " (current-error-port))
	   (display (cadr opts) (current-error-port))
	   (display "\n" (current-error-port))
	   (display "Usage: ipmi-sensors [OPTION...] \n"
		    (current-error-port))
	   (display "Try `ipmi-sensors --help' or `ipmi-sensors --usage' for more information.\n"
		    (current-error-port))
	   (set! sensors-exit-status 64)
	   #f)))

(define (sensors-get-help-option cmd-args)
  (list-ref cmd-args 11))

(define (sensors-get-usage-option cmd-args)
  (list-ref cmd-args 12))

(define (sensors-get-version-option cmd-args)
  (list-ref cmd-args 13))

(define (sensors-get-verbose-option cmd-args)
  (list-ref cmd-args 14))

(define (sensors-get-sdr-info-option cmd-args)
  (list-ref cmd-args 15))

(define (sensors-get-flush-cache-option cmd-args)
  (list-ref cmd-args 16))

(define (sensors-get-list-group-option cmd-args)
  (list-ref cmd-args 17))

(define (sensors-get-all-option cmd-args)
  (list-ref cmd-args 18))

(define (sensors-get-group-option cmd-args)
  (list-ref cmd-args 19))

(define (sensors-get-sensors-option cmd-args)
  (list-ref cmd-args 20))

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
