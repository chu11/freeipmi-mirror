;;; bc-common.scm: BMC configurator common procedures
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

(use-modules (srfi srfi-13))

(use-modules (ice-9 getopt-long))

(define bmc-config-exit-status 0)

(define (assoc-vref alist value)
  (if (null? alist)
      #f
      (if (equal? value (cdr (car alist)))
	  (caar alist)
	  (assoc-vref (cdr alist) value))))

(define privilege-limit-values '(("callback"        . 1) 
				 ("user"            . 2) 
				 ("operator"        . 3) 
				 ("administrator"   . 4) 
				 ("oem_proprietary" . 5) 
				 ("no_access"       . #xF)))

(define (get-privilege-limit value-string)
  (assoc-ref privilege-limit-values (string-downcase value-string)))

(define (valid-privilege-limit? str)
  (pair? (assoc (string-downcase str) privilege-limit-values)))

(define (get-privilege-limit-value-string value)
  (string-capitalize (assoc-vref privilege-limit-values value)))

(define channel-access-modes '(("disabled"         . 0)
			       ("pre_boot_only"    . 1)
			       ("always_available" . 2)
			       ("shared"           . 3)))
(define (get-channel-access-mode value-string)
  (assoc-ref channel-access-modes (string-downcase value-string)))

(define (valid-channel-access-mode? str)
  (pair? (assoc (string-downcase str) channel-access-modes)))

(define (get-channel-access-mode-value-string value)
  (string-capitalize (assoc-vref channel-access-modes value)))

(define ip-address-sources '(("unspecified" . 0)
			     ("static"      . 1)
			     ("use_dhcp"    . 2)
			     ("use_bios"    . 3)
			     ("use_others"  . 4)))

(define (get-ip-address-source value-string)
  (assoc-ref ip-address-sources (string-downcase value-string)))

(define (valid-ip-address-source? str)
  (pair? (assoc (string-downcase str) ip-address-sources)))

(define (get-ip-address-source-value-string value)
  (string-capitalize (assoc-vref ip-address-sources value)))

(define connect-modes '(("modem_connect"  . 0)
			("direct_connect" . 1)))

(define (get-connect-mode value-string)
  (assoc-ref connect-modes (string-downcase value-string)))

(define (valid-connect-mode? str)
  (pair? (assoc (string-downcase str) connect-modes)))

(define (get-connect-mode-value-string value)
  (string-capitalize (assoc-vref connect-modes value)))

(define flow-controls '(("no_flow_control" . 0)
			("rts_cts"         . 1)
			("xon_xoff"        . 2)))

(define (get-flow-control value-string)
  (assoc-ref flow-controls (string-downcase value-string)))

(define (valid-flow-control? str)
  (pair? (assoc (string-downcase str) flow-controls)))

(define (get-flow-control-value-string value)
  (string-capitalize (assoc-vref flow-controls value)))

(define bit-rates '(("9600"   . 6)
		    ("19200"  . 7)
		    ("38400"  . 8)
		    ("57600"  . 9)
		    ("115200" . 10)))

(define (get-bit-rate value-string)
  (assoc-ref bit-rates (string-downcase value-string)))

(define (valid-bit-rate? str)
  (pair? (assoc (string-downcase str) bit-rates)))

(define (get-bit-rate-value-string value)
  (string-capitalize (assoc-vref bit-rates value)))

(define power-restore-policies '(("off_state_ac_apply"     . 0)
				 ("restore_state_ac_apply" . 1)
				 ("on_state_ac_apply"      . 2)))

(define (get-power-restore-policy value-string)
  (assoc-ref power-restore-policies (string-downcase value-string)))

(define (valid-power-restore-policy? str)
  (pair? (assoc (string-downcase str) power-restore-policies)))

(define (get-power-restore-policy-value-string value)
  (string-capitalize (assoc-vref power-restore-policies value)))

(define (read-valid-line fd)
  (let ((line (read-line fd)))
    (cond 
     ((eof-object? line)
      line)
     ((blank-line? line)
      (read-valid-line fd))
     ((comment-line? line)
      (read-valid-line fd))
     (else 
      (string-trim-both (car (string-split (car (string-split line #\#)) #\;)))))))

(define (get-value-validator key validator-def)
  (if (null? validator-def)
      #f
      (if (string-ci=? (string-downcase key) (caar validator-def))
	  (primitive-eval (cadr (car validator-def)))
	  (get-value-validator key (cdr validator-def)))))

(define (get-convertor-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (caddr (car key-def)))
	  (get-convertor-proc key (cdr key-def)))))

(define (get-commit-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (cadddr (car key-def)))
	  (get-commit-proc key (cdr key-def)))))

(define (get-checkout-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (car (cddddr (car key-def))))
	  (get-checkout-proc key (cdr key-def)))))

(define (get-value-convertor-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (cadr (cddddr (car key-def))))
	  (get-value-convertor-proc key (cdr key-def)))))

(define (get-diff-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (caddr (cddddr (car key-def))))
	  (get-diff-proc key (cdr key-def)))))

(define (get-doc-string key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (cadddr (cddddr (car key-def)))
	  (get-doc-string key (cdr key-def)))))

(define (get-string str) str)

(define (get-boolean str)
  (string-ci=? str "yes"))

(define (get-integer str)
  (string->number str))

(define (get-boolean-string bool)
  (if bool "Yes" "No"))

(define (valid-username-password? str)
  (if (string? str)
      (<= (string-length str) 16)
      #f))

(define (valid-boolean? str)
  (if (string? str)
      (or (string-ci=? str "yes") (string-ci=? str "no"))
      #f))

(define (valid-integer? str)
  (if (string? str)
      (integer? (string->number str))
      #f))

(define (valid-ip-address? ip-address)
  (catch #t
	 (lambda ()
	   (inet-aton ip-address)
	   #t)
	 (lambda (k args . opts)
	   #f)))

(define (valid-mac-address? mac-address)
  (and (= (length (string-split mac-address #\:)) 6)
       (boolean? (member #f (map (lambda (s) (string->number s 16)) 
				 (string-split mac-address #\:))))))

(define (file-exists? filename)
  (catch 'system-error 
	 (lambda ()
	   (if (eq? (stat:type (stat filename)) 'regular)
	       #t 
	       (begin 
		 (display 
		  (string-append "error: " filename ": is not a regular file\n") 
		  (current-error-port))
		 #f)))
	 (lambda error-info 
	   (let ((errno (system-error-errno error-info)))
	     (display (string-append "error: " filename ": ") (current-error-port))
	     (display (strerror errno) (current-error-port))
	     (display "\n" (current-error-port))
	     #f))))

(define (make-section key-string value)
  (if (= (length (string-split key-string #\:)) 2)
      (let* ((section-name-key-list (string-split key-string #\:))
	     (section-data (list (car section-name-key-list))))
	(append section-data
		(list (string-append (cadr section-name-key-list) " " value))))
      #f))

(define (same-string? section-name string1 string2)
  (string=? string1 string2))

(define (same-string-ci? section-name string1 string2)
  (string-ci=? string1 string2))

(define (bmc-config-display-usage)
  (begin 
    (display "Usage: bmc-config [-dio?V] [-D IPMIDRIVER] [-h IPMIHOST] [-u USERNAME]\n")
    (display "                  [-p PASSWORD] [-a AUTHTYPE] [-l PRIVILEGE-LEVEL]\n")
    (display "                  [-f FILENAME] [-k KEY-PAIR] [--no-probing]\n")
    (display "                  [--driver-type=IPMIDRIVER]\n")
    (display "                  [--driver-address=DRIVERADDR]\n")
    (display "                  [--driver-device=DEVICE] [--hostname=IPMIHOST]\n")
    (display "                  [--username=USERNAME] [--password=PASSWORD]\n")
    (display "                  [--auth-type=AUTHTYPE]\n")
    (display "                  [--priv-level=PRIVILEGE-LEVEL] [--checkout]\n")
    (display "                  [--commit] [--diff] [--filename=FILENAME]\n")
    (display "                  [--key-pair=KEY-PAIR] [--help] [--usage] [--version]\n")))

(define (bmc-config-display-help)
  (begin 
    (display "Usage: bmc-config [OPTION...] \n")
    (display "bmc-config displays information about BMC.\n")
    (display "\n")
    (display "      --no-probing           Do not probe IPMI devices.\n")
    (display "  -D, --driver-type=IPMIDRIVER   Use this IPMIDRIVER instead of auto selection.\n")
    (display "                              Allowed values are KCS, SMIC, SSIF and LAN.\n")
    (display "      --driver-address=DRIVERADDR\n")
    (display "                             Use this DRIVERADDR address instead of probed\n")
    (display "                             one.\n")
    (display "      --driver-device=DEVICE Use this DEVICE for IPMI driver\n")
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
    (display "  -o, --checkout             Get BMC configuration.\n")
    (display "  -i, --commit               Update BMC configuration.\n")
    (display "  -d, --diff                 Show configuration differences with BMC.\n")
    (display "  -f, --filename=FILENAME    Use FILENAME in checkout, commit or diff.\n")
    (display "  -k, --key-pair=KEY-PAIR    Use KEY-PAIR in commit or diff.\n")
    (display "  -?, --help                 Give this help list.\n")
    (display "      --usage                Give a short usage message.\n")
    (display "  -V, --version              Print program version.\n")
    (display "\n")
    (display "Mandatory or optional arguments to long options are also mandatory or optional\n")
    (display "for any corresponding short options.\n")
    (display "\n")
    (display "Report bugs to <freeipmi-devel@gnu.org>.\n")))

(define (bmc-config-display-version)
  (begin 
    (display (string-append "BMC Configurator [bmc-config-" (fi-version) "]\n"))
    (display "Copyright (C) 2003-2005 FreeIPMI Core Team\n")
    (display "This program is free software; you may redistribute it under the terms of\n")
    (display "the GNU General Public License.  This program has absolutely no warranty.\n")))

(define (bmc-config-argp args)
  (catch 'misc-error 
	 (lambda () 
	   (let* ((bmc-config-cmd-args '())
		  (option-spec '((no-probing     (single-char #\202) (value #f))
				 (driver-type    (single-char #\D)   (value #t))
				 (driver-address (single-char #\203) (value #t))
				 (driver-device  (single-char #\204) (value #t))
				 (host           (single-char #\h)   (value #t))
				 (username       (single-char #\u)   (value #t))
				 (password       (single-char #\p)   (value #t))
				 (auth-type      (single-char #\a)   (value #t))
				 (priv-level     (single-char #\l)   (value #t))
				 (help           (single-char #\?)   (value #f))
				 (usage          (single-char #\377) (value #f))
				 (version        (single-char #\V)   (value #f))
				 (checkout       (single-char #\o)   (value #f))
				 (commit         (single-char #\i)   (value #f))
				 (diff           (single-char #\d)   (value #f))
				 (filename       (single-char #\f)   (value #t))
				 (key-pair       (single-char #\k)   (value #t))))
		  (options (getopt-long args option-spec))
		  (no-probing     (option-ref options 'no-probing     #f))
		  (driver-type    (option-ref options 'driver-type    #f))
		  (driver-address (option-ref options 'driver-address #f))
		  (driver-device  (option-ref options 'driver-device  #f))
		  (host           (option-ref options 'host           #f))
		  (username       (option-ref options 'username       #f))
		  (password       (option-ref options 'password       #f))
		  (auth-type      (option-ref options 'auth-type      #f))
		  (priv-level     (option-ref options 'priv-level     #f))
		  (help-wanted    (option-ref options 'help           #f))
		  (usage-wanted   (option-ref options 'usage          #f))
		  (version-wanted (option-ref options 'version        #f))
		  (checkout       (option-ref options 'checkout       #f))
		  (commit-wanted  (option-ref options 'commit         #f))
		  (diff-wanted    (option-ref options 'diff           #f))
		  (filename       (option-ref options 'filename       #f))
		  (key-pairs      (option-ref options 'key-pair       #f))
		  (extra-args     (option-ref options '()             #f)))
	     ;; extra arguments
	     (if (and (not (null? extra-args)) (list? bmc-config-cmd-args))
		 (begin 
		   (display "Usage: bmc-config [OPTION...] \n"
			    (current-error-port))
		   (display "Try `bmc-config --help' or `bmc-config --usage' for more information.\n"
			    (current-error-port))
		   (set! bmc-config-exit-status 64)
		   (set! bmc-config-cmd-args #f)))
	     ;; --no-probing (0)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list no-probing))))
	     ;; --driver-type (1)
	     (if (and (string? driver-type) (list? bmc-info-cmd-args))
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
		     (set! bmc-info-cmd-args #f))))
		 (set! driver-type 0))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list driver-type))))
	     ;; driver-address (2)
	     (if (and (string? driver-address) (list? bmc-info-cmd-args))
		 (begin 
		   (set! driver-address (string->number driver-address))
		   (if (boolean? driver-address)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! bmc-info-cmd-args #f)))))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list driver-address))))
	     ;; --driver-device (3)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list driver-device))))
	     ;; --host (4)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list host))))
	     ;; --username (5)
	     (if (and (string? username) (list? bmc-info-cmd-args))
		 (begin 
		   (if (> (string-length username) 16)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! bmc-info-cmd-args #f)))))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list username))))
	     ;; --password (6)
	     (if (and (string? password) (list? bmc-info-cmd-args))
		 (begin 
		   (if (> (string-length password) 16)
		       (begin 
			 (display "Usage: bmc-info [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-info --help' or `bmc-info --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-info-exit-status 64)
			 (set! bmc-info-cmd-args #f)))))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list password))))
	     ;; --auth-type (7)
	     (if (and (string? auth-type) (list? bmc-info-cmd-args))
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
		     (set! bmc-info-cmd-args #f))))
		 (set! auth-type 0))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list auth-type))))
	     ;; --priv-level (8)
	     (if (and (string? priv-level) (list? bmc-info-cmd-args))
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
		     (set! bmc-info-cmd-args #f))))
		 (set! priv-level 2))
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list priv-level))))
	     ;; --help (9)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list help-wanted))))
	     ;; --usage (10)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list usage-wanted))))
	     ;; --version (11)
	     (if (list? bmc-info-cmd-args)
		 (set! bmc-info-cmd-args (append bmc-info-cmd-args 
						 (list version-wanted))))
	     ;; --checkout (12) bmc-config specific
	     (if (list? bmc-config-cmd-args)
		 (set! bmc-config-cmd-args (append bmc-config-cmd-args 
						   (list checkout))))
	     ;; --commit (13) bmc-config specific
	     (if (list? bmc-config-cmd-args)
		 (begin 
		   (set! bmc-config-cmd-args (append bmc-config-cmd-args 
						     (list commit-wanted)))
		   (if (and commit-wanted checkout)
		       (begin 
			 (display "bmc-config: any one of option checkout, commit or diff is allowed.\n")
			 (display "Usage: bmc-config [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-config --help' or `bmc-config --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-config-exit-status 64)
			 (set! bmc-config-cmd-args #f)))))
	     ;; --diff (14) bmc-config specific
	     (if (list? bmc-config-cmd-args)
		 (begin 
		   (set! bmc-config-cmd-args (append bmc-config-cmd-args 
						     (list diff-wanted)))
		   (if (and diff-wanted (or checkout commit-wanted))
		       (begin 
			 (display "bmc-config: any one of option checkout, commit or diff is allowed.\n")
			 (display "Usage: bmc-config [OPTION...] \n"
				  (current-error-port))
			 (display "Try `bmc-config --help' or `bmc-config --usage' for more information.\n"
				  (current-error-port))
			 (set! bmc-config-exit-status 64)
			 (set! bmc-config-cmd-args #f)))))
	     ;; --filename (15) bmc-config specific
	     (if (list? bmc-config-cmd-args)
		 (set! bmc-config-cmd-args (append bmc-config-cmd-args 
						   (list filename))))
	     ;; --key-pair (16) bmc-config specific
	     (if (and (string? key-pairs) (list? bmc-config-cmd-args))
		 (set! key-pairs 
		       (let ((klist '()))
			 (for-each (lambda (arg)
				     (if (and (equal? (car arg) 'key-pair)
					      (list? bmc-config-cmd-args))
					 (if (string-index (cdr arg) #\=) 
					     (set! klist 
						   (append 
						    klist 
						    (string-separate (cdr arg) #\=)))
					     (begin 
					       (display "Usage: bmc-config [OPTION...] \n"
							(current-error-port))
					       (display "Try `bmc-config --help' or `bmc-config --usage' for more information.\n"
							(current-error-port))
					       (set! bmc-config-exit-status 64)
					       (set! bmc-config-cmd-args #f)))))
				   options)
			 klist)))
	     (if (list? bmc-config-cmd-args)
		 (set! bmc-config-cmd-args (append bmc-config-cmd-args 
						   (list key-pairs))))
	     ;; special check -- bmc-config specific
	     ;; Following option combinations are errors
	     ;; * No checkout, commit or diff
	     ;; * commit or diff without filename or key-pair
	     ;; * filename without checkout, commit or diff
	     ;; * key-pair without commit or diff
	     (if (list? bmc-config-cmd-args)
		 (if (and 
		      (not (or help-wanted usage-wanted version-wanted))
		      (or (not (or checkout commit-wanted diff-wanted))
			  (and (or commit-wanted diff-wanted)
			       (boolean? filename) (boolean? key-pairs))
			  (and (string? filename)
			       (not (or checkout commit-wanted diff-wanted)))
			  (and (list? key-pairs)
			       (not (or commit-wanted diff-wanted)))))
		     (begin 
		       (bmc-config-display-help)
		       (set! bmc-config-exit-status 64)
		       (set! bmc-config-cmd-args #f))))
	     bmc-config-cmd-args))
	 (lambda (k args . opts)
	   (display "bmc-config: error: " (current-error-port))
	   (display (cadr opts) (current-error-port))
	   (display "\n" (current-error-port))
	   (display "Usage: bmc-config [OPTION...] \n"
		    (current-error-port))
	   (display "Try `bmc-config --help' or `bmc-config --usage' for more information.\n"
		    (current-error-port))
	   (set! bmc-config-exit-status 64)
	   #f)))

(define (bmc-config-get-help-option cmd-args)
  (list-ref cmd-args 9))

(define (bmc-config-get-usage-option cmd-args)
  (list-ref cmd-args 10))

(define (bmc-config-get-version-option cmd-args)
  (list-ref cmd-args 11))

(define (bmc-config-get-checkout-option cmd-args)
  (list-ref cmd-args 12))

(define (bmc-config-get-commit-option cmd-args)
  (list-ref cmd-args 13))

(define (bmc-config-get-diff-option cmd-args)
  (list-ref cmd-args 14))

(define (bmc-config-get-filename-option cmd-args)
  (list-ref cmd-args 15))

(define (bmc-config-get-key-pair-option cmd-args)
  (list-ref cmd-args 16))

