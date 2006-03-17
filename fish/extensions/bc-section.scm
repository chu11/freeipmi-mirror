;;; bc-section.scm: BMC configurator section procedures
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

(define section-name-desc '(("user1"          . user-keys-validator)
			    ("user2"          . user-keys-validator)
			    ("user3"          . user-keys-validator)
			    ("user4"          . user-keys-validator)
			    ("user5"          . user-keys-validator)
			    ("lan_channel"    . lan-serial-channel-keys-validator)
			    ("lan_conf"       . lan-conf-keys-validator)
			    ("lan_conf_auth"  . lan-conf-auth-keys-validator)
                            ("lan_conf_security_keys" . lan-conf-security-keys-validator)
			    ("lan_conf_misc"  . lan-conf-misc-keys-validator)
			    ("rmcpplus_conf_privilege" . rmcpplus-conf-privilege-keys-validator)
			    ("serial_channel" . lan-serial-channel-keys-validator)
			    ("serial_conf"    . serial-conf-keys-validator)
			    ("pef_conf"       . pef-conf-keys-validator)
			    ("sol_conf"       . sol-conf-keys-validator)
			    ("misc"           . misc-keys-validator)))

(define (read-section-data fd)
  (letrec ((section-data '())
	   (line "")
	   (append-section-data 
	    (lambda ()
	      (set! line (read-valid-line fd))
	      (if (eof-object? line)
		  #f
		  (cond 
		   ((string-ci=? (car (string-tokenize line)) "section")
		    #f)
		   ((string-ci=? line "endsection")
		    section-data)
		   (else 
		    (set! section-data (append section-data 
					       (list (list->sentence 
						      (string-tokenize line)))))
		    (append-section-data)))))))
    (append-section-data)))

(define (read-section fd)
  (letrec ((section-name "")
	   (section-data "")
	   (line "")
	   (read-data 
	    (lambda ()
	      (set! line (read-valid-line fd))
	      (if (eof-object? line)
		  '()
		  (if (string-ci=? (car (string-tokenize line)) "section")
		      (begin 
			(set! section-name (cadr (string-tokenize line)))
			(set! section-data (read-section-data fd))
			(if (list? section-data)
			    (cons section-name section-data)
			    #f))
		      #f)))))
    (read-data)))

(define (valid-key? key key-desc-list)
  (not (boolean? (get-value-validator key key-desc-list))))

(define (valid-value? key value key-desc-list)
  (let ((value-validator (get-value-validator key key-desc-list)))
    (if (boolean? value-validator)
	#f
	((primitive-eval value-validator) value))))

(define (valid-section-name? section-name)
  (not (boolean? (assoc (string-downcase section-name) section-name-desc))))

(define (get-section-key-desc section-name)
  (primitive-eval (assoc-ref section-name-desc 
			     (string-downcase section-name))))

;;; commit functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (validate-commit-section-data section-data key-desc-list) 
  (if (null? section-data) 
      #t 
      (let* ((line  (car section-data)) 
	     (key   (car (string-tokenize line))) 
	     (value (if (= (length (string-tokenize line)) 1)
			#f 
			(cadr (string-tokenize line))))) 
	(if (valid-key? key key-desc-list) 
	    (if (or (eq? value #f) (valid-value? key value key-desc-list)) 
		(validate-commit-section-data (cdr section-data) key-desc-list) 
		(begin 
		  (display (string-append "ERROR: invalid value <" 
					  value 
					  "> of key <" 
					  key 
					  ">\n") (current-error-port))
		  #f))
	    (begin 
	      (display (string-append "ERROR: invalid key <" 
				      key 
				      ">\n") (current-error-port))
	      #f)))))

(define (validate-commit-section section-data)
  (if (null? section-data)
      (begin 
	(display "ERROR: invalid section data\n" (current-error-port))
	#f)
      (let ((section-name (car section-data)))
	(if (valid-section-name? section-name)
	    (validate-commit-section-data (cdr section-data)
					  (get-section-key-desc section-name))
	    (begin 
	      (display (string-append "ERROR: invalid section name <"
				    section-name 
				    ">\n") (current-error-port))
	      #f)))))

(define (commit-section-values section-name section-data section-keys)
  (let ((status #t))
    (if (null? section-data)
	status 
	(let* ((line           (car section-data))
	       (key            (car (string-tokenize line)))
	       (value-string   (if (= (length (string-tokenize line)) 1)
				   "" 
				   (cadr (string-tokenize line))))
	       (convertor-proc (get-convertor-proc key section-keys)) 
	       (commit-proc    (get-commit-proc key section-keys))
	       (value          (if (string-null? value-string) 
				   '() 
				   (convertor-proc value-string))))
; 	  (set! value (eval (list convertor-proc value) (interaction-environment)))
; 	  (display key) (newline)
; 	  (display value) (newline)
; 	  (display convertor-proc) (newline)
; 	  (display commit-proc) (newline)
; 	  (if (eval (list commit-proc section-name value) (interaction-environment))
	  (if (not (commit-proc section-name value))
	      (begin 
		(set! status #f)
		(display (string-append "Commit Section <" section-name 
					"> Key <" key 
					"> Value <" value-string 
					"> FAILED\n") (current-error-port))))
	  (commit-section-values section-name (cdr section-data) section-keys)))))

(define (commit-section section-data)
  (let ((section-name (car section-data)))
    (commit-section-values section-name 
			   (cdr section-data)
			   (get-section-key-desc section-name))))

;;; checkout functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (validate-checkout-section-keys key-list key-desc-list) 
  (if (null? key-list) 
      #t 
      (let ((key (car key-list)))
	(if (valid-key? key key-desc-list) 
	    (validate-checkout-section-data (cdr key-list) key-desc-list) 
	    (begin 
	      (display (string-append "ERROR: invalid key <" 
				      key 
				      ">\n") (current-error-port))
	      #f)))))

(define (validate-checkout-section section-data)
  (if (null? section-data)
      (begin 
	(display "ERROR: invalid section data\n" (current-error-port))
	#f)
      (let ((section-name (car section-data)))
	(if (valid-section-name? section-name)
	    (validate-checkout-section-data (cdr section-data)
					    (get-section-key-desc section-name))
	    (begin 
	      (display (string-append "ERROR: invalid section name <"
				    section-name 
				    ">\n") (current-error-port))
	      #f)))))

(define (checkout-section-value section-name key key-desc-list) 
  (let* ((checkout-proc (get-checkout-proc key key-desc-list))
	 (convertor-proc (get-value-convertor-proc key key-desc-list))
	 (value (checkout-proc section-name)))
    (if (list? value)
	((primitive-eval convertor-proc) (car value))
	#f)))

(define (checkout-section-values section-name key-list key-desc-list fp)
  (if (null? key-list) 
      #t 
      (let* ((key (car key-list))
	     (value (checkout-section-value section-name 
					    key 
					    key-desc-list)))
	(if (boolean? value)
	    (display (string-append "This BMC does not support option [" 
				    key 
				    "].\n") (current-error-port))
	    (display (string-append "\t## " 
				    (get-doc-string key key-desc-list)
				    "\n"
				    "\t" 
				    (format #f "~45a" 
					    (string-capitalize key)) 
				    value 
				    "\n") fp))
	(checkout-section-values section-name (cdr key-list) key-desc-list fp))))

(define (checkout-section section-data fp)
  (display (string-append "Section " (car section-data) "\n") fp)
  (checkout-section-values (car section-data)
			   (cdr section-data)
			   (get-section-key-desc (car section-data))
			   fp)
  (display "EndSection\n" fp))


;;; diff functions ;;;
;;;;;;;;;;;;;;;;;;;;;;
(define (diff-section-values section-name section-data key-desc-list)
  (if (null? section-data)
      #t
      (let* ((line           (car section-data))
	     (key            (car (string-tokenize line)))
	     (value          (if (= (length (string-tokenize line)) 1)
				 "" 
				 (cadr (string-tokenize line))))
	     (bmc-value      (checkout-section-value section-name 
						     key 
						     key-desc-list))
	     (diff-proc      (get-diff-proc key key-desc-list)))
	(if (boolean? bmc-value)
	    (display (string-append "This BMC does not support option [" 
				    key 
				    "].\n") (current-error-port))
	    (if (not (diff-proc section-name value bmc-value))
		(begin 
		  (display (string-append "USER:" key "=" value "\n"))
		  (display (string-append "BMC :" key "=" bmc-value " differs\n")))))
	(diff-section-values section-name (cdr section-data) key-desc-list))))

(define (diff-section section-data)
  (let ((section-name (car section-data)))
    (diff-section-values section-name 
			 (cdr section-data)
			 (get-section-key-desc section-name))))
