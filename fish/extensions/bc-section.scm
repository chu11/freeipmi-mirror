(define section-name-list '("user1" 
			    "user2" 
			    "user3" 
			    "user4" 
			    "lan_channel" 
			    "lan_conf" 
			    "lan_conf_auth" 
			    "lan_conf_misc" 
			    "serial_channel" 
			    "serial_conf"
			    "misc"))

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

(define (validate-section-keys section-data section-keys)
  (let ((line "")
	(key "")
	(value "")
	(value-validator ""))
    (if (null? section-data)
	#t
	(begin 
	  (set! line (car section-data))
	  (set! key (car (string-tokenize line)))
	  (set! value (cadr (string-tokenize line)))
	  (set! value-validator (get-value-validator key section-keys)) 
	  (if (boolean? value-validator)
	      (begin 
		(display "ERROR: invalid key <") (display key) (display ">\n")
		#f)
	      (if (eval (list value-validator value) (interaction-environment))
		  (validate-section-keys (cdr section-data) section-keys)
		  (begin 
		    (display "ERROR: invalid value <") (display value) 
		    (display "> of key <") (display key) (display ">\n")
		    #f)))))))

(define (commit-section-values section-name section-data section-keys)
  (let ((line "")
	(key "")
	(value "")
	(convertor-proc "")
	(commit-proc ""))
    (if (null? section-data)
	#t
	(begin 
	  (set! line (car section-data))
	  (set! key (car (string-tokenize line)))
	  (set! value (cadr (string-tokenize line)))
	  (set! convertor-proc (get-convertor-proc key section-keys)) 
	  (set! commit-proc (get-commit-proc key section-keys))
	  (set! value (eval (list convertor-proc value) (interaction-environment)))
; 	  (display key) (newline)
; 	  (display value) (newline)
; 	  (display convertor-proc) (newline)
; 	  (display commit-proc) (newline)
	  (if (eval (list commit-proc section-name value) (interaction-environment))
	      (begin 
		(display (string-append "Function <" (simple->string commit-proc)
					"> Key <" key 
					"> Value <" (simple->string value) 
					"> DONE\n"))
		(commit-section-values section-name (cdr section-data) section-keys))
	      (begin 
		(display (string-append "Function <" (simple->string commit-proc) 
					"> Key <" key 
					"> Value <" (simple->string value) 
					"> FAILED\n"))
		#f))))))

(define (validate-section section-data)
  (let ((section-name ""))
    (if (not (list? section-data))
	(begin 
	  (display "ERROR: invalid section\n")
	  #f)
	(begin 
	  (set! section-name (car section-data))
	  (if (boolean? (member (string-downcase section-name) section-name-list))
	      (begin 
		(display (string-append "ERROR: invalid section name <" 
					section-name ">\n"))
		#f)
	      (cond 
	       ((or (string-ci=? section-name "user1") 
		    (string-ci=? section-name "user2") 
		    (string-ci=? section-name "user3") 
		    (string-ci=? section-name "user4"))
		(validate-section-keys (cdr section-data) user-keys-validator))
	       ((string-ci=? section-name "lan_channel")
		(validate-section-keys (cdr section-data) lan-serial-channel-keys-validator))
	       ((string-ci=? section-name "lan_conf")
		(validate-section-keys (cdr section-data) lan-conf-keys-validator))
	       ((string-ci=? section-name "lan_conf_auth")
		(validate-section-keys (cdr section-data) lan-conf-auth-keys-validator))
	       ((string-ci=? section-name "lan_conf_misc")
		(validate-section-keys (cdr section-data) lan-conf-misc-keys-validator))
	       ((string-ci=? section-name "serial_channel")
		(validate-section-keys (cdr section-data) lan-serial-channel-keys-validator))
	       ((string-ci=? section-name "serial_conf")
		(validate-section-keys (cdr section-data) serial-conf-keys-validator))
	       ((string-ci=? section-name "misc")
		(validate-section-keys (cdr section-data) misc-keys-validator))
	       (else 
		(display "currently not supported\n"))))))))

(define (commit-section section-data)
  (let ((section-name (car section-data)))
    (cond 
     ((or (string-ci=? section-name "user1") 
	  (string-ci=? section-name "user2") 
	  (string-ci=? section-name "user3") 
	  (string-ci=? section-name "user4"))
      (commit-section-values section-name (cdr section-data) user-keys-validator))
     ((string-ci=? section-name "lan_channel")
      (commit-section-values section-name (cdr section-data) lan-serial-channel-keys-validator))
     ((string-ci=? section-name "lan_conf")
      (commit-section-values section-name (cdr section-data) lan-conf-keys-validator))
     ((string-ci=? section-name "lan_conf_auth")
      (commit-section-values section-name (cdr section-data) lan-conf-auth-keys-validator))
     ((string-ci=? section-name "lan_conf_misc")
      (commit-section-values section-name (cdr section-data) lan-conf-misc-keys-validator))
     ((string-ci=? section-name "serial_channel")
      (commit-section-values section-name (cdr section-data) lan-serial-channel-keys-validator))
     ((string-ci=? section-name "serial_conf")
      (commit-section-values section-name (cdr section-data) serial-conf-keys-validator))
     ((string-ci=? section-name "misc")
      (commit-section-values section-name (cdr section-data) misc-keys-validator))
     (else 
      (display "currently not supported\n")))))
