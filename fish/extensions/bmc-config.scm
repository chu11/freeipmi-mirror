;;; bmc-config.scm: BMC configuration system
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
;;; bmc-config.scm should be automatically loaded thru init.scm

(use-modules (ice-9 getopt-long))

(define bmc-config-exit-status 0)

(define (bmc-config-display-usage)
  (display "bmc-config --usage --help --version --checkout --commit --diff --filename=FILENAME --key-pair=\"KEY=VALUE\" ...\n\tGet and set BMC configurations.\n"))

(define (bmc-config-display-help)
  (begin 
    (display "IPMI BMC Configurator is used to get and set BMC configurations\n\n")
    (display "Options:\n")
    (display "  -u, --usage                Usage message\n") 
    (display "  -h, --help                 Show help\n")
    (display "  -V, --version              Show version\n")
    (display "  -o, --checkout             Fetch configuration information from BMC.\n")
    (display "  -i, --commit               Update configuration information to BMC\n")
    (display "  -d, --diff                 Show differences between BMC and config file or key pairs.\n")
    (display "  -f FILENAME, --filename=FILENAME    Use this file for BMC get/set.\n")
    (display "  -k \"KEY=VALUE\", --key-pair=\"KEY=VALUE\"    Update configuration information to BMC.  This option can be used multiple times.\n")))

(define (bmc-config-display-version)
  (display (string-append 
	    "IPMI BMC Configurator version " 
	    (fi-version)))
  (newline))

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

(define (bmc-config-check-key-pairs_old key-list)
  (fi-bmc-config-checkout "/tmp/.bmc-config.swap")
  (letrec ((key-check-flag #t)
	   (bmc-config-check-key 
	    (lambda (key key-list)
	      (if (not (fi-bmc-config-check-key "/tmp/.bmc-config.swap" key))
		  (begin 
		    (display (string-append "error: '" key "' key not found\n"))
		    (set! key-check-flag #f)
		    (set! bmc-config-exit-status 1)))
	      (if (not (null? key-list))
		  (bmc-config-check-key (car key-list) (cddr key-list))))))
    (bmc-config-check-key (car key-list) (cddr key-list))
    key-check-flag))

(define (bmc-config-check-key-pairs key-list)
  (letrec ((key-check-flag #t)
	   (bmc-config-check-key 
	    (lambda (key key-list)
	      (if (not (fi-bmc-config-check-key key))
		  (begin 
		    (display (string-append "error: '" key "' key not found\n"))
		    (set! key-check-flag #f)
		    (set! bmc-config-exit-status 1)))
	      (if (not (null? key-list))
		  (bmc-config-check-key (car key-list) (cddr key-list))))))
    (bmc-config-check-key (car key-list) (cddr key-list))
    key-check-flag))
		    

(define (bmc-config-checkout filename)
  (fi-bmc-config-checkout filename))

(define (bmc-config-commit-file filename)
  (fi-bmc-config-commit filename))

(define (bmc-config-commit-key-pairs key-list)
  (fi-bmc-config-checkout "/tmp/.bmc-config.swap")
  (letrec ((bmc-config-edit-key-pair 
	    (lambda (key value key-list)
	      (fi-bmc-config-edit-key-pair "/tmp/.bmc-config.swap" key value)
	      (if (not (null? key-list))
		  (bmc-config-edit-key-pair (car key-list) (cadr key-list) (cddr key-list))))))
    (bmc-config-edit-key-pair (car key-list) (cadr key-list) (cddr key-list)))
  (fi-bmc-config-commit "/tmp/.bmc-config.swap"))

(define (bmc-config-commit-file-key-pairs filename key-list)
  (system (string-append "cp -af " filename " /tmp/.bmc-config.swap"))
  (letrec ((bmc-config-edit-key-pair 
	    (lambda (key value key-list)
	      (fi-bmc-config-edit-key-pair "/tmp/.bmc-config.swap" key value)
	      (if (not (null? key-list))
		  (bmc-config-edit-key-pair (car key-list) (cadr key-list) (cddr key-list))))))
    (bmc-config-edit-key-pair (car key-list) (cadr key-list) (cddr key-list)))
  (fi-bmc-config-commit "/tmp/.bmc-config.swap"))


(define (bmc-config-commit filename key-list)
  (if (not (string-null? filename))
      (if (file-exists? filename)
	  (if (not (null? key-list))
	      (if (bmc-config-check-key-pairs key-list)
		  (bmc-config-commit-file-key-pairs filename key-list))
	      (bmc-config-commit-file filename))
	  (set! bmc-config-exit-status 1))
      (if (not (null? key-list))
	  (if (bmc-config-check-key-pairs key-list)
	      (bmc-config-commit-key-pairs key-list)))))

(define (bmc-config-diff-key-pairs key-list)
  (fi-bmc-config-checkout "/tmp/.bmc-config.swap")
  (letrec ((bmc-config-diff-key-pair
	    (lambda (key value key-list)
	      (if (= (fi-bmc-config-diff-key-pair "/tmp/.bmc-config.swap" key value) 1)
		  (set! bmc-config-exit-status 1))
	      (if (not (null? key-list))
		  (bmc-config-diff-key-pair (car key-list) (cadr key-list) (cddr key-list))))))
    (bmc-config-diff-key-pair (car key-list) (cadr key-list) (cddr key-list))))

(define (bmc-config-diff-file filename)
  (fi-bmc-config-checkout "/tmp/.bmc-config.swap")
  (if (= (fi-bmc-config-diff-file "/tmp/.bmc-config.swap" filename) 1)
      (set! bmc-config-exit-status 1)))

(define (bmc-config-diff filename key-list)
  (if (not (string-null? filename))
      (if (file-exists? filename)
	  (bmc-config-diff-file filename)
	  (set! bmc-config-exit-status 1)))
  (if (not (null? key-list))
      (if (bmc-config-check-key-pairs key-list)
	  (bmc-config-diff-key-pairs key-list))))

(define (bmc-config-main args)
  (let* ((option-spec '((usage    (single-char #\u) (value #f))
			(help     (single-char #\h) (value #f))
			(version  (single-char #\V) (value #f))
			(checkout (single-char #\o) (value #f))
			(commit   (single-char #\i) (value #f))
			(diff     (single-char #\d) (value #f))
			(filename (single-char #\f) (value #t))
			(key-pair (single-char #\k) (value #t))))
	 (options (getopt-long args option-spec))
	 (usage-wanted    (option-ref options 'usage    #f))
	 (help-wanted     (option-ref options 'help     #f))
	 (version-wanted  (option-ref options 'version  #f))
	 (checkout-wanted (option-ref options 'checkout #f))
	 (commit-wanted   (option-ref options 'commit   #f))
	 (diff-wanted     (option-ref options 'diff     #f))
	 (filename        (option-ref options 'filename ""))
	 (args            (option-ref options '()       '()))
	 (key-pair-list   (if (option-ref options 'key-pair #f)
			      (let ((klist '()))
				(map (lambda (arg)
				       (if (equal? (car arg) 'key-pair)
					   (if (string-index (cdr arg) #\=) 
					       (set! klist 
						     (append 
						      klist 
						      (string-separate (cdr arg) #\=)))
					       (set! klist 
						     (append klist (list (cdr arg) ""))))))
				     options)
				klist)
			      '())))
    ;;(begin (display key-pair-list) (newline))
    (cond 
     (usage-wanted
      (bmc-config-display-usage))
     (help-wanted
      (bmc-config-display-help))
     (version-wanted
      (bmc-config-display-version))
     (checkout-wanted 
      (bmc-config-checkout filename))
     (commit-wanted 
      (bmc-config-commit filename key-pair-list))
     (diff-wanted 
      (bmc-config-diff filename key-pair-list))
     (else 
      (bmc-config-display-usage)))))

(define (bmc-config args)
  "fish bmc-config main"
  (set! args (list->strlist args))
  (catch 'misc-error
	 (lambda ()
	   (bmc-config-main (append '("bmc-config") args)))
	 (lambda (k args . opts)
	   (display "bmc-config: error: ")
	   (display (cadr opts))
	   (newline))))

(fi-register-command! 
 '("bmc-config" 
   "bmc-config --usage --help --version --checkout --commit --diff --filename FILENAME --key-pair=\"KEY=VALUE\" ...\n\tGet and set BMC configurations."))

