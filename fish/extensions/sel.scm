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
  (display "sel --usage --help --version --delete-all --info --hex-dump[=FILE] --delete=REC-LIST\n\tSystem Event Logger.\n"))

(define (sel-display-help)
  (begin 
    (display "IPMI System Event Logger is used to view and delete SEL entries\n\n")
    (display "Options:\n")
    (display "  -u, --usage                Usage message\n") 
    (display "  -h, --help                 Show help\n")
    (display "  -V, --version              Show version\n")
    (display "  -c, --delete-all           Delete all SEL entries\n")
    (display "  -i, --info                 Show general information about SEL\n")
    (display "  -x [FILE], --hex-dump[=FILE]   Output SEL hex dump to FILE or stdout\n")
    (display "  -d REC-LIST, --delete=REC-LIST   Delete given records in SEL\n")))

(define (sel-display-version)
  (display (string-append 
	    "IPMI System Event Logger version " 
	    (fi-version)))
  (newline))

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
		(display (list-ref sel 0)) (display ":")
		(display (strftime "%d-%b-%Y %H:%M:%S" (localtime (list-ref sel 1)))) 
		(display ":")
		(display (list-ref sel 2)) (display ":")
		(display (list-ref sel 3)) (display ":")
		(display (list-ref sel 4)) (newline)
		(force-output)
		(sel-display-entry (fi-sel-get-next-entry)))))))
    (sel-display-entry (fi-sel-get-first-entry))))


(define (sel-delete-record-list delete-list)
  (if (not (null? delete-list))
      (begin 
	(if (not (fi-sel-delete-entry (car delete-list)))
	    (set! sel-exit-status 1))
	(sel-delete-record-list (cdr delete-list)))))

(define (sel-hex-dump)
  (let ((info (fi-sel-get-info)))
    (if (string? info) (display info)))
  (let loop ((first-entry (fi-sel-get-first-entry-hex)))
    (if (string? first-entry)
        (begin
          (display first-entry)
          (loop (fi-sel-get-next-entry-hex))))))

(define (sel-main args)
  (let* ((option-spec '((usage    (single-char #\u) (value #f))
			(help     (single-char #\h) (value #f))
			(version  (single-char #\V) (value #f))
                        (info     (single-char #\i) (value #f))
                        (hex-dump (single-char #\x) (value 'optional))
			(delete-all    (single-char #\c) (value #f))
			(delete   (single-char #\d) (value #t))))
	 (options (getopt-long args option-spec))
	 (usage-wanted         (option-ref options 'usage    #f))
	 (help-wanted          (option-ref options 'help     #f))
	 (version-wanted       (option-ref options 'version  #f))
         (info-wanted          (option-ref options 'info     #f))
         (hex-dump-name        (option-ref options 'hex-dump #f))
	 (delete-all-wanted    (option-ref options 'delete-all    #f))
	 (delete-list          (sentence->tokens (string-replace 
						  (option-ref options 'delete "") 
						  #\, #\space))))
    (cond 
     ;; argument type check
     ((list? (member #f (map number? delete-list)))
      (begin (display (string-append sensors-program-short-name 
			      ": error: Invalid argument [" 
			      (list->asv (list->strlist delete-list) " ") 
			      "] to --delete option\n")
	       (current-error-port))
	     (set! sel-exit-status 1)))
     (usage-wanted
      (sel-display-usage))
     (help-wanted
      (sel-display-help))
     (version-wanted
      (sel-display-version))
     (info-wanted
      (let ((info (fi-sel-get-info)))
        (if (string? info) (display info))))
     ((string? hex-dump-name)
      (with-output-to-file hex-dump-name
        sel-hex-dump))
     (hex-dump-name
      (sel-hex-dump))
     (delete-all-wanted
      (fi-sel-clear))
     ((not (null? delete-list))
      (sel-delete-record-list delete-list))
     (else 
      (sel-display-all-entry)))))

(define (sel args)
  "fish sel main"
  (set! args (list->strlist args))
  (catch 'misc-error
	 (lambda ()
	   (sel-main (append '("sel") args)))
	 (lambda (k args . opts)
	   (display "sel: error: ")
	   (display (cadr opts))
	   (newline))))

(fi-register-command! 
 '("sel" 
   "sel --usage --help --version --info --hex-dump[=FILE] --delete-all --delete=REC-LIST\n\tSystem Event Logger."))

