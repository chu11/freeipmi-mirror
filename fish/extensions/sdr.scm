;;; sdr.scm: sensors SDR procedures
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

(use-modules (ice-9 format))

(define (sensors-display-sdr-info sdr-repository-info)
  (let ((sdr-version  (assoc-ref sdr-repository-info "sdr_version"))
	(record-count (assoc-ref sdr-repository-info "record_count"))
	(free-space   (assoc-ref sdr-repository-info "free_space"))
	(recent-addition-timestamp (assoc-ref sdr-repository-info 
					      "recent_addition_timestamp"))
	(recent-erase-timestamp    (assoc-ref sdr-repository-info 
					      "recent_erase_timestamp")))
    (display "SDR Version ") (display sdr-version) (newline)
    (display record-count) (display " records available\n")
    (display free-space) (display " bytes free in SDR\n")
    (display "Most recent record addition on ")
    (display (strftime "%c" (localtime recent-addition-timestamp)))
    (newline)
    (display "Most recent record deletion on ")
    (display (strftime "%c" (localtime recent-erase-timestamp)))
    (newline)))

(define (get-sdr-record-list)
  (letrec ((sdr-record-list '())
	   (next-record-id  0)
	   (sdr-record      '())
	   (sdr-info        (fi-get-sdr-repository-info))
	   (record-count    0)
	   (record-index    0)
	   (get-sdr-record 
	    (lambda (record-id)
	      (if (not (= record-id #xFFFF))
		  (begin 
		    (set! record-count (assoc-ref sdr-info "record_count"))
		    (set! record-index (+ record-index 1))
		    (display 
		     (format #f "Fetching record ~d of ~d (current record ID ~d) ~!\r" 
			     record-index
			     record-count
			     record-id)
		     (current-error-port))
		    (set! sdr-record (fi-get-sdr-record record-id))
		    (if (not (boolean? sdr-record))
			(begin 
			  (set! next-record-id (car sdr-record))
			  (set! sdr-record-list 
				(append sdr-record-list 
					(list (cadr sdr-record))))
			  (get-sdr-record next-record-id))))))))
    (get-sdr-record 0)
    (display "\n" (current-error-port))
    sdr-record-list))

(define (load-sdr-cache-file filename)
  (if (fi-load filename)
      (if (and (defined? 'c-sdr-repository-info) 
	       (defined? 'c-sdr-record-list))
	  (and (list? c-sdr-repository-info)
	       (list? c-sdr-record-list))
	  #f)
      #f))

(define (create-sdr-cache filename)
  (let ((sdr-info (fi-get-sdr-repository-info))
	(record-list (get-sdr-record-list)))
    (catch 'system-error
	   (lambda ()
	     (with-output-to-file filename 
	       (lambda ()
		 (format #t "(define c-sdr-repository-info '~a)~%" sdr-info)
		 (format #t "(define c-sdr-record-list '~a)~%" record-list)))
	     #t)
	   (lambda error-info
	     #f))))

(define (init-sdr-cache)
  (let* ((cache-filename (fi-get-sdr-cache-filename))
	 (sdr-info (fi-get-sdr-repository-info))
	 (sdr-recent-addition-timestamp (assoc-ref sdr-info 
						   "recent_addition_timestamp"))
	 (sdr-recent-erase-timestamp    (assoc-ref sdr-info 
						   "recent_erase_timestamp"))
	 (cache-recent-addition-timestamp #f)
	 (cache-recent-erase-timestamp    #f))
    (if (load-sdr-cache-file cache-filename)
	(begin 
	  (set! cache-recent-addition-timestamp (assoc-ref sdr-info 
							   "recent_addition_timestamp"))
	  (set! cache-recent-erase-timestamp (assoc-ref sdr-info 
							"recent_erase_timestamp"))
	  (if (and (= cache-recent-addition-timestamp sdr-recent-addition-timestamp)
		   (= cache-recent-erase-timestamp sdr-recent-erase-timestamp))
	      #t
	      (if (create-sdr-cache cache-filename)
		  (load-sdr-cache-file cache-filename)
		  #f)))
	(if (create-sdr-cache cache-filename)
	    (load-sdr-cache-file cache-filename)
	    #f))))

(define (sensors-flush-cache)
  (catch 'system-error 
	 (lambda ()
	   (display "flushing cache... ")
	   (delete-file (fi-get-sdr-cache-filename))
	   (display "done\n")
	   #t)
	 (lambda error-info 
	   (display "FAILED\n")
	   #f)))

