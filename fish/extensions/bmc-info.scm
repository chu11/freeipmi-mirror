;;; bmc-info.scm: Displays BMC Information
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
;;; bmc-info.scm should be automatically loaded thru init.scm

(use-modules (ice-9 getopt-long))

(define bmc-info-exit-status 0)

(define (bmc-info-display-usage)
  (display "bmc-info --usage --help --version \n\tShows BMC Information.\n"))

(define (bmc-info-display-help)
  (begin 
    (display "bmc-info displays information about BMC.\n\n")
    (display "Options:\n")
    (display "  -u, --usage                Usage message\n") 
    (display "  -h, --help                 Show help\n")
    (display "  -V, --version              Show version\n")))

(define (bmc-info-display-version)
  (display (string-append 
	    "BMC Info version " 
	    (fi-version)))
  (newline))

(define (bmc-info-main args)
  (let* ((option-spec '((usage    (single-char #\u) (value #f))
			(help     (single-char #\h) (value #f))
			(version  (single-char #\V) (value #f))))
	 (options (getopt-long args option-spec))
	 (usage-wanted         (option-ref options 'usage    #f))
	 (help-wanted          (option-ref options 'help     #f))
	 (version-wanted       (option-ref options 'version  #f)))
    (cond 
     (usage-wanted
      (bmc-info-display-usage))
     (help-wanted
      (bmc-info-display-help))
     (version-wanted
      (bmc-info-display-version))
     (else 
      (fi-kcs-get-dev-id-display)))))

(fi-register-command! 
 '("bmc-info" 
   "bmc-info --usage --help --version \n\tShows BMC Information."))

