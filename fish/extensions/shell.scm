;;; shell.scm: provides basic shell like facility
;;; author: M.P.Anand Babu <ab@gnu.org.in>

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

;;; this shell.scm extension will be automatically loaded thru
;;; init.scm
;;; usage:
;;; >> sh [command [arguments]]
;;; sh without any arguments will throw u into sub shell. press C-d
;;; to get back to fish. otherwise u can run any shell command
;;; using sh facility
;;;
;;; >> date [arguments]
;;; refer to info pages of "date" command for arguments


(fi-register-command! '("sh" "sh [args]\n\t- enter into shell/invoke a shell command"))
(define (sh args)
  "dynamic command interface to sh facility"
  (set! args (list->strlist args))  
  (if (= (length args) 0)
      (begin
	(display "Press \"C-d\" to get back to Fish")
	(newline)
	(system "sh"))
      (system (list->asv args " "))))

(fi-register-command! '("restart" "restart\n\t- restart fish"))
(define (restart args)
  "dynamic command interface to restart facility"
  (execlp "fish"))

(fi-register-command! '("date" "date [args]\n\t- print current date with all date [options]"))
(define (date args)
  "dynamic command interface to date facility"
  (set! args (list->strlist args))
  (if (= (length args) 0)
      (system "date")
      (system (string-append "date " (list->asv args " ")))))
