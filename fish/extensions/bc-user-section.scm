;;; bc-user-section.scm: BMC configurator User
;;;                      section procedures
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

(define (commit-username section-name username)
  (if (list? username)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-username 1 username))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-username 2 username))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-username 3 username))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-username 4 username)))))

(define (checkout-username section-name)
  (cond 
   ((string-ci=? section-name "user1")
    (fi-get-bmc-username 1))
   ((string-ci=? section-name "user2")
    (fi-get-bmc-username 2))
   ((string-ci=? section-name "user3")
    (fi-get-bmc-username 3))
   ((string-ci=? section-name "user4")
    (fi-get-bmc-username 4))))

(define (commit-enable-user section-name user-status)
  (if (list? user-status)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-enable-user 1 user-status))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-enable-user 2 user-status))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-enable-user 3 user-status))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-enable-user 4 user-status)))))

; achu: Can't checkout
(define (checkout-enable-user section-name) (list ""))

; achu: Can't check, just assume its ok??
(define (diff-enable-user section-name user-enable-user bmc-enable-user) #t)

(define (commit-clear-password section-name password-status)
  (if (list? password-status)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(if password-status (fi-set-bmc-user-password 1 "") #t))
       ((string-ci=? section-name "user2")
	(if password-status (fi-set-bmc-user-password 2 "") #t))
       ((string-ci=? section-name "user3")
	(if password-status (fi-set-bmc-user-password 3 "") #t))
       ((string-ci=? section-name "user4")
	(if password-status (fi-set-bmc-user-password 4 "") #t)))))

(define (checkout-clear-password section-name) #f)

(define (commit-password section-name password)
  (if (list? password)
      (set! password ""))
  (cond 
   ((string-ci=? section-name "user1")
    (fi-set-bmc-user-password 1 password))
   ((string-ci=? section-name "user2")
    (fi-set-bmc-user-password 2 password))
   ((string-ci=? section-name "user3")
    (fi-set-bmc-user-password 3 password))
   ((string-ci=? section-name "user4")
    (fi-set-bmc-user-password 4 password))))

(define (checkout-password section-name) (list ""))

(define (diff-password section-name user-password bmc-password)
  (cond 
   ((string-ci=? section-name "user1")
    (fi-check-bmc-user-password 1 user-password))
   ((string-ci=? section-name "user2")
    (fi-check-bmc-user-password 2 user-password))
   ((string-ci=? section-name "user3")
    (fi-check-bmc-user-password 3 user-password))
   ((string-ci=? section-name "user4")
    (fi-check-bmc-user-password 4 user-password))))

(define (commit-lan-enable-ipmi-msgs section-name enable-ipmi-msgs)
  (if (list? enable-ipmi-msgs)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-lan-channel-access 1 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-lan-channel-access 2 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-lan-channel-access 3 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-lan-channel-access 4 enable-ipmi-msgs 0 0 #f #f)))))

(define (checkout-lan-enable-ipmi-msgs section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
         (param-list (fi-get-bmc-user-lan-channel-access userid)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-lan-enable-link-auth section-name enable-link-auth)
  (if (list? enable-link-auth)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-lan-channel-access 1 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-lan-channel-access 2 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-lan-channel-access 3 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-lan-channel-access 4 0 enable-link-auth 0 #f #f)))))

(define (checkout-lan-enable-link-auth section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-lan-channel-access userid)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-lan-enable-restrict-to-callback section-name enable-restrict-to-callback)
  (if (list? enable-restrict-to-callback)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-lan-channel-access 1 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-lan-channel-access 2 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-lan-channel-access 3 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-lan-channel-access 4 0 0 enable-restrict-to-callback #f #f)))))

(define (checkout-lan-enable-restrict-to-callback section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-lan-channel-access userid)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-lan-privilege-limit section-name privilege-limit)
  (if (list? privilege-limit)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-lan-channel-access 1 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-lan-channel-access 2 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-lan-channel-access 3 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-lan-channel-access 4 0 0 0 privilege-limit #f)))))

(define (checkout-lan-privilege-limit section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-lan-channel-access userid)))
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-lan-session-limit section-name session-limit)
  (if (list? session-limit)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-lan-channel-access 1 0 0 0 #f session-limit))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-lan-channel-access 2 0 0 0 #f session-limit))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-lan-channel-access 3 0 0 0 #f session-limit))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-lan-channel-access 4 0 0 0 #f session-limit)))))

(define (checkout-lan-session-limit section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-lan-channel-access userid)))
    (if (list? param-list) (cddddr param-list) #f)))


(define (commit-sol-payload-access section-name sol-payload-access)
  (if (list? sol-payload-access)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-sol-payload-access 1 sol-payload-access))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-sol-payload-access 2 sol-payload-access))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-sol-payload-access 3 sol-payload-access))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-sol-payload-access 4 sol-payload-access)))))

(define (checkout-sol-payload-access section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-sol-payload-access userid)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-serial-enable-ipmi-msgs section-name enable-ipmi-msgs)
  (if (list? enable-ipmi-msgs)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-serial-channel-access 1 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-serial-channel-access 2 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-serial-channel-access 3 enable-ipmi-msgs 0 0 #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-serial-channel-access 4 enable-ipmi-msgs 0 0 #f #f)))))

(define (checkout-serial-enable-ipmi-msgs section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-serial-channel-access userid)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-serial-enable-link-auth section-name enable-link-auth)
  (if (list? enable-link-auth)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-serial-channel-access 1 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-serial-channel-access 2 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-serial-channel-access 3 0 enable-link-auth 0 #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-serial-channel-access 4 0 enable-link-auth 0 #f #f)))))

(define (checkout-serial-enable-link-auth section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-serial-channel-access userid)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-serial-enable-restrict-to-callback section-name enable-restrict-to-callback)
  (if (list? enable-restrict-to-callback)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-serial-channel-access 1 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-serial-channel-access 2 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-serial-channel-access 3 0 0 enable-restrict-to-callback #f #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-serial-channel-access 4 0 0 enable-restrict-to-callback #f #f)))))

(define (checkout-serial-enable-restrict-to-callback section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-serial-channel-access userid)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-serial-privilege-limit section-name privilege-limit)
  (if (list? privilege-limit)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-serial-channel-access 1 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-serial-channel-access 2 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-serial-channel-access 3 0 0 0 privilege-limit #f))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-serial-channel-access 4 0 0 0 privilege-limit #f)))))

(define (checkout-serial-privilege-limit section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-serial-channel-access userid)))
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-serial-session-limit section-name session-limit)
  (if (list? session-limit)
      #t 
      (cond 
       ((string-ci=? section-name "user1")
	(fi-set-bmc-user-serial-channel-access 1 0 0 0 #f session-limit))
       ((string-ci=? section-name "user2")
	(fi-set-bmc-user-serial-channel-access 2 0 0 0 #f session-limit))
       ((string-ci=? section-name "user3")
	(fi-set-bmc-user-serial-channel-access 3 0 0 0 #f session-limit))
       ((string-ci=? section-name "user4")
	(fi-set-bmc-user-serial-channel-access 4 0 0 0 #f session-limit)))))

(define (checkout-serial-session-limit section-name)
  (let* ((userid (cond 
		((string-ci=? section-name "user1") 1)
		((string-ci=? section-name "user2") 2)
		((string-ci=? section-name "user3") 3)
		((string-ci=? section-name "user4") 4)))
	 (param-list (fi-get-bmc-user-serial-channel-access userid)))
    (if (list? param-list) (cddddr param-list) #f)))

(define user-keys-validator 
  '(
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERSION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERSION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ("username" 
     valid-username-password? 
     get-string 
     commit-username 
     checkout-username 
     get-string
     same-string?
     "Give username")
    ("enable_user" 
     valid-boolean? 
     get-boolean 
     commit-enable-user 
     checkout-enable-user 
     get-string
     diff-enable-user
     "Possible values: Yes/No or blank to not set")
    ("clear_password" 
     valid-boolean? 
     get-boolean 
     commit-clear-password 
     checkout-clear-password 
     get-boolean-string
     same-string?
     "Possible values: Yes/No")
    ("password" 
     valid-username-password? 
     get-string 
     commit-password 
     checkout-password 
     get-string
     diff-password
     "Give password or leave it blank to clear password")
    ("lan_enable_ipmi_msgs" 
     valid-boolean? 
     get-boolean 
     commit-lan-enable-ipmi-msgs 
     checkout-lan-enable-ipmi-msgs 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("lan_enable_link_auth" 
     valid-boolean? 
     get-boolean 
     commit-lan-enable-link-auth 
     checkout-lan-enable-link-auth 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("lan_enable_restrict_to_callback" 
     valid-boolean? 
     get-boolean 
     commit-lan-enable-restrict-to-callback 
     checkout-lan-enable-restrict-to-callback 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("lan_privilege_limit" 
     valid-privilege-limit? 
     get-privilege-limit 
     commit-lan-privilege-limit 
     checkout-lan-privilege-limit 
     get-privilege-limit-value-string 
     same-string-ci?
     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access")
    ("lan_session_limit" 
     valid-integer? 
     get-integer 
     commit-lan-session-limit 
     checkout-lan-session-limit 
     any->string 
     same-string-ci?
     "Give valid number")
    ("sol_payload_access" 
     valid-boolean? 
     get-boolean 
     commit-sol-payload-access
     checkout-sol-payload-access
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("serial_enable_ipmi_msgs" 
     valid-boolean? 
     get-boolean 
     commit-serial-enable-ipmi-msgs 
     checkout-serial-enable-ipmi-msgs 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("serial_enable_link_auth" 
     valid-boolean? 
     get-boolean 
     commit-serial-enable-link-auth 
     checkout-serial-enable-link-auth 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("serial_enable_restrict_to_callback" 
     valid-boolean? 
     get-boolean 
     commit-serial-enable-restrict-to-callback 
     checkout-serial-enable-restrict-to-callback 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("serial_privilege_limit" 
     valid-privilege-limit? 
     get-privilege-limit 
     commit-serial-privilege-limit 
     checkout-serial-privilege-limit 
     get-privilege-limit-value-string
     same-string-ci?
     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access")
    ("serial_session_limit" 
     valid-integer? 
     get-integer 
     commit-serial-session-limit 
     checkout-serial-session-limit 
     any->string 
     same-string-ci?
     "Give valid number")
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERSION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERSION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ))
