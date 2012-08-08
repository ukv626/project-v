(in-package :project-v)

;; Start our web server.
(defvar *my-server* (start
                     (make-instance 'hunchentoot:easy-acceptor :port 4242)))

;; (setf (ACCEPTOR-MESSAGE-LOG-DESTINATION *my-server*) "~/Work/Lisp/project-v/hunchentoot.log")
;; (setf (ACCEPTOR-ACCESS-LOG-DESTINATION *my-server*) "~/Work/Lisp/project-v/hunchentoot.access.log")
(push
 (create-static-file-dispatcher-and-handler "/style.css"
                                            "~/Work/project-v/style.css")
 *dispatch-table*)

;; (push
;;  (create-static-file-dispatcher-and-handler "/project-v.js"
;;                                             "~/Work/Lisp/project-v/project-v.js")
 ;; *dispatch-table*)

;; default path for htmp-tmplate
(setq html-template:*default-template-pathname* #P"~/Work/project-v/")

;;; ht-simple-ajax
;; (defparameter *ajax-processor* 
;;   (make-instance 'ajax-processor :server-uri "/ajax"))

;; (push
;;  (create-ajax-dispatcher *ajax-processor*) *dispatch-table*)


;; Automatically creates a Hunchentoot handler for the given URL associating 
;; it with a function of the same name.
(defmacro define-url-fn ((name) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-prefix-dispatcher ,(format nil "/~(~a~)" name) ',name)
           *dispatch-table*)))

;; All pages on the site will use the following macro; less to type and 
;; a uniform look of the pages (defines the header and the stylesheet).
(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"  :xml\:lang "en" :lang "en"
       (:head 
	 (:title ,title)
         (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"))
         (:body
          (:h1 "Project-V")
	     ,@body))))


(define-url-fn (login)
    (cond ((eq (request-method *request*) :GET)
           (login-form))
          ((eq (request-method *request*) :POST)
           (let ((login (parameter "login"))
                 (password (parameter "password")))
             (let ((user (user-by-login-password login password)))
               (cond (user
                      (setf (session-value :user)
                            (list login
                                  (get-element "firstname" user)))
                      (with-output-to-string (stream)
                        (json:encode-json-plist (list :auth t) stream)))
                      ;;(redirect "/index"))
                     (t
                      (with-output-to-string (stream)
                        (json:encode-json-plist (list :auth nil) stream) ))))))))

(defmacro with-http-authentication (&rest body)
  `(let ((user-details (session-value :user)))
     (cond (user-details
            ,@body)
           (t (redirect "/login")))))


;; (define-url-fn (signup)
;;   (cond ((eq (request-method *request*) :GET)
;;          (sign-up-form))
;;         ((eq (request-method *request*) :POST)
;;          (let ((user-username (parameter "username"))
;;                (user-password (parameter "password"))
;;                (user-re-password (parameter "re-password"))
;;                (user-first-name (parameter "first-name")))
;;            (cond ((and (not (string= "" user-username))
;;                        (not (string= "" user-first-name))
;;                        (not (string= "" user-password))
;;                        (string= user-password user-re-password))
;;                   (let ((user (make-instance 'users
;;                                              :username user-username
;;                                              :password user-password
;;                                              :first-name user-first-name)))
;;                     (clsql:update-records-from-instance user)
;;                     (setf (session-value :user) (list user-username
;;                                                       (first-name user)))
;;                     (redirect "/index")))
;;                  (t (sign-up-form)))))))


(define-url-fn (index)
  (with-http-authentication
      (with-output-to-string (stream)
        (html-template:fill-and-print-template
         #P"index.html"
         (list :firstname (second user-details))
         :stream stream))))


(defun login-form ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template
     #P"login.html"
     nil
     :stream stream)))

(define-url-fn (logout)
  (delete-session-value :user)
  (redirect "/login"))


(define-url-fn (new-request)
  (with-http-authentication
      (cond ((eq (request-method *request*) :GET)
             (with-output-to-string (stream)
               (html-template:fill-and-print-template
                #P"new-request.html"
                nil
                :stream stream)))
            ((eq (request-method *request*) :POST)
             (let ((text (post-parameter "text"))
                   (desc (post-parameter "description"))
                   (type (post-parameter "type"))
                   (tags (post-parameter "tags"))
                   (people (post-parameter "people"))
                   (skills (post-parameter "skills"))
                   (dateStart (str-to-date-time (post-parameter "dateStart")))
                   (dateEnd (str-to-date-time (post-parameter "dateEnd"))))
               (with-output-to-string (stream)
                 (cond ((or (null text)
                            (null desc)
                            (null type)
                            (null dateStart)
                            (null dateEnd))
                        (json:encode-json-plist (list :success nil) stream))
                       (t
                        (request-add (first user-details)
                                     :text text
                                     :desc desc
                                     :type type
                                     :tags (split-sequence #\Comma tags
                                                           :remove-empty-subseqs t)
                                     :people (split-sequence #\Comma people
                                                             :remove-empty-subseqs t)
                                     :skills (split-sequence #\Comma skills
                                                             :remove-empty-subseqs t)
                                     :dates (list (list :start dateStart
                                                        :finish dateEnd)))
                        (json:encode-json-plist (list :sucess t) stream)))))))))

               ;; (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
               ;;   (:html
               ;;    (:body
               ;;     (:div (:b "text25: ") (:str (parameter "text25"))
                   ;; (:div (:b "description: ") (:str desc))
                   ;; (:div (:b "type: ") (:str type))
                   ;; (:div (:b "tags: ") (:str tags))
                   ;; (:div (:b "people: ") (:str people))
                   ;; (:div (:b "skills: ") (:str skills))
                   ;; (:div (:b "dateStart: ") (:str dateStart))
                   ;; (:div (:b "dateEnd: ") (:str dateEnd))
                   ;; (:a :href "/new-request" "Try again"))))))))

      

;; (defun-ajax lenta nil (*ajax-processor*)
;;   (let ((user-details (session-value :user)))
;;     (cond (user-details
;;            (setf (content-type*) "application/json")
;;            (with-output-to-string (stream)
;;              (json:encode-json (logs-by-user (first user-details)) stream)))
;;            (t (setf (return-code*) +http-authorization-required+)))))


(define-url-fn (lenta.json)
  (let ((user-details (session-value :user)))
    (cond (user-details
           (setf (content-type*) "application/json")
           (with-output-to-string (stream)
             (json:encode-json (logs-by-user (first user-details)) stream)))
           (t (setf (return-code*) +http-authorization-required+)))))

(define-url-fn (lenta2.json)
  (setf (content-type*) "application/json")
  (with-output-to-string (stream)
    (json:encode-json (logs-by-user "ukv") stream)))


(define-url-fn (request.json)
    (let ((user-details (session-value :user)))
      (cond (user-details
             (setf (content-type*) "application/json")
             (with-output-to-string (stream)
             (cond ((eq (request-method *request*) :GET)
                    (let ((id (get-parameter "id")))
                      (cond (id (json:encode-json (request-by-id id) stream))
                            (t  (json:encode-json (requests-by-user
                                                   (first user-details))
                                                  stream)))))
                   ((eq (request-method *request*) :POST)
                    (json:encode-json-plist (list :post t) stream)))))
            (t (setf (return-code*) +http-authorization-required+)))))

  ;; (standard-page (:title "Login")
  ;;   (:h2 "Sign In")
  ;;   ;; (:script :type "text/javascript"
  ;;   ;;          (str (ps (defun validate-username ()
  ;;   ;;                     (let ((emptyp (= username.value "")))
  ;;   ;;                               (cond (emptyp
  ;;   ;;                                 (alert "Please enter a name!!"))
  ;;   ;;                                 (t (alert "qwe!!")))
  ;;   ;;                               x)))))
  ;;   (htm (:form
  ;;         :action "/login" :method "post" :onsubmit "return validateUsername()"
  ;;         (:table :border 1 :cellpadding 5 :cellspacing 0
  ;;                 (:tr
  ;;                  (:td "Login:")
  ;;                  (:td (:input :type "text" :name "username")))
  ;;                 (:tr
  ;;                  (:td "Password:")
  ;;                  (:td (:input :type "password" :name "password")))
  ;;                 (:tr
  ;;                  (:td)
  ;;                  (:td (:input :type "submit" :value "submit")))
  ;;                 (:tr
  ;;                  (:td)
  ;;                  (:td (:a :href "/signup" "Sign Up"))))))))


;; (defun sign-up-form ()
;;   (standard-page (:title "Sign Up")
;;     (:h2 "Sign Up")
;;     (htm (:form
;;           :action "/signup" :method "post"
;;           (:table :border 1 :cellpadding 5 :cellspacing 0
;;                   (:tr
;;                    (:td "Firstname:")
;;                    (:td (:input :type "text" :name "first-name")))
;;                   (:tr
;;                    (:td "Login:")
;;                    (:td (:input :type "text" :name "username")))
;;                   (:tr
;;                    (:td "Password:")
;;                    (:td (:input :type "password" :name "password")))
;;                   (:tr
;;                    (:td "Password:")
;;                    (:td (:input :type "password" :name "re-password")))
;;                   (:tr
;;                    (:td)
;;                    (:td (:input :type "submit" :value "submit"))))))))


;; (defun split-by-comma (string)
;;     "Returns a list of substrings of string
;; divided by ONE comma each.
;; Note: Two consecutive spaces will be seen as
;; if there were an empty string between them."
;;     (loop for i = 0 then (1+ j)
;;           as j = (position #\, string :start i)
;;           collect (subseq string i j)
;;           while j))