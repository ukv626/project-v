(in-package :project-v)

;; (defparameter *db*  (mongo :db "test" :name :test))
(db.use "test") ;; :mongo *db*)

(defun user-add (login password &key
                 (firstname "") (lastname "")
                 birthday (sex t) (city "Санкт-Петербург"))
  (cond ((not (docs (iter (db.find "users" (kv (kv "login" login))))))
         (let ((doc (make-document)))
           (add-element "login" login doc)
           (add-element "password" password doc)
           (add-element "firstname" firstname doc)
           (add-element "lastname" lastname doc)
           (add-element "birthday" birthday doc)
           (add-element "sex" sex doc)
           (add-element "city" city doc)
           (db.insert "users" doc)))
        (t nil)))

(defun user-modify (login &key firstname lastname city)
  (let ((user (car (docs (iter (db.find "users" (kv "login" login)))))))
    (cond (user
           (if (not (null firstname)) (add-element "firstname" firstname user))
           (if (not (null lastname)) (add-element "lastname" lastname user))
           (if (not (null city)) (add-element "city" city user))
           (db.save "users" user)
           t))))
  

(defun user-add-hobbies (login array)
  (let ((user (car (docs (iter (db.find "users" (kv "login" login)))))))
    (cond (user
           (add-element "hobbies" array user)
           (db.save "users" user)
           t))))

(defun user-add-friend (login friend-login)
  (let* ((user (car (docs (iter (db.find "users" (kv "login" login))))))
         (cur-friends (get-element "friends" user)))
    (cond (user
           (add-element "friends" (append cur-friends (list friend-login)) user)
           (db.save "users" user)
           t))))

(defun user-by-login-password (login password)
  (car (docs
        (iter
         (db.find "users" (kv (kv "login" login)
                              (kv "password" password)))))))

(defun str-to-date-time (str)
  "string date to mongo/bson date"
  (when (= 16 (length str)) 
    (let ((second 0)
          (minute (parse-integer str :start 14 :end 16 :junk-allowed t))
          (hour (parse-integer str :start 11 :end 13 :junk-allowed t))
          (day (parse-integer str :start 8 :end 10 :junk-allowed t))
          (month (parse-integer str :start 5 :end 7 :junk-allowed t))
          (year (parse-integer str :end 4 :junk-allowed t)))
      (and (>= minute 0) (< minute 60)
           (>= hour 0) (< hour 24)
           (> day 0) (<= day 31)
           (> month 0) (<= month 12)
           (date-time second minute hour day month year)))))

(defun request-add (login &key text desc type tags people skills dates)
  (let ((cl-mongo::*mongo-registry* nil))
    (with-mongo-connection (:db "test")
      (let ((doc (make-document))
            (userid (cl-mongo::make-bson-oid
                     :oid (doc-id (first (docs (db.find "users"
                                                        (kv "login" login))))))))
        (add-element "userid" userid doc)
        (add-element "login" login doc)
        (multiple-value-bind
              (second minute hour day month year)
            (get-decoded-time)
          (add-element "date" (date-time second minute hour day month year) doc))
        (add-element "text" text doc)
        (add-element "desc" desc doc)
        (add-element "type" type doc)
        (if (not (null tags))
            (add-element "tags" tags doc))
        (if (not (null people))
            (add-element "people" people doc))
        (if (not (null skills))
            (add-element "skills" skills doc))
        (if (not (null dates))
            (add-element "dates"
                         (loop for cur-date in dates
                            for doc-date = (kv (kv :start (getf cur-date :start))
                                               (kv :finish (getf cur-date :finish)))
                            collect doc-date) doc))
        (db.insert "requests" doc)
        (log-add login
                 (format nil "Участник \"~A\" добавил заявку \"~A\""
                         login text))))))

(defun request-by-id (id)
  (car (docs (iter (db.find "requests" (kv "_id"  (cl-mongo::make-bson-oid :oid (mongoid:oid id))))))))

(defun requests-by-user (user-login)
  (let ((cl-mongo::*mongo-registry* nil))
    (with-mongo-connection (:db "test")
      (docs (iter (db.find "requests" (kv "login" user-login)
                           :limit 0))))))

(defun requests-by-user-hobbies (user-login)
  (let ((user (car (docs (iter (db.find "users" (kv "login" user-login)
                                        :limit 0))))))
    (when user
      (let ((hobbies (get-element "hobbies" user)))
        (when hobbies
          ;; TODO: - check if db.find returns nil
          (docs (iter
                  (db.find "requests" (kv ($!= "login" user-login)
                                          ($in "type" hobbies))
                           :limit 0))))))))

(defun request-add-wishin (id login)
  (let* ((request (car (docs (iter (db.find "requests" (kv "_id" id))))))
         (cur-values (get-element "wishin" request))
         (author (get-element "login" request)))
    (cond (request
           (add-element "wishin" (append cur-values (list login)) request)
           (db.save "requests" request)
           (log-add author (format nil "Участник \"~A\" желает принять участие в заявке \"~A\" от DD.MM.YYYY" login (get-element "text" request)))
           t))))

(defun request-del-wishin (id login)
  (let* ((request (car (docs (iter (db.find "requests" (kv "_id" id))))))
         (cur-values (get-element "wishin" request)))
    (cond (request
           (add-element "wishin"
                        (remove login cur-values :test #'string=) request)
           (db.save "requests" request)
           t))))

(defun request-add-partner (id login)
  (let* ((request (car (docs (iter (db.find "requests" (kv "_id" id))))))
         (cur-values (get-element "partners" request))
         (author (get-element "login" request)))
    (cond (request
           (add-element "partners" (append cur-values (list login)) request)
           (db.save "requests" request)
           (request-del-wishin id login)
           (log-add author (format nil "Участник \"~A\" добавлен к заявке \"~A\" от DD.MM.YYYY" login (get-element "text" request)))
           t))))


(defun request-del-partner (id login)
  (let* ((request (car (docs (iter (db.find "requests" (kv "_id" id))))))
         (cur-values (get-element "partners" request)))
    (cond (request
           (add-element "partners"
                        (remove login cur-values :test #'string=) request)
           (db.save "requests" request)
           t))))


(defun log-add (login text)
  (let ((doc (make-document)))
    (multiple-value-bind
          (second minute hour day month year)
        (get-decoded-time)
      (add-element "date" (date-time second minute hour day month year) doc))
    (add-element "login" login doc)
    (add-element "text" text doc)
    (db.insert "logs" doc)))

(defun logs-by-user (login)
  (let ((cl-mongo::*mongo-registry* nil))
    (with-mongo-connection (:db "test")
    (let ((user (car (docs (iter (db.find "users" (kv "login" login)
                                          :limit 0))))))
      (when user
        (let ((friends (get-element "friends" user)))
          (when friends
            ;; TODO: check if db.find returns nil
              (docs (iter
                      (db.find "logs" ($in "login"
                                           (append (list login) friends))
                               :limit 0))))))))))

(defun show-result (collection kv)
  (let ((results
         (docs (iter (db.find collection kv)))))
    (loop for doc in results
       collect
         (mapdoc #'(lambda (k v)
                     (format t "~S : ~S~&" k v))
                     ;; (cons k v))
                 doc))))
    
;; (defun binary-example ()
;;   (with-open-file (stream "~/Work/Lisp/qwe.txt") :element-type '(unsigned-byte 8)
;;                   (let ((seq (make-array (file-length stream)
;;                                          :element-type 'character
;;                                          :fill-pointer t)))
;;                     (setf (fill-pointer seq) (read-sequence seq stream))
;;                     (let ((doc (make-document)))
;;                       (add-element "info" seq doc)
;;                       (db.insert "info" doc))))
  
;; (pp (db.find "requests" (kv "needs" (kv "artist" "скрипач"))))
;; (request-add :login "ukv" :text "хочу устроить мега-романтик" :needs (list (kv "artist" "скрипач") (kv "n" 6)))
;; (pp (db.find "requests" (kv "userid" (cl-mongo::make-bson-oid :oid (mongoid:oid "99380668169c45a881059dc3"))) :limit 0))
;; (pp (db.find "requests" (kv "text" ($/ "^устроить" "i"))))
;; (get-element "login" (docs (db.find "test" (kv "login" "ukv626@gmail.com"))))
;; (pp (iter (db.find "users" (kv (kv "login" "ukv") 
;;                                           (kv "password" "njgktcc")))))
;; (pp (db.find "requests" ($in "needs" (list "повар" "мексиканец")) :limit 0))
;; (db.find "requests" ($> "dates.start" (date-time 00 00 12 16 07 2012)))
;; (pp (db.find "requests" (kv "_id"  (cl-mongo::make-bson-oid :oid (mongoid:oid "50110E573433E58C511A7158")))))
  
;;(kv <key> <value> ) is cl-mongo's equivalent of the { : } key-value constructor in java script.
;; (kv can also be used to group key-value pairs.

;; beautifull
;; (let ((counter 0)) (defun gen-id () (incf counter)))
