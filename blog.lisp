(defvar *if-loaded* nil)
(unless *if-loaded*
  (asdf:oos 'asdf:load-op 'cl-who)
  (asdf:oos 'asdf:load-op 'hunchentoot)
  (asdf:oos 'asdf:load-op 'sqlite)
  (setf *if-loaded* t))
;;file coding(utf-8)
#+ccl
(setf ccl:*default-external-format* :utf-8)


(defpackage cfy.blog
  (:use :cl :cl-who :hunchentoot :sqlite))

(in-package :cfy.blog)
(defparameter *root* "/home/cfy/blog/blog/")
(defparameter *blog-database-name* (concatenate 'string "/home/cfy/blog/" "blog.sqlite3"
) "The name of the database we will work in.")
(defparameter *articles* (concatenate 'string *root* "articles/"))
(defparameter *style* (concatenate 'string *root* "styles/"))
(defparameter *images* (concatenate 'string *root* "images/"))
(defparameter *robots* (concatenate 'string *root* "robots.txt"))

(defvar *if-http-server-started* nil)
(defvar *db* (connect *blog-database-name*))
(defvar *http-server* (make-instance 'hunchentoot:acceptor :port 4242))
(setf *catch-errors-p* nil)
(setf *default-handler* 'default)

(defun latin1->utf-8(string)
  (flex:octets-to-string
   (flex:string-to-octets string)
   :external-format :utf-8))
;; (defun utf-8->latin1(string)
;;   (flex:octets-to-string
;;    (flex:string-to-octets string :external-format :utf-8)))

;; (defun get-decoding(get-results)
;;   (mapcar
;;    (lambda (x)
;;      (mapcar
;;       (lambda (x)
;; 	(if (stringp x)
;; 	    (latin1->utf-8 x)
;; 	    x))
;;       x))
;;    get-results))

;; (defmacro para-encoding (&body body)
;;   (let ((func (caar body))
;; 	(args (loop for i in (cdar body)
;; 		 if (stringp i)
;; 		 collect (utf-8->latin1 i)
;; 		 else
;; 		 collect i
;; 		 end)))
;;     `(,func ,@args)))

(defun now()
	(multiple-value-bind (s m h d mon y)
	    (get-decoded-time)
	  (format nil "~a-~a-~a ~a:~a:~a" y mon d h m s)))

;; do frontend function
(defun if-exist-table(name)
  (execute-to-list *db* "select name from sqlite_master where type = 'table' and name = ?" name))
(defun if-exist-index(name)
  (execute-to-list *db* "select tbl_name from sqlite_master where type = 'index' and tbl_name = ?" name))

(defun get-max-article-id()
  (execute-single *db* "select max(id) from 'articles'"))
(defun get-max-comments-id()
  (execute-single *db* "select max(id) from 'comments'"))
(defun get-next-available-article-id()
  (let ((max (get-max-article-id)))
    (if max
	(1+ max)
	0)))
(defun get-next-available-comments-id()
  (let ((max (get-max-comments-id)))
    (if max
	(1+ max)
	0)))
(defun get-articles(key value)
  (execute-to-list *db*
		   (format nil "select * from 'articles' where ~a = ?" key)
		   value))
(defun get-comments(key value)
  (execute-to-list *db*
		   (format nil "select * from 'comments' where ~a = ?" key)
		   value))
(defun save-article(title &key(path "/dev/null")(date (now))(id (get-next-available-article-id)))
  (execute-non-query *db*
		     "insert into 'articles' (id,title,path,date) values (?,?,?,?)" id title path date))
(defun save-comment(article-id &key (name "nil") (email "nil") (content "nil") (web-site "nil")(date (now))(id (get-next-available-comments-id)))
  (execute-non-query *db*
		     "insert into 'comments' (id,article,name,web_site,email,content,date) values (?,?,?,?,?,?,?)" id article-id name web-site email content date))
(defun get-next(type id)
  (execute-single *db*
		  (format nil "select id from '~a' where id > ? order by id" type)
		  id))
(defun get-previous(type id)
  (execute-single *db*
		  (format nil "select id from '~a' where id < ? order by id desc" type)
		  id))
(defun delete-record(type id)
  (execute-non-query *db*
		     (format nil "delete from '~a' where id = ?" type)
		     id))
(defun get-all(type)
  (execute-to-list *db*
		   (format nil "select * from '~a'" type)))

(defun get-title-by-id(id)
  (cadar (get-articles "id" id)))

(defun get-lastest-n-articles(n)
  (execute-to-list *db*
		   "select * from 'articles' order by id desc limit ?" n))
;; initial
(loop for (i table index) in '(("articles"
				"create table 'articles' (id number not null, title text not null, path text not null,date text, constraint articlepk primary key(id));"
				"create index 'index_articles' on 'articles' (id,title,path,date)")
			       ("comments"
				"create table 'comments' (id number not null, article number not null, name text not null, web_site text, email text not null,content text not null, date text, constraint commentspk primary key(id));"
				"create index 'index_comments' on 'comments' (id,article,name,web_site,email,content,date)"))
     do (unless
	    (if-exist-table i)
	  (execute-non-query *db* table))
     do (unless
	    (if-exist-index i)
	  (execute-non-query *db* index)))
(unless (get-max-article-id)
  (save-article "main"))

(defun get-content(path)
  "get body content from html,not include <body> and </body>"
  (with-open-file (in path)
    (let ((a (make-array (file-length in) :adjustable t))
	  l start end)
      (setf l (read-sequence a in))
      (adjust-array a l)
      (setf start (+ 6 (search "<body>" a)))
      (setf end (search "</body>" a))
      (concatenate 'string (subseq a start end)))))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

;; page id:-1 mean redirect
(defmacro standard-page ((&key title)(&key page-id time url (page-comments t) (comment-post t)) &body body)
  `(with-html
     (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	    (:head
	     (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
	     (if (= -1 ,page-id)(htm (:meta :http-equiv "REFRESH" :content (format nil "~a;url=~a" ,time ,url))))
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "/style/style.css"))
	    (:body
	     (:div :id "header"
		   (:a :href "/default.html"
		       (:img :src "/images/logo120x80.png"
			     :alt "Made with Lisp"
			     :id "lisplogo"))
		   (:div :id "debianlogo"
			 (:h1 "debian")))
	     (:div :id "main"
		   (:div :id "navigation"
			 (:ul :id "blog-links"
			      (:li
			       (:p "Blog links"))
			      (:li (:a :href "http://tusooa.tk/" "tusooa"))
			      (:li (:a :href "http://maskray.tk/" "Maskray"))
			      (:li (:a :href "http://machinelife.org/" "kandu"))
			      (:li (:a :href "add-article.html" "add article"))
			      (:li (:a :href "http://validator.w3.org/check?uri=referer" (:img :src "http://www.w3.org/Icons/valid-xhtml10" :alt "Valid XHTML 1.0 Strict" :height"31" :width "88" )))))
		   (:div :id "main-content"
			 ,@body
			 (if (and ,page-comments (get-comments "article" ,page-id))
			     (htm (:div :id "page-comments"
					(:p "Comments:")
					(:ol
					 (loop for (id nil name nil nil content) in (get-comments "article" ,page-id)
					    for i from 0  
					    do (htm
						(:li :class (format nil "~[evenbox~;oddbox~]" (mod i 2))
						     (:span :class "comment-id" (str id))
						     (:span :class "name" (str name))
						     (:p (str content))))))))))
		   (if ,comment-post
		       (htm (:div :id "comments"
				  (:p :id "comments-title" "Comments")
				  (:form :action "comments.html" :method "post"
					 (:table
					  (:tbody
					   (:tr
					    (:td (:input :type "hidden" :name "page-id" :value ,page-id))
					    (:th))
					   (:tr
					    (:td (:input :type "text" :name "name"))
					    (:th "Name(required)"))
					   (:tr
					    (:td (:input :type "text" :name "email"))
					    (:th "Email(will not be published)(required)"))
					   (:tr 
					    (:td (:textarea :name "comments" :rows "10" :cols "48"))
					    (:th "Your Comments"))
					   (:tr
					    (:td (:input :type "submit" :value "Submit"))
					    (:th)))))))))))))
	    

;; (defun cookies-test()
;;   (set-cookie "pumpkin" :value (write-to-string (1+ (parse-integer (cookie-in "pumpkin")))))
;;   (no-cache)
;;   (with-html
;;     (:html
;;      (:head (:title "cookie test"))
;;      (:body
;;       (:p (str (concatenate 'string "hello,world" (cookie-in "pumpkin"))))))))

(defun escape-not-ascii(string)
  (escape-string string :test #'(lambda (c)(> (char-code c) 127))))
(defmacro escape-not-ascii!(&rest args)
  `(progn ,@(loop for i in args  collect (list 'setf i `(escape-not-ascii ,i)))))

(defun parse-title-from-path(string)
  (let ((a (1+ (position #\/ string :from-end t)))
	(b (search ".html" string)))
    (subseq string a b)))

(defun html->plain(string)
  (substitute #\] #\>
	      (substitute #\[ #\< string)))

(defmacro html->plain!(&rest args)
  `(progn ,@(loop for i in args  collect (list 'setf i `(html->plain ,i)))))

(defun mainpage(&optional (record (car (get-articles "id" 0))))
  (let ((id (car record))
	(title (cadr record))
	(path (caddr record)))
    (cond
      ((> id 0)
       (standard-page
	(:title (str (escape-not-ascii title)))
	(:page-id id)
	(str (escape-not-ascii (get-content path)))))
      ((= id 0)
       (standard-page
      	   (:title (str (escape-not-ascii title)))
      	   (:page-id id)
      	 (:ul :id "articles"
      	      (loop for (id title nil date) in (get-all "articles")
      		 if (> id 0)
      		 do (htm (:li (:span :class "date" (str date))
      			      (:a :href (format nil "/default?title=~a" title) (str (escape-string title))))))))))))

(defun add-article()
  (let ((articles (directory (concatenate 'string *articles* "*.html"))))
    (loop for j in articles
       for i = (latin1->utf-8 (namestring j))
       do (unless (get-articles "path" i)
  	    (save-article (parse-title-from-path i) :path i))))
  (escape-not-ascii (mainpage)))

(defun default()
  (no-cache)
  (setf (content-type*)
        (format nil "text/html; charset=~A" :utf-8))
  (recompute-request-parameters :external-format
                                (flex:make-external-format :utf-8 :eol-style :lf))
  (let* ((title (get-parameter "title"))
	 (get (car (get-articles "title" title))))
    (if get
	(escape-not-ascii (mainpage get))
	(escape-not-ascii (mainpage)))))
	      
(defun handle-comments-post()
  (setf (content-type*)
        (format nil "text/html; charset=~A" :utf-8))
  (recompute-request-parameters :external-format
                                (flex:make-external-format :utf-8 :eol-style :lf))
  (let ((id (or (post-parameter "page-id") ""))
	(name (or (post-parameter "name") ""))
	(email (or (post-parameter "email") ""))
	(comments (or (post-parameter "comments") "")))
    (if (and 
	 (string/= "" id)
	 (string/= "" name)
	 (string/= "" email)
	 (string/= "" comments)
	 (get-articles 'id id)
	 (< (length name) 160)
	 (< (length email) 160))
	(progn
	  (html->plain! name email comments)
	  (escape-not-ascii! name email comments)
	  (save-comment id :name name :email email :content comments)
	  (standard-page
	      (:title "handle comment post")
	      (:page-id -1 :url (format nil "default.html?title=~a" (get-title-by-id id)) :time 3)
	    (:p (str (format nil "~{[~a]~}" `(,id ,name ,email ,comments))))
	    (:p "Please wait for forwarding,i have received your comments :)")))


	(standard-page
	    (:title "your post is not correct")
	    (:page-id (parse-integer id) :page-comments nil)
	  (:p "below is your post")
	  (:table
	   (:tr
	    (:td (str (format nil "{~a}" name)))
	    (:th "name(required)(up to 20 characters)"))
	   (:tr
	    (:td (str (format nil "{~a}" email)))
	    (:th "email(required)(will not be published)(up to 50 characters)"))
	   (:tr
	    (:td (str (format nil "{~a}" comments)))
	    (:th "your comments(required")))))))

(progn
  (push (create-folder-dispatcher-and-handler "/style/" *style*) *dispatch-table*)
  (push (create-folder-dispatcher-and-handler "/images/" *images*) *dispatch-table*)
  (push (create-prefix-dispatcher "/add-article.html" 'add-article) *dispatch-table*)
  (push (create-prefix-dispatcher "/default" 'default) *dispatch-table*)
  (push (create-prefix-dispatcher "/comments.html" 'handle-comments-post) *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler "/robots.txt" *robots*) *dispatch-table*))
(unless *if-http-server-started*
  (hunchentoot:start *http-server*)
  (setf *if-http-server-started* t))
;; (hunchentoot:stop *http-server*)

(defun enable-flash()
  (load "/home/cfy/clp/downloads.lisp")
  (load "/home/cfy/clp/base64.lisp")
  (defun flash()
	   (encode64 (cfy.downloads:flash-from-content (ccl:decode-string-from-octets (hunchentoot:raw-post-data)))))
  (push (create-prefix-dispatcher "/downloads" 'flash) *dispatch-table*))