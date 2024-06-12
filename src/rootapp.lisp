;;(ql:quickload '(:reblocks :reblocks-ui :reblocks-navigation-widget :mito))
(uiop:define-package #:mbfamilysys/rootapp
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:reblocks/app
		#:defapp)
  (:import-from #:reblocks-ui/form)
  (:import-from #:reblocks/html)
  (:import-from #:reblocks-navigation-widget
		#:defroutes)
  (:import-from #:reblocks/js/base
		#:with-javascript) ;;trick to resolve naming conflicts  (unintern 'quickstart::with-javascript '#:quickstart)
  (:import-from #:reblocks/session
		#:reset)
  (:import-from #:parenscript)
  (:import-from #:mito)
  (:import-from #:mbfamilysys/taskapp)
  (:import-from #:mbfamilysys/otherapp)
  (:export #:@mbfamilysys/rootapp
	   #:*stat*
	   #:rootapp))
(in-package #:mbfamilysys/rootapp)

;;use (reblocks/server:start :port 5050 :interface "0.0.0.0" :apps '(taskapp otherapp rootapp)) to start server
;;though we have defroutes as following, but the defapp for each route is still a MUST
;;and you also need to add apps list you want to run into the server:start arguments

(defparameter *stat* nil)

(defroutes apps-routes
    ("/" (make-home-page))
    ("/tasks/" (make-task-list))
  ("/others" (make-others)))

(reblocks/app:defapp rootapp :prefix "/")
(reblocks/widget:defwidget homepage ()
  ((title
    :initarg :title
    :initform "Welcome to Michael's family system!"
    :reader get-title)
   (body
    :initarg :body
    :initform "Here are list of family function in the system you could visit:"
    :reader get-body)
   (functions
    :initarg :func
    :initform '(gallery calendar task blog)
    :reader get-funcs)))
(defun make-home-page ()
  (make-instance 'homepage))
(defmethod reblocks/widget:render ((hpw homepage))
    "Render Homepage."
    (reblocks/html:with-html
      (:h1 "Welcome to Michael's family system homepage!")
      (:h2 "There are following functions in this family system:"))(reblocks/html:with-html 
  (:h3 (get-title hpw))
  (:p (get-body hpw))
  (:ul
   (loop for e in (get-funcs hpw) do
	 (:li e)))))
;;it seems only root uri "/" need this init-page method, other routes could be done through defroutes!
(defmethod reblocks/page:init-page ((app rootapp) (url-path string) expire-at)
  (declare (ignorable app url-path expire-at))
  (make-home-page)
  (make-apps-routes))

(reblocks/server:start :port 5050 :interface "0.0.0.0" :apps '(rootapp mbfamilysys/taskapp:taskapp mbfamilysys/otherapp:otherapp))
