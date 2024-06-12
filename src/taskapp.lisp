(uiop:define-package #:mbfamilysys/taskapp
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
  (:export #:@mbfamilysys/taskapp
	   #:taskapp))
(in-package #:mbfamilysys/taskapp)

(mito:connect-toplevel :sqlite3 :database-name #P"/root/.roswell/lisp/quicklisp/local-projects/mbfamilysys/src/db/mytaskdb")

(mito:deftable taskt ()
  ((title :col-type :text)
   (tag :col-type :text)
   (done :col-type (:integer 0)))
  (:conc-name task-))

;;(if (mito:ensure-table-exists 'taskt)
;;    nil
;;    (mapc #'mito:execute-sql (mito:table-definition 'taskt)))

;;(defvar ataskt (car (mito:retrieve-dao 'taskt :id 1)))
;;(defvar ataskw (make-instance 'taskw :title (taskt-title ataskt) :done (taskt-done ataskt)))


(reblocks/app:defapp taskapp :prefix "/tasks/")
(reblocks/widget:defwidget task ()
    ((title
      :initarg :title
      :accessor title)
     (tag
      :initarg :tag
      :accessor tag)
     (done
      :initarg :done
      :initform 0
      :accessor done)))
  
  (reblocks/widget:defwidget task-list ()
    ((tasks
      :initarg :tasks
      :accessor tasks)))

(reblocks/widget:defwidget msgbox ()
  ((title
    :initarg :title
    :accessor title
    :initform "message")))
(defun make-msgbox (title)
  (make-instance 'msgbox :title title))
(defmethod reblocks/widget:render ((mb msgbox))
  (reblocks/html:with-html
    (:p (title mb))
    (:div :class "alert"
	  (:span :class "closebtn"
		 :onclick "this.parentElement.style.display='none';")
	  :value "the task is about to delete")))

(defun combine-title-tag (task)
  (format nil "~A:~A" (title task)(tag task)))
  

(defmethod reblocks/widget:render ((task task))
    (reblocks/html:with-html
      (:p (:input :type "checkbox"
                  :checked (if (= 0 (done task)) nil t)
                  :onclick (reblocks/actions:make-js-action
                            (lambda (&key &allow-other-keys)
                              (toggle task))))
          (:span (if (= 1 (done task))
		     (reblocks/html:with-html
                       ;; strike
                       (:s (combine-title-tag task)))
                     (combine-title-tag task)))
	  (:input :type "submit"
		  :class "button"
		  :value "Delete"
		  :onclick (reblocks/actions:make-js-action
                            (lambda (&key &allow-other-keys)
                              (del-task task)))))))

  (defmethod reblocks/widget:render ((widget task-list))
    "Render a list of tasks."
    (reblocks/html:with-html
      (:h1 "Tasks")
      (:ul
       (loop for task in (tasks widget) do
         (:li (reblocks/widget:render task))))))

  (defun make-task (title &key tag done)
    (make-instance 'task :title title :tag tag :done done))

(defun make-task-list ()
  (make-instance 'task-list
		:tasks
		(loop for row in (mito:retrieve-dao 'taskt) collect
		  (make-task (task-title row) :tag (task-tag row) :done (task-done row)))))
    ;;(make-instance 'task-list
    ;;               :tasks (list (make-task "Make my first Reblocks app" :tag "work")
    ;;                            (make-task "Deploy it somewhere" :tag "work")
    ;;                            (make-task "Have a profit" :tag "personal"))))

;;in original example, add-task is a method (defmethod), i think defun should be fine
;;but also notice that if you define a method without defgeneric, the lisp will automatic generate one for you. so if you wanna change the signature of method, you have to explicitly define defgeneric.
;;(defgeneric add-task (task-list title tag))
(defun add-task (task-list title tag)
    (push (make-task title :tag tag :done 0)
          (tasks task-list))
  (mito:create-dao 'taskt :title title :tag tag :done 0)
    (reblocks/widget:update task-list))
  
  (defmethod reblocks/widget:render ((task-list task-list))
    (reblocks/html:with-html
      (:h1 "Tasks")      
      (:input :type "button"
	      :value "refresh"
	      :onclick (reblocks/js/base:with-javascript
			   ";"))
			;;(lambda (&key &allow-other-keys)
			 ;;"window.location.reload()")));;<- this is not working!
      (loop for task in (tasks task-list) do
        (reblocks/widget:render task))
      (reblocks-ui/form:with-html-form (:POST (lambda (&key title tag &allow-other-keys)
                                                (add-task task-list title tag)))
        (:input :type "text"
                :name "title"
                :placeholder "Task's title")
         (:input :type "text"
                :name "tag"
                :placeholder "Task's tag")
	(:input :type "submit"
                :class "button"
                :value "Add"))))

(defun page-refresh ()
    (if mbfamilysys/rootapp:*stat*
	  "history.go(0);"
	  ";"))

(defmethod toggle ((task task))
  (setf (done task) 1)
  (let ((tosave (mito:find-dao 'taskt :title (title task))))
    (setf (task-done tosave) 1)
    (mito:save-dao tosave))
  (reblocks/widget:update task))


(defmethod del-task ((task task))
  (let ((todelete (mito:find-dao 'taskt :title (title task))))
    (mito:delete-dao todelete))
  (reblocks/widget:update task)
  (reblocks/debug:reset-latest-session)
  (setf mbfamilysys/rootapp:*stat* t))


(defmethod sub-task ((task task))
  (reblocks-ui/form:with-html-form (:POST (lambda (&key &allow-other-keys)
                                                (add-sub-task)))
        (:input :type "text"
                :name "title"
                :placeholder "SubTask's title")
         (:input :type "text"
                :name "tag"
                :placeholder "SubTask's tag")
	(:input :type "submit"
                :class "button"
                :value "Add"))
  (reblocks/widget:update task))

(defun add-sub-task ()
  "to add subtask code here")

(defmethod reblocks/page:init-page ((app taskapp) (url-path string) expire-at)
  (declare (ignorable app url-path expire-at))
  (make-task-list))
