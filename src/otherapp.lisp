(uiop:define-package #:mbfamilysys/otherapp
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
  (:export #:@mbfamilysys/otherapp
	   #:otherapp))
(in-package #:mbfamilysys/otherapp)

(reblocks/app:defapp otherapp :prefix "/others/")
(reblocks/widget:defwidget others ()
  ())
(defun make-others ()
  (make-instance 'others))
(defmethod reblocks/widget:render ((ow others))
    "Render others."
    (reblocks/html:with-html
      (:h1 "Other apps")
      (:p "To show other apps here")))

(defmethod reblocks/page:init-page ((app otherapp) (url-path string) expire-at)
  (declare (ignorable app url-path expire-at))
  (make-others))
