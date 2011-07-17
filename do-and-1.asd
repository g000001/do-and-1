;;;; do-and-1.asd

(cl:in-package :asdf)

(defsystem :do-and-1
  :serial t
  :depends-on (:fare-matcher)
  :components ((:file "package")
               (:file "do-and-1")))

(defmethod perform ((o test-op) (c (eql (find-system :do-and-1))))
  (load-system :do-and-1)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :do-and-1-internal :do-and-1))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

