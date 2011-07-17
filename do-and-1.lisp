;;;; do-and-1.lisp

(cl:in-package :do-and-1-internal)

(def-suite do-and-1)

(in-suite do-and-1)

;;; "do-and-1" goes here. Hacks and glory await!


(defmacro do& (clauses &body body)
  (let ((clauses (do&-clauses clauses))
        (again (gensym)))
    `(prog (,@(apply #'append
                     (mapcan (lambda (e)
                               (and e (list (getf e :init))))
                             clauses)))
       ,again
           (progn
             ,@(mapcar (lambda (e) (getf e :body))
                       clauses))
           (progn ,@body)
           (progn
             ,@(mapcar (lambda (e) (getf e :next))
                       clauses))
           (go ,again))))

(defun do&-clauses (clauses)
  (mapcar (lambda (c)
            (match c
              ;;
              (`(,elt &aref ,array &return ,acc)
                (let ((idx (gensym "IDX-"))
                      (len (gensym "LEN-")))
                  `(:init
                    ((,elt nil)
                     (,idx 0)
                     (,len (length ,array)))
                    :body
                    (progn
                      (when (= ,len ,idx)
                        (return ,acc))
                      (setq ,elt (aref ,array ,idx)))
                    :next (incf ,idx))))
              (`(,elt &pop ,lst &return ,acc)
                `(:init
                  ((,elt nil)
                   (,lst ,lst))
                  :body
                  (progn
                    (when (null ,lst)
                      (return ,acc))
                    (setq ,elt (car ,lst)))
                  :next (pop ,lst)))
              (`(,var &count ,init)
                `(:init
                  ((,var ,init))
                  :body ()
                  :next (incf ,var)))
              (`(,acc &push ,var)
                `(:init
                  ((,acc () ))
                  :body (push ,var ,acc)
                  :next () ))
              (`(,acc &push ,var &only-if ,pred)
                `(:init
                  ((,acc () ))
                  :body (when ,pred (push ,var ,acc))
                  :next () ))
              (`(,acc &push ,var &return-if ,pred)
                `(:init
                  ((,acc () ))
                  :body (progn (push ,var ,acc)
                               (when ,pred (return ,acc)))
                  :next () ))))
          clauses))

(test do&
  ;; listarray
  (defun listarray (array)
    (do& ((elt &aref array
               &return (nreverse list))
          (list &push elt))))
  (is (equal (listarray #(1 2 3 4 5))
             '(1 2 3 4 5)))
  ;; reverse
  (defun &reverse (list)
    (do& ((elt &pop list &return accum)
          (accum &push elt))))
  (is (equal (&reverse '(1 2 3 4 5 6))
             '(6 5 4 3 2 1)))
  ;; remq
  (defun remq (item list)
    (do& ((a &pop list &return (nreverse out))
          (out &push a
               &only-if (not (eq a item))))))
  (is (equal (remq 'item '(1 2 3 4 5 item 6))
             '(1 2 3 4 5 6)))
  ;;
  (defun &length (list)
    (do& ((i &count 0)
          (l &pop list &return i))))
  (is (= (&length '(1 2 3 4 5))
         5))
  )
