;;;; cl-pandorum.lisp
;;; See LICENSE on details of copying.

(in-package #:cl-pandorum)

(defun let-binding-transform (bs)
  (iter (for binding in bs)
	(collect (cond ((and (symbolp binding) binding)
			`(,binding))
		       ((consp binding) binding)
		       (t (error "Bad let binding: ~a"
				 binding))))))

(defmacro! pandoriclet (letargs &rest body)
  (let ((letargs `((,e!-this)
                   ,(let-binding-transform letargs))))
    `(let (,@letargs)
       (setq ,e!-this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
	    (apply ,e!-this args))))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar (lambda (x)
		 `((,(car x)) ,(car x)))
               letargs)
     (t (error
          "Unknown pandoric get: ~a"
          sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar (lambda (x)
		 `((,(car x))
                   (setq ,(car x) val)))
               letargs)
     (t (error
          "Unknown pandoric set: ~a"
          sym))))

(declaim (inline get-pandoric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro! with-pandoric (syms box &rest body)
  `(let ((,g!-box ,box))
     (declare (ignorable ,g!-box))
     (symbol-macrolet
         (,@(mapcar (lambda (x)
		      `(,x (get-pandoric ,g!-box ',x)))
                    syms))
         ,@body)))

(defun pandoric-hotpatch (box new)
  (with-pandoric (e!-this) box ; whatever that means for
					; now
		 (setq e!-this new)))


(defmacro pandoric-recode (vars box new)
  `(with-pandoric (e!-this ,@vars) ,box
		  (setq e!-this ,new)))



(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)
                  ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)
                  ,(pandoriclet-set pargs))
                (t (&rest args)
                  (apply this args)))))))



(defvar pandoric-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
           (plambda () ,vars t)))
     (eval `(with-pandoric
              ,',vars pandoric-eval-tunnel
              ,,expr))))


