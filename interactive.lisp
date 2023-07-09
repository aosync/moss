(in-package #:moss)

(defun viewhash (hash)
  (loop :for key :being :each :hash-key :in hash
	  :using (:hash-value value)
	:do (format t "~a(~a)~%" key value)))

(defparameter bb1 (make-instance 'basic-block))
(defparameter tag0 (make-instance 'tag))
(defparameter tagr0 (make-instance 'tagref :inner tag0))
(vector-push-extend (make-instance 'biop :volatile t
					 :lhs tagr0
					 :rhs tagr0)
		    (bb-forms bb1))


(defparameter bb0 (make-instance 'basic-block))
(vector-push-extend (make-instance 'datum :tag tag0) (bb-forms bb0))
(push bb1 (bb-next bb0))

(bb-pass-make-epoch bb0)
(bb-pass-clean bb0)
(bb-pass-link-loci bb0)
(bb-pass-replace-loci bb0)

(let ((locus (make-instance 'locus)))
  (format t "~a~%" locus))
