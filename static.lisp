(in-package #:moss)

(defclass tag () ()
  (:documentation
   "Object indicating that two different DATA represents the same thing."))

(defmethod tag-make-ref ((self tag))
  "Creates a TAGREF from the TAG."
  (make-instance 'tagref :inner self))

(defclass locus ()
  ((replace-by
   :initform nil
   :accessor lc-replace-by
   :documentation "LOCUS to replace current LOCUS at next pass."))
  (:documentation
   "Represents a location backing DATA, with its characteristics."))

(defmethod lc-last-replacing-locus ((self locus))
  "Get the final replacing LOCUS, they stack in case a pass creates multiple new LOCI for DATUMs."
  (with-slots (replace-by) self
    (if replace-by
	(lc-last-replacing-locus replace-by)
	self)))

(defmethod lc-mark-replaced-by ((self locus) (by locus))
  "Mark the LOCUS to be replaced by another LOCUS."
  (let ((last-replacing (lc-last-replacing-locus self)))
    (unless (eq last-replacing by)
      (setf (lc-replace-by last-replacing) by))))

(defclass datum ()
  ((locus
    :initform (make-instance 'locus)
    :accessor dt-locus)
   (tag
    :initform nil
    :initarg :tag
    :accessor dt-tag    
    :documentation "DATUM's tag")
   (volatile
    :initform nil
    :initarg :volatile
    :accessor dt-volatile
    :documentation "DATUM's volatility"))
  (:documentation
   "Piece of DATA."))

(defmethod dt-pass-replace-loci ((self datum))
  "Pass to replace the LOCI in the DATUM."
  (dolist (child (dt-children self))
    (dt-pass-replace-locus child))
  (setf (dt-locus self)
	(lc-last-replacing-locus (dt-locus self))))
  
(defmethod dt-children ((self datum))
  "Get a list of the children of the DATUM."
  '())

(defmethod dt-pass-relax-order ((self datum) forms)
  "Relax the relative order of execution of the non-volatile DATA."
  (dolist (child (dt-children self))
    (dt-pass-relax-order child forms))
  (when (and (dt-volatile self)
	     (not (find self forms)))
    (vector-push-extend self forms)))

(defclass tagref (datum)
  ((inner
    :initarg :inner
    :accessor tref-inner
    :documentation "TAG referenced by this TAGREF.")))

(defclass biop (datum)
  ((lhs
    :initform nil
    :initarg :lhs
    :accessor biop-lhs
    :documentation "Left hand side of BIOP DATUM.")
   (rhs
    :initform nil
    :initarg :rhs
    :accessor biop-rhs
    :documentation "Right hand side of BIOP DATUM.")))

(defmethod dt-children ((self biop))
  (with-slots (lhs rhs) self
    (list lhs rhs)))

(defclass add (biop) ())
