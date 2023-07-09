(in-package #:moss)

(defclass basic-block ()
  ((epoch
    :initform 0
    :accessor bb-epoch    
    :documentation "Topologically consistant basic block execution number")
   (deps
    :initform (make-hash-table)
    :accessor bb-deps
    :documentation "TAGs required by the basic block")
   (products
    :initform (make-hash-table)
    :accessor bb-products
    :documentation "TAGs made by the basic block. Association TAG to DATUM.")
   (forms
    :initform (make-array 0
	       :element-type 'datum
	       :adjustable t
	       :fill-pointer 0)
    :initarg :forms
    :accessor bb-forms
    :documentation "List of DATUMs evaluated in sequence in the BASIC-BLOCK.")
   (next
    :initform '()
    :accessor bb-next
    :documentation "List of BASIC-BLOCKs dominated by current."))
  (:documentation
   "A basic block of IR code declaring its dependencies and what it provides"))

(defmethod bb-pass-reset-epoch ((self basic-block))
  "Reset the epoch of the BASIC-BLOCK and those dominated by it."
  (when (/= (bb-epoch self) 0)
    (setf (bb-epoch self) 0)
    (dolist (next (bb-next self))
      (bb-pass-reset-epoch next))))

(defmethod bb-pass-make-epoch ((self basic-block) &optional (epoch 1))
  "Make the epochs of the BASIC-BLOCK and those dominated by it."
  (when (or (= (bb-epoch self) 0)
	    (< epoch (bb-epoch self)))
    (setf (bb-epoch self) epoch)
    (dolist (next (bb-next self))
      (bb-pass-make-epoch next (+ epoch 1)))))

(defmethod bb-make-deps ((self basic-block))
  "Find the dependencies of the BASIC-BLOCK."
  (with-slots (deps forms products) self
    (setf deps (make-hash-table))
    (loop :for form :across forms
	  :do (bb-find-deps-in self form))
    (loop :for key :being :each :hash-key :in products
	    :using (:hash-value value)
	  :when (eq key value)
	    :do (let ((tagref (make-instance 'tagref :inner value)))
		  (setf (gethash key products) tagref)
		  (setf (gethash key deps) tagref)))))

(defmethod bb-find-deps-in ((self basic-block) (datum datum))
  "Find the dependencies in a DATUM."
  (dolist (child (dt-children datum))
    (bb-find-deps-in self child)))

(defmethod bb-find-deps-in ((self basic-block) (tagref tagref))
  "Add the TAG to the dependencies."
  (setf (gethash (tref-inner tagref) (bb-deps self))
	tagref))

(defmethod bb-make-products ((self basic-block))
  "Make the product map by first reporting all the deps, then all the DATUMs with a TAG attached."
  (with-slots (deps forms products next) self
    (setf products	  
	  (let ((newprods (make-hash-table)))
	    (dolist (next next)
	      (loop :for dep :being :each :hash-key :in (bb-deps next)
		    :when (gethash dep products)
		      :do (setf (gethash dep newprods) (gethash dep products))
		    :do (setf (gethash dep newprods) dep)))
	    newprods))
    (loop :for form :across forms
	  :do (bb-find-products-in self form))))

(defmethod bb-find-products-in ((self basic-block) (datum datum))
  "Helper method to navigate through the DATUM trees in the BASIC-BLOCK."
  (dolist (child (dt-children datum))
    (bb-find-products-in self child))
  (with-slots (tag) datum
    (with-slots (products) self
      (when (and tag (gethash tag products))
	(setf (gethash tag products) datum)))))

(defmethod bb-relax-order ((self basic-block))
  "Relax the relative order of execution of the non-volatile DATA."
  (setf (bb-forms self)
	(let ((forms (make-array 0
				 :element-type 'datum
				 :adjustable t
				 :fill-pointer 0)))
	  (loop :for form :across (bb-forms self)
		:when (dt-volatile form)
		  :do (vector-push-extend form forms))
	  forms)))

(defmethod bb-pass-clean ((self basic-block))
  "Clean the BASIC-BLOCKs: make PRODUCTs, relax order and make DEPs."
  (with-slots (epoch next) self
    (dolist (next next)
      (when (< epoch (bb-epoch next))
	(bb-pass-clean next)))
    (bb-make-products self)
    (bb-relax-order self)
    (bb-make-deps self)))

(defmethod bb-pass-link-loci ((self basic-block))
  "Link adjacent LOCI together."
  (loop :for key :being :each :hash-key :in (bb-products self)
	  :using (:hash-value value)
	:do (let ((locus (make-instance 'locus)))
	      (lc-mark-replaced-by (dt-locus value) locus)
	      (dolist (next (bb-next self))
		(let ((value (gethash key (bb-deps next))))
		  (when value
		    (lc-mark-replaced-by (dt-locus value) locus))))))
  (dolist (next (bb-next self))
    (when (< (bb-epoch self)
	     (bb-epoch next))
      (bb-pass-link-loci next))))

(defmethod bb-pass-replace-loci ((self basic-block))
  "Replace the DATA's LOCI by what they should be replaced. Follows BB-PASS-LINK-LOCI."
  (loop :for form :across (bb-forms self)
	:do (dt-pass-replace-loci form))
  (loop :for form :being :each :hash-value :in (bb-products self)
	:do (dt-pass-replace-loci form))
  (dolist (next (bb-next self))
    (when (< (bb-epoch self)
	     (bb-epoch next))
      (bb-pass-replace-loci next))))
