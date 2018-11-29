(in-package :org.unclesniper.sloth.artifact)

(defparameter *assume-all-out-of-date* nil)

(defclass artifact nil
  ((path
     :initarg :pathname
     :accessor artifact-pathname)
   (mimetype
     :initarg :type
     :initform "application/octet-stream"
     :accessor artifact-type)
   (updater
     :initarg :updater
     :initform nil
     :accessor artifact-updater)
   (properties
     :initform (make-hash-table)
     :reader artifact-properties)
   (prerequisites
     :initform (make-hash-table :test 'equal))
   (known-up-to-date-p
     :initarg :known-up-to-date
     :initform nil
     :accessor artifact-known-up-to-date-p)))

(defun artifact (path &key (mimetype nil has-type-p) updater known-up-to-date)
  (apply #'make-instance
         (nconc
           (list
             'artifact
             :pathname (pathname path)
             :updater updater
             :known-up-to-date known-up-to-date)
           (when has-type-p (list :type mimetype)))))

(defun artifact-property (artifact key)
  (gethash key (artifact-properties artifact)))

(defun set-artifact-property (artifact key value)
  (setf (gethash key (artifact-properties artifact)) value))

(defsetf artifact-property set-artifact-property)

(defun artifact-prerequisites (artifact)
  (with-slots (prerequisites) artifact
    (loop for prq being the hash-values of prerequisites collect prq)))

(defun add-artifact-prerequisites (artifact &rest prereqs)
  (with-slots (prerequisites) artifact
    (loop for prq in prereqs
          do (setf (gethash (artifact-pathname prq) prerequisites) prq))))

(defun remove-artifact-prerequisites (artifact &rest prereqs)
  (with-slots (prerequisites) artifact
    (loop for prq in prereqs
          do (remhash (artifact-pathname prq) prerequisites))))

(defmethod print-object ((object artifact) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (path mimetype properties) object
      (format stream "~s :type ~s :properties {" (namestring path) mimetype)
      (print-properties properties stream)
      (format stream "}"))))

(defun print-properties (properties stream)
  (loop
    for k being the hash-keys of properties using (hash-value v)
    for head = t then nil
    do (format stream "~:[, ~;~]~s -> ~s" head k v)))

(defun artifact-out-of-date-p (artifact)
  (with-slots (path prerequisites known-up-to-date-p) artifact
    (let ((truename (file-exists-p path)))
      (cond
        (known-up-to-date-p t)
        (*assume-all-out-of-date* nil)
        ((not truename) t)
        ((directory-pathname-p truename) nil)
        (t (let ((awtime (file-write-date truename)))
             (or
               (not awtime)
               (loop
                 for prq being the hash-keys of prerequisites
                 for pwtime = (let ((ptname (file-exists-p prq)))
                                (and ptname (file-write-date ptname)))
                 when (and pwtime (> pwtime awtime)) return t))))))))

(define-condition dont-know-how-to-update (error)
  ((artifact :initarg :artifact :reader dont-know-how-to-update-artifact)))

(defun dont-know-how-to-update (destination &rest sources)
  (declare (ignore sources))
  (error 'dont-know-how-to-update :artifact destination))

(defun update-artifact (artifact &key always)
  (with-slots (path prerequisites updater) artifact
    (when (or always (> (hash-table-size prerequisites) 0))
      (funcall (or updater #'dont-know-how-to-update)
               artifact
               (loop for prq being the hash-values of prerequisites collect prq)))))

;(defun require-artifact (artifact &key (if-up-to-date :keep) (if-no-prerequisites :skip))
;  foo
;  )
