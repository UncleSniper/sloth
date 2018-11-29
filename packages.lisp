(in-package :cl-user)

; pathnames.lisp
(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

; utils.lisp
(defpackage :org.unclesniper.sloth.utils
  (:use :common-lisp)
  (:export
   :unrecognized-symbolic-arg
   :by-symbolic-arg))

; artifact.lisp
(defpackage :org.unclesniper.sloth.artifact
  (:use
   :common-lisp
   :com.gigamonkeys.pathnames
   :org.unclesniper.sloth.utils)
  (:export
   :*assume-all-out-of-date*
   :artifact
   :artifact-pathname
   :artifact-type
   :artifact-properties
   :artifact-property
   :set-artifact-property
   :artifact-prerequisites
   :add-artifact-prerequisites
   :remove-artifact-prerequisites
   :artifact-known-up-to-date-p
   :artifact-out-of-date-p
   :dont-know-how-to-update
   :dont-know-how-to-update-artifact))
