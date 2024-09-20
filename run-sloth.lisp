#!/usr/bin/env -S sbcl --script

; According to CLHS, this should work anywhere (ASDF is supported).
;(require "asdf")

; UIOP comes with ASDF, so we can use it.
(require "uiop")

; Fake it up...
(defpackage #:usdo-sloth-boot
	(:use #:cl))

(in-package #:usdo-sloth-boot)

; Parse out the path evars
(defvar *file-separator-char* (or
	#+unix #\Slash
	#+(or windows win32) #\Backslash
	(error "Unrecognized platform, what is the file separator on this?")
))
(defvar *path-separator-char* (uiop:inter-directory-separator))

(defvar *file-separator-string* (string *file-separator-char*))
(defvar *path-separator-string* (string *path-separator-char*))

; jajaja
(defun rudimentary-split (haystack needle &optional (start 0))
	(let ((p (position needle haystack :start start)))
		(if p
			(let ((tail (rudimentary-split haystack needle (1+ p))))
				(if (> p start)
					(cons (subseq haystack start p) tail)
					tail))
			(if (< start (length haystack))
				(list (subseq haystack start))))))

; loadage

(defclass load-failure-handler () ())

(defgeneric on-load-failure (handler script semantics conditio)
	(:method (handler script semantics conditio)
		(error (format nil
			"Fell back to condition ~S in on-load-failure for script '~A' (type ~S), handler = ~S"
			conditio script semantics handler))))

(defvar *load-failure-handler* nil)

(defparameter *current-script-semantics* nil)

(defun load-script (path allow-not-exist semantics)
	(handler-case
		(let ((*current-script-semantics* semantics))
			(load path :if-does-not-exist (not allow-not-exist)))
		(error (conditio)
			(on-load-failure *load-failure-handler* path semantics conditio))))

; prepare scripts

(defun recalculate-scripts-string (old-value keep evar)
	(let ((from-evar (if (and evar (> (length evar) 0)) (uiop:getenvp evar))))
		(if (and keep old-value)
			(if from-evar
				(format nil "~A~A~A" old-value *path-separator-string* from-evar)
				old-value)
			from-evar)))

(defun recalculate-scripts-list (old-value keep paths)
	(let ((from-string (and paths (rudimentary-split paths *path-separator-char*))))
		(if (and keep old-value)
			(append old-value from-string)
			from-string)))

(defun glob-sym (prefix suffix)
	(intern (string-upcase (format nil "*~A-~A*" prefix suffix))))
(defun updater-sym (prefix suffix)
	(intern (string-upcase (format nil "update-~A-~A" prefix suffix))))

(defmacro def-script-stage (name default-evar)
	(let
		(
			(scripts-evar (glob-sym name "scripts-evar"))
			(scripts-string (glob-sym name "scripts-string"))
			(update-scripts-string (updater-sym name "scripts-string"))
			(scripts (glob-sym name "scripts"))
			(update-scripts (updater-sym name "scripts"))
		)
		`(progn
			(defvar ,scripts-evar ,default-evar)
			(defvar ,scripts-string nil)
			(defun ,update-scripts-string (keep)
				(setf ,scripts-string (recalculate-scripts-string ,scripts-string keep ,scripts-evar)))
			(,update-scripts-string nil)
			(defvar ,scripts nil)
			(defun ,update-scripts (keep)
				(setf ,scripts (recalculate-scripts-list ,scripts keep ,scripts-string)))
			(,update-scripts nil))))

(defun lisp-files-in-dir (path)
	(mapcar #'uiop:native-namestring (directory (format nil "~A~A*.lisp" path *file-separator-char*))))
(defun mod-sloth-files-in-subdirs (path)
	(mapcar #'uiop:native-namestring (directory
		(format nil "~A~A*~Amod.sloth" path *file-separator-char* *file-separator-char*))))

(defvar *user-conf-dir* (uiop:native-namestring "~/.usdo-sloth"))

(defparameter *preload-system-default-script* (or
	#+unix "/etc/usdo-sloth/preload.lisp"
	nil
))
(def-script-stage "preload-system" "USDO_SLOTH_PRELOAD_SYSTEM_SCRIPTS")

(defparameter *preload-user-default-script*
	(format nil "~A~A~A" *user-conf-dir* *file-separator-char* "preload.lisp"))
(def-script-stage "preload-user" "USDO_SLOTH_PRELOAD_USER_SCRIPTS")

(defparameter *boot-system-default-script* (or
	#+unix "/etc/usdo-sloth/boot.lisp"
	nil
))
(def-script-stage "boot-system" "USDO_SLOTH_BOOT_SYSTEM_SCRIPTS")
(defvar *boot-system-scripts-aux* nil)
(defun slate-boot-system-script (script)
	(pushnew script *boot-system-scripts-aux* :test #'equal))

(defparameter *boot-user-default-script*
	(format nil "~A~A~A" *user-conf-dir* *file-separator-char* "boot.lisp"))
(def-script-stage "boot-user" "USDO_SLOTH_BOOT_USER_SCRIPTS")
(defvar *boot-user-scripts-aux* nil)
(defun slate-boot-user-script (script)
	(pushnew script *boot-user-scripts-aux* :test #'equal))

(defparameter *init-system-default-script* (or
	#+unix "/etc/usdo-sloth/init.lisp"
	nil
))
(def-script-stage "init-system" "USDO_SLOTH_INIT_SYSTEM_SCRIPTS")
(defvar *init-system-scripts-aux* nil)
(defun slate-init-system-script (script)
	(pushnew script *init-system-scripts-aux* :test #'equal))

(defparameter *init-user-default-script*
	(format nil "~A~A~A" *user-conf-dir* *file-separator-char* "init.lisp"))
(def-script-stage "init-user" "USDO_SLOTH_INIT_USER_SCRIPTS")
(defvar *init-user-scripts-aux* nil)
(defun slate-init-user-script (script)
	(pushnew script *init-user-scripts-aux* :test #'equal))

(defvar *modules-dirs-evar* "USDO_SLOTH_MODULES_DIRS")
(defvar *modules-dirs-string* nil)
(defun update-modules-dirs-string (keep)
	(setf *modules-dirs-string* (recalculate-scripts-string *modules-dirs-string* keep *modules-dirs-evar*)))
(update-modules-dirs-string nil)
(defvar *modules-dirs* nil)
(defun update-modules-dirs (keep)
	(setf *modules-dirs* (recalculate-scripts-list *modules-dirs* keep *modules-dirs-string*)))
(update-modules-dirs nil)

; prepare modules

; Thing about ASDF is, they always try and walk it in...
; Unfortunately, ASDF doesn't seem to allow us to sink
; our hacking claws into it deeply enough to control all
; of the resolution. We'll just roll our own here...

; module-error-handler
(defclass module-error-handler () ())

(defgeneric on-module-header-error (handler script conditio)
	(:method (handler script conditio)
		(error (format nil
			"Fell back to condition ~S in on-module-header-error for script '~A', handler = ~S"
			conditio script handler))))

(defgeneric on-module-missing (handler modname parent))

(defgeneric on-module-load-error (handler module conditio))

(defgeneric on-module-shadow (handler old-module new-module))

(defgeneric on-module-dependency-cycle (handler module))

(defgeneric on-module-did-not-load (handler module))

(defvar *module-error-handler* nil)

; module
(defclass module () ())

(defgeneric module-name (module)
	(:method (module)
		(error (format nil "Module ~S does not provide a name" module))))

(defgeneric module-definition-location (module)
	(:method ((module module)) nil))

(defgeneric module-dependencies (module)
	(:method (module) nil))

(defgeneric module-should-load (module)
	(:method (module) t))

(defgeneric module-load (module))

(defun module-name-or-symbolic (module)
	(let ((modname (if (typep module 'module) (module-name module))))
		(if modname
			(format nil "'~A'" modname)
			(format nil "~S" module))))

(defun module-name-and-location (module)
	(let ((location (module-definition-location module)) (name (module-name-or-symbolic module)))
		(if location
			(format nil "~A [~A]" name location)
			name)))

(defmethod on-module-missing (handler modname parent)
	(error (format nil "Missing module '~A' required by ~A" modname (module-name-or-symbolic parent))))

(defmethod on-module-load-error (handler module conditio)
	(error (format nil
		"Fell back to condition ~S in on-module-load-error for module ~A, handler = ~S"
		conditio (module-name-or-symbolic module) handler)))

(defmethod on-module-shadow (handler old-module new-module)
	(format *error-output*
		"Warning: Ignoring module ~A, as it would shadow ~A~%"
		(module-name-and-location new-module)
		(module-name-and-location old-module)))

(defmethod on-module-did-not-load (handler module)
	(format *error-output*
		"Warning: Module ~A did not load, but configuration causes startup to continue, anyay.~%"
		(module-name-and-location module)))

; registry
(defvar *module-registry* (make-hash-table :test #'equal))

(defun register-module (module)
	(if (not (typep module 'module))
		(error (format nil "Cannot register module ~S, as it is not a module" module)))
	(let ((name (module-name module)))
		(if (not (stringp name))
			(error (format nil "Module ~S has non-string name: ~S" module name)))
		(let ((old-module (gethash name *module-registry*)))
			(if old-module
				(on-module-shadow *module-error-handler* old-module module)
				(setf (gethash name *module-registry*) module)))))

(defun find-module (modname)
	(gethash (if (symbolp modname) (string-downcase (symbol-name modname)) modname) *module-registry*))

(defparameter *modules-currently-loading* nil)
(defparameter *currently-loading-module* nil)

(defmethod on-module-dependency-cycle (handler module)
	(error (format nil
		"Circular dependency detected: Module ~A ultimately depends on itself: ~S"
		(module-name-and-location module)
		*modules-currently-loading*)))

(defun try-load-module (module impl)
	(if (not (typep module 'module))
		(error (format nil "Cannot load ~S, as it is not a module" module)))
	(if (member module *modules-currently-loading* :test #'eq)
		(progn
			(on-module-dependency-cycle *module-error-handler* module)
			nil)
		(let ((*modules-currently-loading* (cons module *modules-currently-loading*)))
			(funcall impl module))))

(defun load-module-dependencies (module)
	(loop for depname in (module-dependencies module) do
		(if depname
			(let ((dependency (find-module depname)))
				(if dependency
					(module-load dependency)
					(progn
						(on-module-missing *module-error-handler* depname module)
						(return-from load-module-dependencies))))))
	t)

; module-base
(defclass module-base (module) (
	(name :initarg :name :initform (error "no :name for module-base") :reader module-name)
	(is-loaded :initform nil)
	(dependencies :initarg :dependencies :initform nil :accessor module-base-dependencies)
	(should-load :initarg :should-load :initform t :accessor module-base-should-load)))

(defmethod module-should-load ((module module-base)) (module-base-should-load module))

(defmethod module-dependencies ((module module-base)) (reverse (module-base-dependencies module)))

(defgeneric module-base-add-dependency (module dependency)
	(:method ((module module-base) dependency)
		(cond
			((stringp dependency) t)
			((symbolp dependency) (setf dependency (string-downcase (symbol-name dependency))))
			((typep dependency 'module)
				(let ((name (module-name module)))
					(if (not name)
						(error (format nil
							"Cannot add module ~A as dependency of ~A, as the former has no name"
							(module-name-and-location dependency)
							(module-name-and-location module))))
					(setf dependency name)))
			(t (error (format nil
				"Cannot add dependency of module ~A, as it is not a module designator: ~S"
				(module-name-and-location module)
				dependency))))
		(pushnew dependency (module-base-dependencies module) :test #'equal)))

(defgeneric real-module-load (module))

(defmethod module-load ((module module-base))
	(cond
		((slot-value module 'is-loaded) t)
		((try-load-module module #'real-module-load) (setf (slot-value module 'is-loaded) t))))

; header-enrichable-module
(defclass header-enrichable-module (module) ())

(defgeneric enrich-module-with-header (module name dependencies bodies))

(defmethod enrich-module-with-header ((module header-enrichable-module) name dependencies bodies)
	(error (format nil
		"Attempted to enrich module ~A with header info, but it does not define this (is a ~S)"
		(module-name-and-location module) (type-of module))))

(defmacro defmodule (name &key depends-on scripts)
	`(enrich-module-with-header *currently-loading-module* ',name ',depends-on ',scripts))

; dir-module
(defclass dir-module (module-base header-enrichable-module) (
	(header :initarg :header :initform (error "no :header for dir-module") :reader dir-module-header)
	(dir :initarg :dir :initform (error "no :dir for dir-module") :reader dir-module-dir)
	(scripts :initarg :scripts :initform nil :accessor dir-module-scripts)))

(defmethod module-definition-location ((module dir-module)) (dir-module-header module))

(defgeneric dir-module-load-scripts (module))

(defmethod real-module-load ((module dir-module))
	(and
		(load-module-dependencies module)
		(dir-module-load-scripts module)))

(defvar *module-body-target-package* nil)

(defmethod dir-module-load-scripts ((module dir-module))
	(loop for script in (dir-module-scripts module) do
		(if script
			(handler-case
				(let
					(
						(*currently-loading-module* module)
						(*current-script-semantics* :module-body)
						(*package* (or *module-body-target-package* (find-package '#:common-lisp-user)))
					)
					(load script))
				(error (conditio)
					(on-module-load-error *module-error-handler* module conditio)
					(return-from dir-module-load-scripts)))))
	t)

(defmethod enrich-module-with-header ((module dir-module) name dependencies bodies)
	(if name
		(setf (slot-value module 'name) (cond
			((stringp name) name)
			((symbolp name) (string-downcase (symbol-name name)))
			(t (error (format nil "Cannot use ~S as name for dir-module, must be string or symbol" name))))))
	(loop for dep in dependencies do
		(if dep (module-base-add-dependency module dep)))
	(let ((dir (dir-module-dir module)))
		(if (not (stringp dir))
			(error (format nil "Directory of dir-module is not a string: ~S" dir)))
		(setf (dir-module-scripts module)
			(loop for body in bodies
				when body
				collect (format nil "~A~A~A.lisp" dir *file-separator-char* (cond
					((stringp body) body)
					((pathnamep body) (uiop:native-namestring body))
					((symbolp body) (string-downcase (symbol-name body)))
					(t (error (format nil
						"Cannot add ~S as body of dir-module ~A, must be nil, a string, a pathname, or a symbol"
						body
						(module-name-and-location module)))))))))
	module)

; dir-module-creator
(defclass dir-module-creator () ())

(defgeneric create-dir-module (creator name header dir))

(defmethod create-dir-module (creator name header dir)
	(make-instance 'dir-module
		:name (cond
			((stringp name) name)
			((symbolp name) (string-downcase (symbol-name name)))
			(t (error (format nil "Cannot use ~S as name for dir-module, must be string or symbol" name))))
		:header (cond
			((stringp header) header)
			((pathnamep header) (uiop:native-namestring header))
			(t (error (format nil
				"Cannot use ~S as header-path for dir-module, must be a string or pathname" header))))
		:dir (cond
			((null dir) (uiop:native-namestring (uiop:pathname-directory-pathname
				(if (stringp header) (pathname header) header))))
			((stringp dir) dir)
			((pathnamep dir) (uiop:native-namestring dir))
			(t (error (format nil
				"Cannot use ~S as dir-path for dir-module, must be nil, a string, or a pathname" dir))))))

(defvar *dir-module-creator* nil)

; dir-module-factory
(defclass dir-module-factory () ())

(defgeneric dir-module-from-header (factory header-path))

(defmethod dir-module-from-header (factory header-path)
	(let*
		(
			(header-pathname (cond
				((stringp header-path) (pathname header-path))
				((pathnamep header-path) header-path)
				(t (error (format nil
					"Cannot use ~S as header path for dir-module, must be a string or pathname"
					header-path)))))
			(full-header (uiop:ensure-absolute-pathname header-pathname (uiop:getcwd)))
			(full-dir (uiop:pathname-directory-pathname full-header))
			(native-header (uiop:native-namestring full-header))
			(native-dir (uiop:native-namestring full-dir))
			(last-dir-component (first (last (pathname-directory full-header))))
			(name (if (stringp last-dir-component) last-dir-component "unnamed-module"))
			(module (create-dir-module *dir-module-creator* name native-header native-dir))
		)
		(unless module
			(error (format nil
				"No module returned from create-dir-module (*dir-module-creator* = ~S)" *dir-module-creator*)))
		(handler-case
			(let ((*currently-loading-module* module) (*current-script-semantics* :module-header))
				(load full-header))
			(error (conditio)
				(on-module-header-error *module-error-handler* native-header conditio)))
		module))

(defvar *dir-module-factory* nil)

; module-scanner
(defclass module-scanner () ())

(defgeneric scan-for-modules (scanner))

(defgeneric get-modules-dirs (scanner))

(defgeneric found-module-header (scanner header))

(defmethod scan-for-modules (scanner)
	(loop for dir in (get-modules-dirs scanner) do
		(loop for header in (and dir (mod-sloth-files-in-subdirs dir)) do
			(found-module-header scanner header))))

(defmethod get-modules-dirs (scanner) *modules-dirs*)

(defmethod found-module-header (scanner header)
	(let ((module (dir-module-from-header *dir-module-factory* header)))
		(if module
			(register-module module))))

(defvar *module-scanner* nil)

; flush scripts

(defun merge-scripts-default (conf-list default)
	(or conf-list (and default (list default))))

(loop for script in (merge-scripts-default *preload-system-scripts* *preload-system-default-script*) do
	(load-script script t :preload-system))

(loop for script in (merge-scripts-default *preload-user-scripts* *preload-user-default-script*) do
	(load-script script t :preload-user))

(loop for script in (merge-scripts-default *boot-system-scripts* *boot-system-default-script*) do
	(load-script script t :boot-system))

(loop for script in (reverse *boot-system-scripts-aux*) do
	(load-script script t :boot-system-aux))

(loop for script in (merge-scripts-default *boot-user-scripts* *boot-user-default-script*) do
	(load-script script t :boot-user))

(loop for script in (reverse *boot-user-scripts-aux*) do
	(load-script script t :boot-user-aux))

; scan and load modules

(defpackage #:usdo-sloth-user
	(:use #:cl))

(unless *module-body-target-package*
	(setf *module-body-target-package* (find-package '#:usdo-sloth-user)))

(scan-for-modules *module-scanner*)

(loop for module being the hash-value of *module-registry* do
	(if (and (module-should-load module) (not (module-load module)))
		(on-module-did-not-load *module-error-handler* module)))

; finish initialization

(let ((target-package (or *module-body-target-package* (find-package '#:usdo-sloth-user))))
	(loop for script in (merge-scripts-default *init-system-scripts* *init-system-default-script*) do
		(let ((*package* target-package))
			(load-script script t :init-system)))
	(loop for script in (reverse *init-system-scripts-aux*) do
		(let ((*package* target-package))
			(load-script script t :init-system-aux)))
	(loop for script in (merge-scripts-default *init-user-scripts* *init-user-default-script*) do
		(let ((*package* target-package))
			(load-script script t :init-user)))
	(loop for script in (reverse *init-user-scripts-aux*) do
		(let ((*package* target-package))
			(load-script script t :init-user-aux))))

; Kickoff!
(in-package #:usdo-sloth-user)
