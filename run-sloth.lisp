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

; script conditions

(define-condition script-load-failure (error)
	(
		(script
			:initarg :script
			:initform (error "no :script for script-load-failure")
			:reader script-load-failure-script)
		(cause :initarg :cause :initform nil :reader script-load-failure-cause)
	)
	(:report print-object))

(defmethod print-object ((conditio script-load-failure) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (script cause) conditio
			(format out "script = '~A', cause = ~S" script cause))))

(define-condition fallback-script-load-failure (error)
	(
		(script
			:initarg :script
			:initform (error "no :script for fallback-script-load-failure")
			:reader fallback-script-load-failure-script)
		(semantics
			:initarg :semantics
			:initform (error "no :semantics for fallback-script-load-failure")
			:reader fallback-script-load-failure-semantics)
		(cause :initarg :cause :initform nil :reader fallback-script-load-failure-cause)
	)
	(:report print-object))

(defmethod print-object ((conditio fallback-script-load-failure) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (script semantics cause) conditio
			(format out "script = '~A', semantics = ~S, cause = ~S" script semantics cause))))

; misc conditions

(define-condition missing-delegate-error (error)
	(
		(abstraction
			:initarg :abstraction
			:initform (error "no :abstraction for missing-delegate-error")
			:reader missing-delegate-error-abstraction)
		(wrapper
			:initarg :wrapper
			:initform (error "no :wrapper for missing-delegate-error")
			:reader missing-delegate-error-wrapper)
	)
	(:report print-object))

(defmethod print-object ((conditio missing-delegate-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (abstraction wrapper) conditio
			(format out "abstraction = ~S, wrapper = ~S" abstraction wrapper))))

; loadage

; delegator
(defclass delegator () (
	(delegate :initarg :delegate :initform nil :accessor delegator-delegate)
	(prefer-this-over-delegate :initarg :prefer-this-over-delegate :initform nil :accessor prefer-this-over-delegate)))

(defgeneric delegate-or-this (this))

(defmethod delegate-or-this (this) this)

(defmethod delegate-or-this ((this delegator))
	(if (prefer-this-over-delegate this)
		this
		(delegator-delegate this)))

; misc condition stuff

; Credit where credit is due: This is copy-and-pasted from the Common Lisp Cookbook
; (+/- the 'eval' part).
(defun prompt-new-value (prompt)
	(format *query-io* "~S: " prompt)
	(force-output *query-io*)
	(list (eval (read *query-io*))))

; load-failure-handler
(defclass load-failure-handler () ())

(defgeneric on-load-failure (handler script semantics conditio)
	(:method (handler script semantics conditio)
		(error 'fallback-script-load-failure :script script :semantics semantics :cause conditio)))

(defvar *load-failure-handler* nil)

(defparameter *current-script-semantics* nil)

(defun load-script (path allow-not-exist semantics)
	(handler-case
		(let ((*current-script-semantics* semantics))
			(load path :if-does-not-exist (not allow-not-exist)))
		(error (conditio)
			(restart-case
				(on-load-failure *load-failure-handler* path semantics
					(make-condition 'script-load-failure :script path :cause conditio))
				(skip-script () :report "Skip this script" nil)
				(use-new-path (new-path)
					:report "Enter path for replacement script"
					:interactive (lambda nil (prompt-new-value "Form returning replacement script"))
					(load-script new-path allow-not-exist semantics))))))

; callback-load-failure-handler
(defclass callback-load-failure-handler (load-failure-handler delegator) (
	(on-load-failure
		:initarg :on-load-failure
		:initform nil
		:accessor callback-load-failure-handler-on-load-failure)))

(defmethod on-load-failure ((handler callback-load-failure-handler) script semantics conditio)
	(let ((delegate-or (delegate-or-this handler)))
		(with-slots (delegate on-load-failure) handler
			(if on-load-failure
				(funcall on-load-failure delegate-or (lambda nil (call-next-method)) script semantics conditio)
				(on-load-failure delegate script semantics conditio)))))

(defun intercept-load-failure-handler (&key load-failure prefer-this-over-delegate)
	(setf *load-failure-handler* (make-instance 'callback-load-failure-handler
		:delegate *load-failure-handler*
		:prefer-this-over-delegate prefer-this-over-delegate
		:on-load-failure load-failure)))

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

; module conditions

(define-condition module-header-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-header-error")
			:reader module-header-error-module)
		(cause :initarg :cause :initform nil :reader module-header-error-cause)
	)
	(:report print-object))

(defmethod print-object ((conditio module-header-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module cause) conditio
			(format out "module = ~S, cause = ~S" module cause))))

(define-condition module-missing-error (error)
	(
		(name
			:initarg :name
			:initform (error "no :name for module-missing-error")
			:reader module-missing-error-name)
		(parent :initarg :parent :initform nil :reader module-missing-error-parent)
	)
	(:report print-object))

(defmethod print-object ((conditio module-missing-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (name parent) conditio
			(format out "name = '~A', parent = ~S" name parent))))

(define-condition module-load-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-load-error")
			:reader module-load-error-module)
		(body-script :initarg :body :initform nil :reader module-load-error-body)
		(cause :initarg :cause :initform nil :reader module-load-error-cause)
	)
	(:report print-object))

(defmethod print-object ((conditio module-load-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module body-script cause) conditio
			(format out "module = ~S, body-script = ~S, cause = ~S" module body-script cause))))

(define-condition module-shadow-warning (warning)
	(
		(old-module
			:initarg :old-module
			:initform (error "no :old-module for module-shadow-warning")
			:reader module-shadow-warning-old-module)
		(new-module
			:initarg :new-module
			:initform (error "no :new-module for module-shadow-warning")
			:reader module-shadow-warning-new-module)
	)
	(:report print-object))

(defmethod print-object ((conditio module-shadow-warning) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (old-module new-module) conditio
			(format out "old-module = ~S, new-module = ~S" old-module new-module))))

(define-condition module-dependency-cycle-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-dependency-cycle-error")
			:reader module-dependency-cycle-error-module)
		(cycle :initarg :cycle :initform nil :reader module-dependency-cycle-error-cycle)
	)
	(:report print-object))

(defmethod print-object ((conditio module-dependency-cycle-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module cycle) conditio
			(format out "module = ~S~@[, cycle = ~{~S~^ <==[requires]== ~}~]" module cycle))))

(define-condition module-did-not-load-warning (warning)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-did-not-load-warning")
			:reader module-did-not-load-warning-module)
	)
	(:report print-object))

(defmethod print-object ((conditio module-did-not-load-warning) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module) conditio
			(format out "module = ~S" module))))

(define-condition fallback-module-header-error (error)
	(
		(script
			:initarg :script
			:initform (error "no :script for fallback-module-header-error")
			:reader fallback-module-header-error-script)
		(cause :initarg :cause :initform nil :reader fallback-module-header-error-cause)
	)
	(:report print-object))

(defmethod print-object ((conditio fallback-module-header-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (script cause) conditio
			(format out "script = '~A', cause = ~S" script cause))))

(define-condition module-name-missing-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-name-missing-error")
			:reader module-name-missing-error-module)
	)
	(:report print-object))

(defmethod print-object ((conditio module-name-missing-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module) conditio
			(format out "module = ~S" module))))

(define-condition registering-non-module-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for registering-non-module-error")
			:reader registering-non-module-error-module)
	)
	(:report print-object))

(defmethod print-object ((conditio registering-non-module-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module) conditio
			(format out "module = ~S" module))))

(define-condition module-with-non-string-name-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for module-with-non-string-name-error")
			:reader module-with-non-string-name-error-module)
		(name
			:initarg :name
			:initform (error "no :name for module-with-non-string-name-error")
			:reader module-with-non-string-name-error-name)
	)
	(:report print-object))

(defmethod print-object ((conditio module-with-non-string-name-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module name) conditio
			(format out "module = ~S, name = ~S" module name))))

(define-condition trying-to-load-non-module-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for trying-to-load-non-module-error")
			:reader trying-to-load-non-module-error-module)
	)
	(:report print-object))

(defmethod print-object ((conditio trying-to-load-non-module-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module) conditio
			(format out "module = ~S" module))))

(define-condition trying-to-add-nameless-dependency-error (error)
	(
		(parent
			:initarg :parent
			:initform (error "no :parent for trying-to-add-nameless-dependency-error")
			:reader trying-to-add-nameless-dependency-error-parent)
		(child
			:initarg :child
			:initform (error "no :child for trying-to-add-nameless-dependency-error")
			:reader trying-to-add-nameless-dependency-error-child)
	)
	(:report print-object))

(defmethod print-object ((conditio trying-to-add-nameless-dependency-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (parent child) conditio
			(format out "parent = ~S, child = ~S" parent child))))

(define-condition trying-to-add-non-module-dependency-error (error)
	(
		(parent
			:initarg :parent
			:initform (error "no :parent for trying-to-add-non-module-dependency-error")
			:reader trying-to-add-non-module-dependency-error-parent)
		(child
			:initarg :child
			:initform (error "no :child for trying-to-add-non-module-dependency-error")
			:reader trying-to-add-non-module-dependency-error-child)
	)
	(:report print-object))

(defmethod print-object ((conditio trying-to-add-non-module-dependency-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (parent child) conditio
			(format out "parent = ~S, child = ~S" parent child))))

(define-condition trying-to-enrich-non-enrichable-module-with-header-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for trying-to-enrich-non-enrichable-module-with-header-error")
			:reader trying-to-enrich-non-enrichable-module-with-header-error-module)
		(name
			:initarg :module
			:initform nil
			:reader trying-to-enrich-non-enrichable-module-with-header-error-name)
		(target-package
			:initarg :target-package
			:initform nil
			:reader trying-to-enrich-non-enrichable-module-with-header-error-target-package)
		(dependencies
			:initarg :dependencies
			:initform nil
			:reader trying-to-enrich-non-enrichable-module-with-header-error-dependencies)
		(bodies
			:initarg :bodies
			:initform nil
			:reader trying-to-enrich-non-enrichable-module-with-header-error-bodies)
	)
	(:report print-object))

(defmethod print-object ((conditio trying-to-enrich-non-enrichable-module-with-header-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module name target-package dependencies bodies) conditio
			(format out
				"module = ~S, name = ~S, target-package = ~S, dependencies = ~S, bodies = ~S"
				module name target-package dependencies bodies))))

(define-condition enrich-with-bad-name-error (error)
	(
		(module
			:initarg :module
			:initform (error "no :module for enrich-with-bad-name-error")
			:reader enrich-with-bad-name-error-module)
		(name
			:initarg :name
			:initform (error "no :name for enrich-with-bad-name-error")
			:reader enrich-with-bad-name-error-name)
	)
	(:report print-object))

(defmethod print-object ((conditio enrich-with-bad-name-error) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (module name) conditio
			(format out "module = ~S, name = ~S" module name))))

(define-condition illegal-target-package-for-enrich-module-with-header (error)
	(
		(target-package
			:initarg :target-package
			:initform (error "no :target-package for illegal-target-package-for-enrich-module-with-header")
			:reader illegal-target-package-for-enrich-module-with-header-target-package)
		(module
			:initarg :module
			:initform (error "no :module for illegal-target-package-for-enrich-module-with-header")
			:reader illegal-target-package-for-enrich-module-with-header-module)
	)
	(:report print-object))

(defmethod print-object ((conditio illegal-target-package-for-enrich-module-with-header) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (target-package module) conditio
			(format out "target-package = ~S, module = ~S" target-package module))))

(define-condition illegal-dir-for-enrich-module-with-header (error)
	(
		(dir
			:initarg :dir
			:initform (error "no :dir for illegal-dir-for-enrich-module-with-header")
			:reader illegal-dir-for-enrich-module-with-header-dir)
		(module
			:initarg :module
			:initform (error "no :module for illegal-dir-for-enrich-module-with-header")
			:reader illegal-dir-for-enrich-module-with-header-module)
	)
	(:report print-object))

(defmethod print-object ((conditio illegal-dir-for-enrich-module-with-header) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (dir module) conditio
			(format out "dir = ~S, module = ~S" dir module))))

(define-condition illegal-body-for-enrich-module-with-header (error)
	(
		(body
			:initarg :body
			:initform (error "no :body for illegal-body-for-enrich-module-with-header")
			:reader illegal-body-for-enrich-module-with-header-body)
		(module
			:initarg :module
			:initform (error "no :module for illegal-body-for-enrich-module-with-header")
			:reader illegal-body-for-enrich-module-with-header-module)
	)
	(:report print-object))

(defmethod print-object ((conditio illegal-body-for-enrich-module-with-header) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (body module) conditio
			(format out "body = ~S, module = ~S" body module))))

(define-condition illegal-arg-for-create-dir-module (error)
	(
		(name
			:initarg :name
			:initform (error "no :name for illegal-arg-for-create-dir-module")
			:reader illegal-arg-for-create-dir-module-name)
		(header
			:initarg :header
			:initform (error "no :header for illegal-arg-for-create-dir-module")
			:reader illegal-arg-for-create-dir-module-header)
		(dir
			:initarg :dir
			:initform (error "no :dir for illegal-arg-for-create-dir-module")
			:reader illegal-arg-for-create-dir-module-dir)
	)
	(:report print-object))

(defmethod print-object ((conditio illegal-arg-for-create-dir-module) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (name header dir) conditio
			(format out "name = ~S, header = ~S, dir = ~S" name header dir))))

(define-condition illegal-name-for-create-dir-module (illegal-arg-for-create-dir-module) ())

(define-condition illegal-header-for-create-dir-module (illegal-arg-for-create-dir-module) ())

(define-condition illegal-dir-for-create-dir-module (illegal-arg-for-create-dir-module) ())

(define-condition illegal-header-for-dir-module-from-header (error)
	(
		(header
			:initarg :header
			:initform (error "no :header for illegal-header-for-dir-module-from-header")
			:reader illegal-header-for-dir-module-from-header-header)
		(factory
			:initarg :factory
			:initform (error "no :factory for illegal-header-for-dir-module-from-header")
			:reader illegal-header-for-dir-module-from-header-factory)
	)
	(:report print-object))

(defmethod print-object ((conditio illegal-header-for-dir-module-from-header) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (header factory) conditio
			(format out "header = ~S, factory = ~S" header factory))))

(define-condition no-module-returned-from-create-dir-module (error)
	(
		(creator
			:initarg :creator
			:initform (error "no :creator for no-module-returned-from-create-dir-module")
			:reader no-module-returned-from-create-dir-module-creator)
		(name
			:initarg :name
			:initform (error "no :name for no-module-returned-from-create-dir-module")
			:reader no-module-returned-from-create-dir-module-name)
		(header
			:initarg :header
			:initform (error "no :header for no-module-returned-from-create-dir-module")
			:reader no-module-returned-from-create-dir-module-header)
		(dir
			:initarg :dir
			:initform (error "no :dir for no-module-returned-from-create-dir-module")
			:reader no-module-returned-from-create-dir-module-dir)
	)
	(:report print-object))

(defmethod print-object ((conditio no-module-returned-from-create-dir-module) out)
	(print-unreadable-object (conditio out :type t)
		(with-slots (creator name header dir) conditio
			(format out "creator = ~S, name = ~S, header = ~S, dir = ~S" creator name header dir))))

; module-error-handler
(defclass module-error-handler () ())

(defgeneric on-module-header-error (handler script conditio)
	(:method (handler script conditio)
		(error 'fallback-module-header-error :script script :cause conditio)))

(defgeneric on-module-missing (handler modname parent))

(defgeneric on-module-load-error (handler module body-script conditio))

(defgeneric on-module-shadow (handler old-module new-module))

(defgeneric on-module-dependency-cycle (handler module))

(defgeneric on-module-did-not-load (handler module))

(defvar *module-error-handler* nil)

; callback-module-error-handler
(defclass callback-module-error-handler (module-error-handler delegator) (
	(on-module-header-error
		:initarg :on-module-header-error
		:initform nil
		:accessor callback-module-error-handler-on-module-header-error)
	(on-module-missing
		:initarg :on-module-missing
		:initform nil
		:accessor callback-module-error-handler-on-module-missing)
	(on-module-load-error
		:initarg :on-module-load-error
		:initform nil
		:accessor callback-module-error-handler-on-module-load-error)
	(on-module-shadow
		:initarg :on-module-shadow
		:initform nil
		:accessor callback-module-error-handler-on-module-shadow)
	(on-module-dependency-cycle
		:initarg :on-module-dependency-cycle
		:initform nil
		:accessor callback-module-error-handler-on-module-dependency-cycle)
	(on-module-did-not-load
		:initarg :on-module-did-not-load
		:initform nil
		:accessor callback-module-error-handler-on-module-did-not-load)))

(defmethod on-module-header-error ((handler callback-module-error-handler) script conditio)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-header-error) handler
			(if on-module-header-error
				(funcall on-module-header-error delegate-or (lambda nil (call-next-method)) script conditio)
				(on-module-header-error delegate script conditio)))))

(defmethod on-module-missing ((handler callback-module-error-handler) modname parent)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-missing) handler
			(if on-module-missing
				(funcall on-module-missing delegate-or (lambda nil (call-next-method)) modname parent)
				(on-module-missing delegate modname parent)))))

(defmethod on-module-load-error ((handler callback-module-error-handler) module body-script conditio)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-load-error) handler
			(if on-module-load-error
				(funcall on-module-load-error delegate-or (lambda nil (call-next-method)) module body-script conditio)
				(on-module-load-error delegate module body-script conditio)))))

(defmethod on-module-shadow ((handler callback-module-error-handler) old-module new-module)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-shadow) handler
			(if on-module-shadow
				(funcall on-module-shadow delegate-or (lambda nil (call-next-method)) old-module new-module)
				(on-module-shadow delegate old-module new-module)))))

(defmethod on-module-dependency-cycle ((handler callback-module-error-handler) module)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-dependency-cycle) handler
			(if on-module-dependency-cycle
				(funcall on-module-dependency-cycle delegate-or (lambda nil (call-next-method)) module)
				(on-module-dependency-cycle delegate module)))))

(defmethod on-module-did-not-load ((handler callback-module-error-handler) module)
	(let ((delegate-or (delegator-delegate handler)))
		(with-slots (delegate on-module-did-not-load) handler
			(if on-module-did-not-load
				(funcall on-module-did-not-load delegate-or (lambda nil (call-next-method)) module)
				(on-module-did-not-load delegate module)))))

(defun intercept-module-error-handler
	(&key
		header-error
		missing
		load-error
		shadow
		dependency-cycle
		did-not-load
		prefer-this-over-delegate)
	(setf *module-error-handler* (make-instance 'callback-module-error-handler
		:delegate *module-error-handler*
		:prefer-this-over-delegate prefer-this-over-delegate
		:on-module-header-error header-error
		:on-module-missing missing
		:on-module-load-error load-error
		:on-module-shadow shadow
		:on-module-dependency-cycle dependency-cycle
		:on-module-did-not-load did-not-load)))

; module
(defclass module () ())

(defgeneric module-name (module)
	(:method (module)
		(error 'module-name-missing-error :module module)))

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
	(error 'module-missing-error :name modname :parent parent))

(defmethod on-module-load-error (handler module body-script conditio)
	(error 'module-load-error :module module :body body-script :cause conditio))

(defmethod on-module-shadow (handler old-module new-module)
	(warn 'module-shadow-warning :old-module old-module :new-module new-module))

(defmethod on-module-did-not-load (handler module)
	(warn 'module-did-not-load-warning :module module))

; callback-module
(defclass callback-module (module delegator) (
	(module-name :initarg :module-name :initform nil :accessor callback-module-module-name)
	(module-definition-location
		:initarg :module-definition-location
		:initform nil
		:accessor callback-module-module-definition-location)
	(module-dependencies :initarg :module-dependencies :initform nil :accessor callback-module-module-dependencies)
	(module-should-load :initarg :module-should-load :initform nil :accessor callback-module-module-should-load)
	(module-load :initarg :module-load :initform nil :accessor callback-module-module-load)))

(defmethod module-name ((module callback-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-name) module
			(if module-name
				(funcall module-name delegate-or (lambda nil (call-next-method)))
				(module-name delegate)))))

(defmethod module-definition-location ((module callback-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-definition-location) module
			(if module-definition-location
				(funcall module-definition-location delegate-or (lambda nil (call-next-method)))
				(module-definition-location delegate)))))

(defmethod module-dependencies ((module callback-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-dependencies) module
			(if module-dependencies
				(funcall module-dependencies delegate-or (lambda nil (call-next-method)))
				(module-dependencies delegate)))))

(defmethod module-should-load ((module callback-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-should-load) module
			(if module-should-load
				(funcall module-should-load delegate-or (lambda nil (call-next-method)))
				(module-should-load delegate)))))

(defmethod module-load ((module callback-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-load) module
			(cond
				(module-load (funcall module-load delegate-or))
				(delegate (module-load delegate))
				(t (error 'missing-delegate-error :abstraction 'module :wrapper module))))))

(defun wrap-module
	(delegate &key
		prefer-this-over-delegate
		name
		definition-location
		dependencies
		should-load
		load)
	(make-instance 'callback-module
		:delegate delegate
		:prefer-this-over-delegate prefer-this-over-delegate
		:module-name name
		:module-definition-location definition-location
		:module-dependencies dependencies
		:module-should-load should-load
		:module-load load))

; module-filter
(defclass module-filter () ())

(defgeneric filter-module (filter module)
	(:method (filter module) module))

(defvar *register-module-filter* nil)
(defvar *found-module-header-filter* nil)

; callback-module-filter
(defclass callback-module-filter (module-filter delegator) (
	(filter-module :initarg :filter-module :initform nil :accessor callback-module-filter-filter-module)))

(defmethod filter-module ((filter callback-module-filter) module)
	(let ((delegate-or (delegate-or-this filter)))
		(with-slots (delegate filter-module) filter
			(if filter-module
				(funcall filter-module delegate-or (lambda nil (call-next-method)) module)
				(filter-module delegate module)))))

(defun intercept-register-module-filter (&key filter-module prefer-this-over-delegate)
	(setf *register-module-filter* (make-instance 'callback-module-filter
		:delegate *register-module-filter*
		:prefer-this-over-delegate prefer-this-over-delegate
		:filter-module filter-module)))

(defun intercept-found-module-header-filter (&key filter-module prefer-this-over-delegate)
	(setf *found-module-header-filter* (make-instance 'callback-module-filter
		:delegate *found-module-header-filter*
		:prefer-this-over-delegate prefer-this-over-delegate
		:filter-module filter-module)))

; registry
(defvar *module-registry* (make-hash-table :test #'equal))

(defun register-module (module)
	(if (not (typep module 'module))
		(error 'registering-non-module-error :module module))
	(let ((filtered (filter-module *register-module-filter* module)))
		(if filtered
			(setf module filtered)
			(return-from register-module)))
	(let ((name (module-name module)))
		(if (not (stringp name))
			(error 'module-with-non-string-name-error :module module :name name))
		(let ((old-module (gethash name *module-registry*)))
			(if old-module
				(on-module-shadow *module-error-handler* old-module module)
				(setf (gethash name *module-registry*) module)))))

(defun find-module (modname)
	(gethash (if (symbolp modname) (string-downcase (symbol-name modname)) modname) *module-registry*))

(defparameter *modules-currently-loading* nil)
(defparameter *currently-loading-module* nil)

(defmethod on-module-dependency-cycle (handler module)
	(error 'module-dependency-cycle-error :module module :cycle *modules-currently-loading*))

(defun try-load-module (module impl)
	(restart-case
		(progn
			(if (not (typep module 'module))
				(error 'trying-to-load-non-module-error :module module))
			(if (member module *modules-currently-loading* :test #'eq)
				(progn
					(on-module-dependency-cycle *module-error-handler* module)
					nil)
				(let ((*modules-currently-loading* (cons module *modules-currently-loading*)))
					(funcall impl module))))
		(skip-module (&optional result) :report "Skip this module" result)
		(use-new-module (new-module)
			:report "Enter expression for replacement module object"
			:interactive (lambda nil (prompt-new-value "Form returning replacement module"))
			(try-load-module new-module impl))))

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
	(target-package :initarg :target-package :initform nil :accessor module-base-target-package)
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
						(error 'trying-to-add-nameless-dependency-error :parent module :child dependency))
					(setf dependency name)))
			(t (error 'trying-to-add-non-module-dependency-error :parent module :child dependency)))
		(pushnew dependency (module-base-dependencies module) :test #'equal)))

(defgeneric real-module-load (module))

(defmethod module-load ((module module-base))
	(cond
		((slot-value module 'is-loaded) t)
		((try-load-module module #'real-module-load) (setf (slot-value module 'is-loaded) t))))

; header-enrichable-module
(defclass header-enrichable-module (module) ())

(defgeneric enrich-module-with-header (module name target-package dependencies bodies))

(defmethod enrich-module-with-header ((module header-enrichable-module) name target-package dependencies bodies)
	(error 'trying-to-enrich-non-enrichable-module-with-header-error
		:module module
		:name name
		:target-package target-package
		:dependencies dependencies
		:bodies bodies))

(defmacro defmodule (name &key target-package depends-on scripts)
	`(enrich-module-with-header *currently-loading-module* ',name ',target-package ',depends-on ',scripts))

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

(defun dir-module-load-single-script (module script)
	(if script
		(handler-case
			(let
				(
					(*currently-loading-module* module)
					(*current-script-semantics* :module-body)
					(*package* (or
						(let ((target-package (module-base-target-package module)))
							(if target-package
								(find-package target-package)))
						*module-body-target-package*
						(find-package '#:common-lisp-user)))
				)
				(load script)
				:continue)
			(error (conditio)
				(restart-case
					(progn
						(on-module-load-error *module-error-handler* module script conditio)
						(return-from dir-module-load-single-script :return-nil))
					(skip-script () :report "Skip this script" :continue)
					(skip-remaining-scripts () :report "Skip this script and the rest in this module" :return-t)
					(use-new-path (new-path)
						:report "Enter path for replacement script"
						:interactive (lambda nil (prompt-new-value "Form returning replacement script"))
						(dir-module-load-single-script module new-path)))))
		:continue))

(defmethod dir-module-load-scripts ((module dir-module))
	(loop for script in (dir-module-scripts module) do
		(let ((action (dir-module-load-single-script module script)))
			(cond
				((eq action :continue) nil)
				((eq action :return-nil) (return-from dir-module-load-scripts))
				((eq action :return-t) (return-from dir-module-load-scripts t))
				(t (error (format nil "Unrecognized result from dir-module-load-single-script: ~S" action))))))
	t)

(defmethod enrich-module-with-header ((module dir-module) name target-package dependencies bodies)
	(if name
		(setf (slot-value module 'name) (cond
			((stringp name) name)
			((symbolp name) (string-downcase (symbol-name name)))
			(t (error 'enrich-with-bad-name-error :module module :name name)))))
	(if target-package
		(setf (slot-value module 'target-package) (cond
			((stringp target-package) (make-symbol (string-upcase target-package)))
			((symbolp target-package) target-package)
			(t (error 'illegal-target-package-for-enrich-module-with-header
				:target-package target-package
				:module module)))))
	(loop for dep in dependencies do
		(if dep (module-base-add-dependency module dep)))
	(let ((dir (dir-module-dir module)))
		(if (not (stringp dir))
			(error 'illegal-dir-for-enrich-module-with-header :dir dir :module module))
		(setf (dir-module-scripts module)
			(loop for body in bodies
				when body
				collect (format nil "~A~A~A.lisp" dir *file-separator-char* (cond
					((stringp body) body)
					((pathnamep body) (uiop:native-namestring body))
					((symbolp body) (string-downcase (symbol-name body)))
					(t (error 'illegal-body-for-enrich-module-with-header :body body :module module)))))))
	module)

(defmethod print-object ((module dir-module) out)
	(print-unreadable-object (module out :type t)
		(with-slots (name is-loaded target-package dependencies should-load header dir)
			module
			(format out
				"~A, is-loaded = ~S, target-package = ~S, dependencies = ~S, should-load = ~S, header = '~A', dir = '~A'"
				name is-loaded target-package dependencies should-load header dir))))

; callback-dir-module
(defclass callback-dir-module (dir-module delegator) (
	(module-name
		:initarg :module-name
		:initform nil
		:accessor callback-dir-module-module-name)
	(module-definition-location
		:initarg :module-definition-location
		:initform nil
		:accessor callback-dir-module-module-definition-location)
	(module-dependencies
		:initarg :module-dependencies
		:initform nil
		:accessor callback-dir-module-module-dependencies)
	(module-should-load
		:initarg :module-should-load
		:initform nil
		:accessor callback-dir-module-module-should-load)
	(module-load
		:initarg :module-load
		:initform nil
		:accessor callback-dir-module-module-load)
	(module-base-add-dependency
		:initarg :module-base-add-dependency
		:initform nil
		:accessor callback-dir-module-module-base-add-dependency)
	(real-module-load
		:initarg :real-module-load
		:initform nil
		:accessor callback-dir-module-real-module-load)
	(enrich-module-with-header
		:initarg :enrich-module-with-header
		:initform nil
		:accessor callback-dir-module-enrich-module-with-header)
	(dir-module-load-scripts
		:initarg :dir-module-load-scripts
		:initform nil
		:accessor callback-dir-module-dir-module-load-scripts)))

(defmethod module-name ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-name) module
			(cond
				(module-name (funcall module-name delegate-or (lambda nil (call-next-method))))
				(delegate (module-name delegate))
				(t (call-next-method))))))

(defmethod module-definition-location ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-definition-location) module
			(cond
				(module-definition-location
					(funcall module-definition-location delegate-or (lambda nil (call-next-method))))
				(delegate (module-definition-location delegate))
				(t (call-next-method))))))

(defmethod module-dependencies ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-dependencies) module
			(cond
				(module-dependencies (funcall module-dependencies delegate-or (lambda nil (call-next-method))))
				(delegate (module-dependencies delegate))
				(t (call-next-method))))))

(defmethod module-should-load ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-should-load) module
			(cond
				(module-should-load (funcall module-should-load delegate-or (lambda nil (call-next-method))))
				(delegate (module-should-load delegate))
				(t (call-next-method))))))

(defmethod module-load ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-load) module
			(cond
				(module-load (funcall module-load delegate-or (lambda nil (call-next-method))))
				(delegate (module-load delegate))
				(t (call-next-method))))))

(defmethod module-base-add-dependency ((module callback-dir-module) dependency)
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate module-base-add-dependency) module
			(cond
				(module-base-add-dependency
					(funcall module-base-add-dependency delegate-or (lambda nil (call-next-method)) dependency))
				(delegate (module-base-add-dependency delegate dependency))
				(t (call-next-method))))))

(defmethod real-module-load ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate real-module-load) module
			(cond
				(real-module-load (funcall real-module-load delegate-or (lambda nil (call-next-method))))
				(delegate (real-module-load delegate))
				(t (call-next-method))))))

(defmethod enrich-module-with-header ((module callback-dir-module) name target-package dependencies bodies)
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate enrich-module-with-header) module
			(cond
				(enrich-module-with-header
					(funcall enrich-module-with-header
						delegate-or (lambda nil (call-next-method)) name target-package dependencies bodies))
				(delegate (enrich-module-with-header delegate name target-package dependencies bodies))
				(t (call-next-method))))))

(defmethod dir-module-load-scripts ((module callback-dir-module))
	(let ((delegate-or (delegate-or-this module)))
		(with-slots (delegate dir-module-load-scripts) module
			(cond
				(dir-module-load-scripts (funcall dir-module-load-scripts delegate-or (lambda nil (call-next-method))))
				(delegate (dir-module-load-scripts delegate))
				(t (call-next-method))))))

; dir-module-creator
(defclass dir-module-creator () ())

(defgeneric create-dir-module (creator name header dir))

(defmethod create-dir-module (creator name header dir)
	(make-instance 'dir-module
		:name (cond
			((stringp name) name)
			((symbolp name) (string-downcase (symbol-name name)))
			(t (error 'illegal-name-for-create-dir-module :name name :header header :dir dir)))
		:header (cond
			((stringp header) header)
			((pathnamep header) (uiop:native-namestring header))
			(t (error 'illegal-header-for-create-dir-module :name name :header header :dir dir)))
		:dir (cond
			((null dir) (uiop:native-namestring (uiop:pathname-directory-pathname
				(if (stringp header) (pathname header) header))))
			((stringp dir) dir)
			((pathnamep dir) (uiop:native-namestring dir))
			(t (error 'illegal-dir-for-create-dir-module :name name :header header :dir dir)))))

(defvar *dir-module-creator* nil)

; callback-dir-module-creator
(defclass callback-dir-module-creator (dir-module-creator delegator) (
	(create-dir-module
		:initarg :create-dir-module
		:initform nil
		:accessor callback-dir-module-creator-create-dir-module)))

(defmethod create-dir-module ((creator callback-dir-module-creator) name header dir)
	(let ((delegate-or (delegate-or-this creator)))
		(with-slots (delegate create-dir-module) creator
			(if create-dir-module
				(funcall create-dir-module delegate-or (lambda nil (call-next-method)) name header dir)
				(create-dir-module delegate name header dir)))))

(defun intercept-dir-module-creator (&key create-dir-module prefer-this-over-delegate)
	(setf *dir-module-creator* (make-instance 'create-dir-module
		:delegate *dir-module-creator*
		:prefer-this-over-delegate prefer-this-over-delegate
		:create-dir-module create-dir-module)))

; dir-module-factory
(defclass dir-module-factory () ())

(defgeneric dir-module-from-header (factory header-path))

(defun load-module-header (factory module full-header native-header)
	(handler-case
		(let ((*currently-loading-module* module) (*current-script-semantics* :module-header))
			(load full-header)
			module)
		(error (conditio)
			(restart-case
				(on-module-header-error *module-error-handler* native-header
					(make-condition 'module-header-error :module module :cause conditio))
				(skip-header () :report "Skip this header script" module)
				(skip-module () :report "Skip this module" nil)
				(use-new-path (new-path)
					:report "Enter path for replacement header script"
					:interactive (lambda nil (prompt-new-value "Form returning replacement header script"))
					(cond
						((pathnamep new-path) nil)
						((stringp new-path)
							(setf new-path (pathname new-path)))
						(t (error 'illegal-header-for-dir-module-from-header :header new-path :factory factory)))
					(setf full-header (uiop:ensure-absolute-pathname new-path (uiop:getcwd)))
					(setf native-header (uiop:native-namestring full-header))
					(load-module-header factory module full-header native-header))))))

(defmethod dir-module-from-header (factory header-path)
	(let*
		(
			(header-pathname (cond
				((stringp header-path) (pathname header-path))
				((pathnamep header-path) header-path)
				(t (error 'illegal-header-for-dir-module-from-header :header header-path :factory factory))))
			(full-header (uiop:ensure-absolute-pathname header-pathname (uiop:getcwd)))
			(full-dir (uiop:pathname-directory-pathname full-header))
			(native-header (uiop:native-namestring full-header))
			(native-dir (uiop:native-namestring full-dir))
			(last-dir-component (first (last (pathname-directory full-header))))
			(name (if (stringp last-dir-component) last-dir-component "unnamed-module"))
			(module (create-dir-module *dir-module-creator* name native-header native-dir))
		)
		(unless module
			(error 'no-module-returned-from-create-dir-module
				:creator factory
				:name name
				:header native-header
				:dir native-dir))
		(load-module-header factory module full-header native-header)))

(defvar *dir-module-factory* nil)

; callback-dir-module-factory
(defclass callback-dir-module-factory (dir-module-factory delegator) (
	(dir-module-from-header
		:initarg :dir-module-from-header
		:initform nil
		:accessor callback-dir-module-factory-dir-module-from-header)))

(defmethod dir-module-from-header ((factory callback-dir-module-factory) header-path)
	(let ((delegate-or (delegate-or-this factory)))
		(with-slots (delegate dir-module-from-header) factory
			(if dir-module-from-header
				(funcall dir-module-from-header delegate-or (lambda nil (call-next-method)) header-path)
				(dir-module-from-header delegate header-path)))))

(defun intercept-dir-module-factory (&key dir-module-from-header prefer-this-over-delegate)
	(setf *dir-module-factory* (make-instance 'callback-dir-module-factory
		:delegate *dir-module-factory*
		:prefer-this-over-delegate prefer-this-over-delegate
		:dir-module-from-header dir-module-from-header)))

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
			(setf module (filter-module *found-module-header-filter* module)))
		(if module
			(register-module module))))

(defvar *module-scanner* nil)

; callback-module-scanner
(defclass callback-module-scanner (module-scanner delegator) (
	(scan-for-modules :initarg :scan-for-modules :initform nil :accessor callback-module-scanner-scan-for-modules)
	(get-modules-dirs :initarg :get-modules-dirs :initform nil :accessor callback-module-scanner-get-modules-dirs)
	(found-module-header
		:initarg :found-module-header
		:initform nil
		:accessor callback-module-scanner-found-module-header)))

(defmethod scan-for-modules ((scanner callback-module-scanner))
	(let ((delegate-or (delegate-or-this scanner)))
		(with-slots (delegate scan-for-modules) scanner
			(if scan-for-modules
				(funcall scan-for-modules delegate-or (lambda nil (call-next-method)))
				(scan-for-modules delegate)))))

(defmethod get-modules-dirs ((scanner callback-module-scanner))
	(let ((delegate-or (delegate-or-this scanner)))
		(with-slots (delegate get-modules-dirs) scanner
			(if get-modules-dirs
				(funcall get-modules-dirs delegate-or (lambda nil (call-next-method)))
				(get-modules-dirs delegate)))))

(defmethod found-module-header ((scanner callback-module-scanner) header)
	(let ((delegate-or (delegate-or-this scanner)))
		(with-slots (delegate found-module-header) scanner
			(if found-module-header
				(funcall found-module-header delegate-or (lambda nil (call-next-method)) header)
				(found-module-header delegate header)))))

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

(let ((*package* (or *module-body-target-package* (find-package '#:usdo-sloth-user))))
	(loop for script in (merge-scripts-default *init-system-scripts* *init-system-default-script*) do
		(load-script script t :init-system))
	(loop for script in (reverse *init-system-scripts-aux*) do
		(load-script script t :init-system-aux))
	(loop for script in (merge-scripts-default *init-user-scripts* *init-user-default-script*) do
		(load-script script t :init-user))
	(loop for script in (reverse *init-user-scripts-aux*) do
		(load-script script t :init-user-aux)))

; Kickoff!
(in-package #:usdo-sloth-user)

(unless (find-package '#:usdo-sloth-core)
	(error "Module 'core' is missing"))

(usdo-sloth-core:go-baby-go)
