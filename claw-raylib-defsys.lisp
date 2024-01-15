;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Wed Oct 23 18:25:53 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
(in-package "CL-USER")

;;; claw-raylib-defsys.lisp. define mk-defsystem system definitions
;;; for the generated bindings
;;;
;;; ;madhu 241023 split from claw.lisp

(defmacro claw-raylib-defsystem (name)
  (check-type name string)
  (setq name (string-downcase name))
  (let* ((system-name (concatenate 'string "claw-raylib." name))
	 (dll-system-name (concatenate 'string system-name ".library")))
    `(progn
       (mk:defsystem ,system-name
	 :source-pathname ,(pathname *claw-raylib-dir*)
	 :binary-pathname ,(let ((*binary-directory-fasl-root*
				  (merge-pathnames "fasl/" *claw-raylib-dir*)))
			     (binary-directory (concatenate 'string "")))
	 :source-extension "lisp"
	 :depends-on ("uiop" "cffi" ,dll-system-name)
	 :components ((:module ,name
		       :components
		       ((:file "x86_64-pc-linux-gnu"
			   :if-feature
			   (:and :x86-64 :linux))
			  #+nil
			  (:file "i686-pc-linux-gnu"
			   :if-feature (:and :x86 :linux))))))
       (mk:defsystem ,dll-system-name
	 :source-pathname ,(pathname *claw-raylib-dir*)
	 :binary-pathname ,(merge-pathnames "fasl/" *claw-raylib-dir*)
	 :source-extension "lisp"
	 :depends-on ("uiop" "cffi" "claw-cxx/util")
	 :components ((:module "lib"
		       :components
		       ((:file ,(concatenate 'string "lib" name "-adapter.x86_64-pc-linux-gnu")
			 :source-extension "c"
			 :compile-only t ;; handle loading via library.lisp
			 :if-feature (:and :x86-64 :linux)
			 :language :claw-cxx-adapter
			 :compiler-options
			 (:cflags ,(format nil "-O0 -g3 -I~A" *claw-raylib-inc*)
			  :ldflags ,(format nil "-Wl,-O1 -Wl,--as-needed -L~A" *claw-raylib-lib*)
			  )))))))))

(progn
(claw-raylib-defsystem "raylib")
(claw-raylib-defsystem "rlgl")
(claw-raylib-defsystem "raygui"))
