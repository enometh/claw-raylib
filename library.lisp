(defpackage claw-raylib.library
  (:use #:cl #:alexandria))

(in-package #:claw-raylib.library)

#+asdf
(pushnew
 (namestring
  (merge-pathnames #P"lib/"
		   (asdf:component-pathname (asdf:find-system '#:claw-raylib))))
 cffi:*foreign-library-directories* :test
 ;; enometh/cffi extends the type of elements that can be found in
 ;; cffi:*foreign-library-directories*
 #'(lambda (x y)
     (and (stringp x) (string y) (string= x y))))


(eval-when (load eval compile)
(defvar *dll-path*
  (namestring
   (merge-pathnames #P"fasl/lib/"
		    cl-user::*claw-raylib-dir*))))


(cffi:define-foreign-library libraylib
  (:unix "libraylib.so" :search-path #.cl-user::*claw-raylib-lib*)
  (t (:default "libraylib")))

(cffi:use-foreign-library libraylib)

(cffi:define-foreign-library libraylib-adapter
  (:unix "libraylib-adapter.x86_64-pc-linux-gnu.so" :search-path #.*dll-path*)
  (t (:default "libraylib-adapter")))

(cffi:use-foreign-library libraylib-adapter)

(pushnew :raylib *features*)

(cffi:define-foreign-library librlgl-adapter
  (:unix "librlgl-adapter.x86_64-pc-linux-gnu.so" :search-path #.*dll-path*)
  (t (:default "librlgl-adapter")))

(cffi:use-foreign-library librlgl-adapter)

(pushnew :rlgl *features*)

;; not libraygui.so but raygui.so
(cffi:define-foreign-library libraygui
  (:unix "raygui.so" :search-path #.cl-user::*claw-raylib-lib*)
  (t (:default "libraygui")))

(cffi:define-foreign-library libraygui-adapter
  (:unix "libraygui-adapter.x86_64-pc-linux-gnu.so" :search-path #.*dll-path*)
  (t (:default "libraygui-adapter")))

(handler-case
    (progn
      (cffi:use-foreign-library libraygui)
      (cffi:use-foreign-library libraygui-adapter)
      (push :raygui *features*))
  (cffi:load-foreign-library-error ()
    (delete-package '#:raygui)))


#||
mkdir /dev/shm/claw-cxx-raylib/fasl/lib

     (let ((arch "x86_64-pc-linux-gnu")
           (path (merge-pathnames #P"lib/" cl-user::*claw-raylib-dir*))
	   (cflags (format nil "-fPIC -O0 -g3 -I~A" cl-user::*claw-raylib-inc*))
	   (ldflags (format nil "-Wl,-O1 -Wl,--as-needed -L~A" cl-user::*claw-raylib-lib*)))
       (dolist (lib '("raylib" "rlgl" "raygui"))
	 (let* ((source (namestring (merge-pathnames (format nil "lib~A-adapter.~A.c" lib arch) path)))
		(destdir (namestring (merge-pathnames "fasl/lib/" cl-user::*claw-raylib-dir*)))
		(obj (namestring (merge-pathnames (format nil "lib~A-adapter.~a.o" lib arch) destdir)))
		(dll (namestring (merge-pathnames (format nil "lib~A-adapter.~a.so" lib arch) destdir)))
		(cc-cmd `("gcc" ,@(user::string-split-map #(#\Space) cflags)
				"-c" "-o" ,obj ,source))
		(ld-cmd `("gcc" ,@(user::string-split-map #(#\Space) ldflags)
				"-shared" "-o" ,dll ,obj)))
	 (write  cc-cmd)
;;	 (uiop:run-program cc-cmd :error-output *error-output* :output *standard-output*)
	 (terpri)
	 (write ld-cmd)
;;	 (uiop:run-program ld-cmd :error-output *error-output* :output *standard-output*)
	 (terpri))))
||#
