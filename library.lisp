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
