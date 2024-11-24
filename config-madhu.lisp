(in-package "CL-USER")

;; local configuration. used in both claw.lisp (claw-raylib/gen to
;; generate bindings) and library.lisp (claw-raylib/library to load
;; bindings)


(defvar *claw-raylib-dir* "/dev/shm/claw-cxx-raylib/")
(defvar *claw-raylib-inc* "/opt/raylib-5.5_linux_amd64/include")
(defvar *claw-raylib-lib* "/opt/raylib-5.5_linux_amd64/lib/")
