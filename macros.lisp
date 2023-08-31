(in-package #:claw-raylib)

(defmacro raylib::with-mode-3d (camera &body body)
  `(progn
     (raylib:begin-mode-3d ,camera)
     (unwind-protect (progn . ,body)
       (raylib:end-mode-3d))))

(export 'raylib::with-mode-3d :raylib)

(defmacro raylib::with-shader-mode (shader &body body)
  `(progn
     (raylib:begin-shader-mode ,shader)
     (unwind-protect (progn . ,body)
       (raylib:end-shader-mode))))

(export 'raylib::with-shader-mode :raylib)

(defmacro raylib::with-mode-2d (camera &body body)
  `(progn
     (raylib:begin-mode-2d ,camera)
     (unwind-protect (progn . ,body)
       (raylib:end-mode-2d))))

(export 'raylib::with-mode-2d :raylib)

(defmacro raylib::with-drawing (&body body)
  `(progn
     (raylib:begin-drawing)
     (unwind-protect (progn . ,body)
       (raylib:end-drawing))))

(export 'raylib::with-drawing :raylib)

(defmacro raylib::with-blend-mode (mode &body body)
  `(progn
     (raylib:begin-blend-mode ,mode)
     (unwind-protect (progn . ,body)
       (raylib:end-blend-mode))))

(export 'raylib::with-blend-mode :raylib)

(defmacro raylib::with-texture-mode (target &body body)
  `(progn
     (raylib:begin-texture-mode ,target)
     (unwind-protect (progn . ,body)
       (raylib:end-texture-mode))))

(export 'raylib::with-texture-mode :raylib)

(defmacro raylib::with-scissor-mode ((recx &optional (y nil xywhp) width height) &body body)
  `(progn
     ,(if xywhp
          `(raylib:begin-scissor-mode ,recx ,y ,width ,height)
          (with-gensyms (x y width height)
            `(with-foreign-slots (((,x raylib:x)
                                   (,y raylib:y)
                                   (,width raylib:width)
                                   (,height raylib:height))
                                  ,recx (:struct raylib:rectangle))
               (raylib:begin-scissor-mode
                (the (signed-byte 32) (truncate ,x))
                (the (signed-byte 32) (truncate ,y))
                (the (signed-byte 32) (truncate ,width))
                (the (signed-byte 32) (truncate ,height))))))
     (unwind-protect (progn . ,body)
       (raylib:end-scissor-mode))))

(export 'raylib::with-scissor-mode :raylib)

(defmacro raylib::with-window ((width height &optional (title (asdf:component-name (asdf:find-system '#:claw-raylib))) flags) &body body)
  `(progn
     (raylib:set-config-flags ,(eval `(foreign-bitfield-value 'raylib:config-flags ',flags)))
     (raylib:init-window ,width ,height ,title)
     (unwind-protect (progn . ,body)
       (raylib:close-window))))

(export 'raylib::with-window :raylib)

(defmacro raylib::with-audio-device (&body body)
  `(progn
     (raylib:init-audio-device)
     (unwind-protect (progn . ,body)
       (raylib:close-audio-device))))

(export 'raylib::with-audio-device :raylib)

(defmacro raylib::with-audio-stream ((stream sample-rate sample-size channels) &body body)
  `(with-foreign-object (,stream '(:struct raylib:audio-stream))
     (raylib:load-audio-stream ,stream ,sample-rate ,sample-size ,channels)
     (unwind-protect (progn . ,body)
       (raylib:unload-audio-stream ,stream))))

(export 'raylib::with-audio-stream :raylib)
