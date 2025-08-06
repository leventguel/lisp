(load "/home/inline/quicklisp/setup.lisp")

(asdf:initialize-source-registry
 `(:source-registry
   (:tree "/home/inline/source/quicklisp/software/")
   (:tree "/home/inline/prg/lisp/lisp/")
   :default-registry
   :inherit-configuration))

(asdf:initialize-output-translations
 `(:output-translations
   #.(let ((wild-subdir
            (make-pathname :directory '(:relative :wild-inferiors)))
           (wild-file
            (make-pathname :name :wild :version :wild :type :wild)))
       `((:root ,wild-subdir ,wild-file)
         (:user-cache ,wild-subdir ,wild-file)))
   :inherit-configuration))

(asdf:load-system :mcclim)

(defpackage pack1
  (:use :clim-lisp))

(defclass a ()
  (slot1
   slot2))

(defpackage pack2
  (:use :clim-lisp))

(defclass b ()
  (slot1
   slot2))

(defpackage pack
  (:use :clim-lisp :pack1 :pack2))

(in-package :pack)


