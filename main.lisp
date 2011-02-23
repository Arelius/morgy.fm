(require 'drakma)
(require 'xmls)

(defpackage :morgy.fm (:use :cl))
(in-package :morgy.fm)

(defparameter *root-url* "http://ws.audioscrobbler.com/2.0/")
(defparameter *api-key* )
(defparameter *secret-key* )

(defun mappair (fun list)
  (cons
   (funcall fun (car list) (cadr list))
   (if (cddr list) 
    (mappair fun (cddr list))
    '())))

(defun last-fm-call-function (fun &rest params)
  (let ((ret
         (xmls:parse
          (drakma:http-request
           *root-url*
           :parameters
           `(("method" . ,(string-downcase (string fun)))
             ("api_key" . ,*api-key*)
             ,@(mappair
                (lambda (param value)
                  (cons
                   (string-downcase
                    (symbol-name param))
                   (string value)))
                params)))
          :compress-whitespace t)))
    (if (equal (car (cdaadr ret)) "ok")
     (caddr ret)
     nil)))

(defun similar-artists (artist)
    (mapcar
     (lambda (x)
       (caddar (remove-if-not (lambda (e)
                         (and (listp e)
                              (equal (car e) "name")))
                       x)))
     (cddr (last-fm-call-function 'artist.getsimilar :artist artist))))

(defun similar-artist-union (&rest artists)
 (let* ((items (apply
                #'concatenate
                (cons 'list
                      (mapcar
                       (lambda (x)
                         (similar-artists x))
                       artists))))
        (unique (remove-duplicates items :test #'equal)))
   (sort
    (mapcar
     (lambda (x)
       (cons x
             (count x items :test #'equal)))
     unique)
    (lambda (x y)
      (> (cdr x) (cdr y))))))

(setf tmp
      (similar-artist-union 'MGMT
                            "The Naked and Famous"
                            "The XX"
                            "Phoenix"
                            "Matt Pond PA"
                            "Fischerspooner"
                            "Death Cab For Cutie"
                            "M83"
                            "Passion Pit"))

(defparameter tmp3
      (similar-artist-union "Broken Bells"
                            "Broken Social Scene"
                            "Massive Attack"
                            "Collide"
                            "In Strict Confidence"))

(similar-artists "In Strict Confidence")