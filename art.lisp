(in-package :cl-user)
(defpackage :art
  (:use :cl))

(in-package :art)

(defparameter *debug* t)
(defparameter *beta* 1.0)
(defparameter *vigilance* 0.9)

(defparameter *max-clusters* 5)
(defstruct (cluster
             (:print-object (lambda (cluster stream)
                              (format stream "[Cluster ~a : ~a : customers ~a]"
                                      (cluster-index cluster)
                                      (print-data (cluster-prototype cluster) nil)
                                      (cluster-customers cluster)))))
  index prototype customers sum)

(defparameter *clusters* nil)
(defparameter *items* '(hammer paper snickers screwdriver
                        pen kit-kat wrench pencil
                        heath-bar tape-measure binder))

(defun print-data (data &optional (stream t))
  (format stream "~(~a ~)" (loop
                              for i from 0
                              for item in data
                              unless (zerop item)
                              collect (nth i *items*))))

(defparameter *database*
  '((0   0   0   0   0   1   0   0   1   0   0)
    (0   1   0   0   0   0   0   1   0   0   1)
    (0   0   0   1   0   0   1   0   0   1   0)
    (0   0   0   0   1   0   0   1   0   0   1)
    (1   0   0   1   0   0   0   0   0   1   0)
    (0   0   0   0   1   0   0   0   0   0   1)
    (1   0   0   1   0   0   0   0   0   0   0)
    (0   0   1   0   0   0   0   0   1   0   0)
    (0   0   0   0   1   0   0   1   0   0   0)
    (0   0   1   0   0   1   0   0   1   0   0)))

(defparameter *customers* nil)
(defstruct (customer
             (:print-object (lambda (customer stream)
                              (format stream "[Customer ~a : ~a]"
                                      (customer-index customer)
                                      (print-data (customer-data customer) nil)))))
  index cluster)

(defun customer-data (customer)
  (nth (customer-index customer) *database*))

(defun main ()
  (initialize)
  (perform-ART1)
  (println "---------")
  (mapcar #'println (reverse *clusters*))
  (println "---------")
  (mapcan #'make-recommendation *customers*))


(defun initialize ()
  (setf *customers* (loop for i from 0  below (length *database*)
                       collect (make-customer :index i)))
  (setf *clusters* nil))

(defun magnitude (vector)
  (reduce #'+ vector))

(defun println (object)
  (princ object)
  (terpri))

(defun perform-ART1 ()
  (loop
     for i below 50
     with done = nil
     unless done
     do
       (setf done t)
       (loop for customer in *customers*
          for data = (customer-data customer)
          do
            (when *debug*
              (println customer))
            (loop
               for cluster in *clusters*
               for prototype = (cluster-prototype cluster)
               for and-result = (mapcar #'* prototype data)
               for mag-PE = (magnitude and-result)
               for mag-P = (magnitude prototype)
               for mag-E = (magnitude data)
               for result = (/ mag-PE (+ *beta* mag-P))
               for test = (/ mag-E (+ *beta* (length *items*)))
               do
                 (when *debug*
                   (format t "Test ~a~%" cluster)
                   (format t "step 3: ~a > ~a ?~%" result test))
                 (when (> result test)
                   (when *debug*
                     (format t "step 4: testing viligance ~a < ~a~%" (/ mag-PE mag-E) *vigilance*))
                   (when (< (/ mag-PE mag-E *vigilance*))
                     (let ((current (customer-cluster customer)))
                       (unless (equal current cluster)
                         (setf done nil)
                         (when *debug*
                           (format t "Moved example ~a from cluster ~a to ~a~%"
                                   (print-data data nil)
                                   current
                                   cluster))
                         (setf (customer-cluster customer) cluster)
                         (push customer (cluster-customers cluster))
                         (when current
                           (remove-customer current customer)
                           (update-cluster current))
                         (update-cluster cluster))))))
            (unless (customer-cluster customer)
              (setf (customer-cluster customer)
                    (create-new-cluster customer))
              (setf done nil)))))

(defun create-new-cluster (customer)
  "Return index of the newly created cluster"
  (when (>=  (length *clusters*) *max-clusters*)
    (error "Too many prototypes"))
  (let ((cluster (make-cluster :index (length *clusters*)
                               :prototype (copy-list (customer-data customer))
                               :customers (list customer))))
    (push cluster *clusters*)
    (when *debug*
      (println "Creating new cluster")
      (println cluster))
    cluster))

(defun remove-customer (cluster customer)
  (setf (cluster-customers cluster) (remove customer (cluster-customers cluster)))
  (if (zerop (length (cluster-customers cluster)))
      (setf *customers* (remove cluster *clusters*))
      (update-cluster cluster)))

(defun update-cluster (cluster)
  (setf (cluster-prototype cluster) nil)
  (loop
     for customer in (cluster-customers cluster)
     for data = (customer-data customer)
     for prototype = (cluster-prototype cluster)
     do
       (setf (cluster-prototype cluster)
             (if prototype
                 (mapcar #'* prototype data)
                 data))
       (setf (cluster-sum cluster)
             (if prototype
                 (mapcar #'+ prototype data)
                 data))))

(defun make-recommendation (customer)
  (loop
     with best = nil
     with best-score = 0
     for item in *items*
     for has-item in (customer-data customer)
     for score in (cluster-sum (customer-cluster customer))
     do (when (and (zerop has-item)
                   (> score best-score))
          (setf best-score score
                best item))
     finally (format t "Recomendation for ~a: ~a ~a~%" customer best-score best)))
