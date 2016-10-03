(in-package :cl-user)
(defpackage :simulated-annealing
  (:use :cl))
(in-package :simulated-annealing)

(defclass solution ()
  ((queens :initarg :queens :accessor solution-queens)
   (energy :initarg :energy :initform 0 :accessor solution-energy)
   (size :initarg :size :accessor solution-size)))

(defparameter *default-size* 5)

(defun make-solution (&optional (size *default-size*))
  (loop
     with solution = (make-instance 'solution
                                    :size size
                                    :queens (loop for i from 0 below size collect i))
     repeat size
     do (tweak-solution solution)
     finally (return solution)))

(defun tweak-solution (solution)
  (loop
     with size = (solution-size solution)
     with queens = (solution-queens solution)
     with x = (random size)
     for y = (random size)
     while (= x y)
     finally (rotatef (nth x queens)
                      (nth y queens))))

(defun print-solution (solution &optional (stream *standard-output*))
  (loop
     with size = (solution-size solution)
     for queen in (solution-queens solution)
     do (loop for y from 0 below size
           do (format stream "~a " (if (= y queen) "Q" ".")))
       (terpri stream)))

(defun compute-energy (solution)
  (flet ((make-board ()
           (loop
              with size = (solution-size solution)
              with board = (make-array (list size size) :initial-element 0)
              for x from 0 below size
              for y in (solution-queens solution)
              do (setf (aref board x y) 1)
              finally (return board))))
    (loop
       with board = (make-board)
       with size = (solution-size solution)
       with conflicts = 0
       with diags = '((-1 -1) (1 1) (-1 1) (1 -1))
       for x from 0 below size
       for y in (solution-queens solution)
       do (loop for delta in diags
             do (loop
                   with tempx = x
                   with tempy = y
                   do
                     (incf tempx (first delta))
                     (incf tempy (second delta))
                   until (or (< tempx 0)
                             (< tempy 0)
                             (>= tempx size)
                             (>= tempy size))
                   do (incf conflicts (aref board tempx tempy))))
       finally (setf (slot-value solution 'energy) conflicts))))

(defun copy-solution (solution)
  (let ((copy (make-instance 'solution :size (solution-size solution)) ))
    (with-slots (queens energy) copy
      (setf queens (copy-list (solution-queens solution)))
      (setf energy (solution-energy solution)))
    copy))


(defun main ()
  (let ((current (make-solution))
        (working nil)
        (best (make-instance 'solution :energy 100)))
    (compute-energy current)
    (setf working (copy-solution current))
    (loop
       with alpha = 0.99
       with final-temperature = 0.5
       with steps-per-change = 100
       for temperature = 30.0 then (* temperature alpha)
       while (> temperature final-temperature)
       do
         (loop for i from 0 below steps-per-change
            with use-new = nil
            do
              (tweak-solution working)
              (compute-energy working)
              (if (<= (solution-energy working) (solution-energy best))
                  (setf use-new t)
                  (let ((test (random 1.0))
                        (delta (- (solution-energy working)
                                  (solution-energy current))))
                    (when (> (exp (/ (- delta) temperature)) test)
                      (setf use-new t))))
              (if use-new
                  (progn
                    (setf use-new nil)
                    (setf current (copy-solution working))
                    (when (<  (solution-energy current)
                              (solution-energy best))
                      (setf best (copy-solution current))))
                  (setf working (copy-solution current)))))
    (print-solution best)))

(main)
