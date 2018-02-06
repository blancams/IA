;;; EJERCICIO 1.1
(defun sc-rec (x y)
   (if (or (null(check x)) (null(check y)) (not (= (length x) (length y))))
   nil
   (/ (prod-esc-rec x y)
      (* (sqrt (prod-esc-rec x x))
         (sqrt (prod-esc-rec y y))))))

(defun sc-mapcar (x y)
   (if (or (null(check x)) (null(check y)) (not (= (length x) (length y))))
   nil
   (/ (prod-esc-mapcar x y)
      (* (sqrt (prod-esc-mapcar x x))
         (sqrt (prod-esc-mapcar y y))))))

(defun prod-esc-rec (x y)
   (if (or (null x) (null y))
      0
      (+ (* (first x) (first y))
         (prod-esc-rec (rest x) (rest y)))))

(defun prod-esc-mapcar (x y)
   (apply #'+ (mapcar #'* x y)))

(defun check (x)
   (every #'identity (mapcar #'(lambda (y) (> y 0)) x)))

;;;
