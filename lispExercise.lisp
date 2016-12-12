;; Hemang Bhatt
;; Basic Lisp Exercises



;; This function takes the radius from user and calculate the volume of the sphere
(defconstant PI 3.141592)
(defun calcVolume()
  (format t "**** To compute the volume of a sphere **** ~%Enter Radius: ")
  (setq rad(read))
  (setq volume (/ (* 4 PI rad rad rad) 3))
  (format t "Volume of the sphere is ~f ~% *****************************~%" volume)
)


;; This function takes 3 input from user and calculate two roots
(defun getRoot()
  (format t "**** To compute the real or complex root of quadratic Equation ****~%")
  (format t "Enter coefficients of equation. To compute ax^2 + bx + c = 0, Enter a b c~%")
  (format t "Enter 3 coefficients: ")
  (setq a(read) b(read) c(read))
  (format t "~f   ~f  ~f ~%" a b c)
  
  (setq re(/ (- b) (* 2 a)))
  (setq real(- (* b b) (* 4 a c)))

  
  (cond ((>= real 0) (format t "The roots are real~%")
	 (format t "The root1 is ~f~%" (+ (/ (sqrt real) (* 2 a)) re) )
	 (format t "The root2 is ~f~%" (+ (/ (- (sqrt real)) (* 2 a)) re) )
	 

	 )
 
  ((< real 0) (format t "The roots are complex~%")
   	 (format t "The root1 is ~f + ~fi ~%" re (/ (sqrt real) (* 2 a)))
	 (format t "The root2 is ~f - ~fi ~%" re (/ (- (sqrt real)) (* 2 a)))


	 )
  )


)


;; This function takes list as a parameter and returns list of min and max 
(defun getMinMax (someList)
  (sort someList #'<)
  (setq num1 (car someList))
  (sort someList #'>)
  (setq num2 (car someList))
  (setq minmaxList (list num1 num2))
  (return-from getMinMax (list num1 num2))
)


;; This function takes a list as a parameter and sorts it using quicksort algorithm
(defun quicksort (someList)
  (labels ((swap (a b) (rotatef (elt someList a) (elt someList b)))
           (sub-sort (left right)
             (when (< left right)
               (let ((pivot (elt someList right))
                     (index left))
                 (loop for i from left below right
                       when (<= (elt someList i) pivot)
                         do (swap i (prog1 index (incf index))))
                 (swap right index)
                 (sub-sort left (1- index))
                 (sub-sort (1+ index) right)))))
    (sub-sort 0 (1- (length someList)))
    someList)

;;  (print someList)
  (return-from quicksort someList)
)


;; Find range of a List between Min and Max values
(defun range(aList min max)
  (sort aList #'<)
  (setq counter 0)
  (loop for x in aList do
	(cond ((and (>= x min) (<= x max))
	       (setq counter (+ counter 1))
;;	       (write x)
	       )
	      )
	;;(return-from range counter)
	)
  (return-from range counter)
)




(calcVolume)
(getRoot)

;; read some list
(format t "Enter a list: Ex. (3 2 10 1 7 8) ~%:")
(setq someList (read))

;; get min max of the list
(setq numbers (getMinMax someList))
(format t "The minimum number from the list is: ")
(write (car numbers))
(terpri)
(format t "The maximum number from the list is: ")
(write (car (last numbers)))
(terpri)



;; read some list and do the quicksort
(format t "Enter a list: Ex. (3 2 10 1 7 8) ~%:")
(setq someList (read))
(setq numbers (quicksort someList))
(format t "Quicksorted list:~%")
(print numbers)
(terpri)



(format t "Enter a list: Ex. (3 2 10 1 7 8) ~%:")
(setq someList (read))
(format t "Enter minimum number: ")
(setq minNum(read))
(format t "Enter maximum number: ")
(setq maxNum(read))
(setq num(range someList minNum maxNUm))
(format t "Number of element in range is: ")
(print num)
