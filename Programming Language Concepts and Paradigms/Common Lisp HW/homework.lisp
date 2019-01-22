;;;; Patrick Hwang
;;;; CSC345 
;;;; homework.lisp

;;;-------------------------------------------------------------------------------------
;;; sum function: returns the sum of two integers n and m  recursively


(defun sum (n m)
  (cond
    ;; Checks if the arguments are integers
    ((not (and (typep n 'integer) (typep m 'integer))) nil)
    
    ;; Decrements the arguments to zero so the recursive function can end
    ((and (> (abs n) (abs m)) (> n 0)) (sum (1- m) (1+ n)))
    ((and (> (abs n) (abs m)) (< n 0)) (sum (1- m) (1+ n)))
    ((and (< (abs n) (abs m)) (> n 0)) (sum (1- n) (1+ m)))
    ((and (< (abs n) (abs m)) (< n 0)) (sum (1- n) (1+ m)))

    ;; Decrements n and checks if the aruments are equal
    ((eq (abs n) (abs m)) (sum (1- n) (1+ m)))

    ;; Returns the sum which will be m
    (t m)))


;;;---------------------------------------------------------------------------------------
;;; my-replace function: returns the list L with all occurrences of element e1 replaced,
;;; at all levels within the list, with element e2.

(defun my-replace (e1 e2 L)
  (cond
    ((endp L) L)
    ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))  ;; When an element is found the replacement is executed
    ((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
    (t (cons (first L) (my-replace e1 e2 (rest L))))))            ;; CDR's down the list


;;;---------------------------------------------------------------------------------------
;;; fibonacci function: 

(defun fibonacci (n)
  (cond
    ((minusp n) 0)                                    ;; Checks if the argument is negative
    ((< n 2) n)                                       ;; fibonacci sequence starting with 0 and 1
    (t (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))) ;; Calculates the fibonacci sequence


;;;---------------------------------------------------------------------------------------------------------------------
;;; fibonacci-tr function:

(defun fibonacci-TR(n)
  (labels ((cal-fibonacci (n x z)
	     (cond ((eql n 0) x)                            ;; If n is equal to 0 output 0
		   (t (cal-fibonacci (- n 1) z (+ x z)))))) ;; Calculates the fibonacci sequence
    (cal-fibonacci n 0 1)))



;;;-----------------------------------------------------------------------------------------------------------------------



;;; FOR USE IN HOMEWORK 1

;;;==================================================================================
;;; a macro to do FOR loops  ;; equivalent to Java for-loop control: for (int var=start, var<=stop; var+=update) body
(defmacro for ((var start stop update) &body body)
  (let ((gstop (gensym))                    ;; generate new symbols, GUARANTEED to be new; prevents capture
	(gupdate (gensym)))
    `(do ( (,gupdate ,update)               ;; needed so that the update expression is evaluated just once
	   (,var ,start (+ ,gupdate ,var))  ;; needed so that the stop expression is evaluated just once
	   (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;; EXAMPLE
;; CL-USER> (for (i 1 6 1) (print i))     ;; equivalent to Java for-loop control: (for int i=1, i<=6; i++)
;;
;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
;; 6 
;; NIL

;;;==================================================================================
(defun comparefibonaccis()
  (for (i 10 35 5)                               ;; for (int i=10; i<=35, i+=5) ...
       (format t "TAIL REC FIBONACCI ~a~%" i)
       (time(fibonacci-TR i))
       (format t "FIBONACCI ~a~%" i)
       (time(fibonacci i))
       (format t "=======================================================~%")
       )
  )

;;;==================================================================================
;;; END 

