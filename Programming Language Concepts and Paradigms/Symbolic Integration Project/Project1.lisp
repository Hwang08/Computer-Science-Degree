;;;; Patrick Hwang                                                     10/29/18 6:18 PM                                                               
;;;; CSC 345
;;;; Project 1 - Symbolic Integration Source code
;;;; Project1.lisp

;;;--------------------------------------------------------------------------------------------
;;; Integrate function - The function F integrates with the variable V.

(defun integrate (F V &optional lower upper)
    (def-integral (indef-integral F V) V lower upper))

;;;--------------------------------------------------------------------------------------------
;;; Indefinite integration function

(defun indef-integral (F V)
    (labels (
        ;; Indefinite integration helper function
        (indef-integral-aux (F V)
            (cond
                ;; Numbers
                ((number-p F) (make-product F V))
                ;; Negative
                ((negative-p F) (make-negative (integrate (make-negative F) V)))
                ((variable-p F) (integrate (make-power F 1) V))
                ;; Addition
                ((sum-p F) (make-sum
                    (integrate (sum-first-operand F) V)
                    (integrate (sum-second-operand F) V)))
                ;; Subtraction
                ((difference-p F) (make-difference
                    (integrate (difference-first-operand F) V)
                    (integrate (difference-second-operand F) V)))
                ;; Power (n != -1)
                ((and (power-p F) (not (equal (power-second-operand F) -1))) (make-quotient
                    (make-power V (make-sum (power-second-operand F) 1))
                    (make-sum (power-second-operand F) 1)))
                ;; Power (n = -1)
                ((and (power-p F) (equal (power-second-operand F) -1) (make-log (power-first-operand F))))
                (t nil))))
        (cond
            ((mult-negative-p F) (indef-integral-aux (make-simplified-negative F) V))
            ((variable-p V) (indef-integral-aux F V))
            (t nil))))

;;;--------------------------------------------------------------------------------------------
;;;  Definite integration function

(defun def-integral (F V lower upper)
    (cond
        ((not (and (number-p lower) (number-p upper))) F)
        (t (eval (make-difference
            (my-replace V upper F)
            (my-replace V lower F))))))

;;;--------------------------------------------------------------------------------------------
;;; my-replace function - Replaces element E1 with E2 in the list L.

(defun my-replace (e1 e2 L)
    (labels
        ((my-replace-aux (e1 e2 L)
            (cond
                ((endp L) L)
                ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
                ((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
                (t (cons (first L) (my-replace e1 e2 (rest L)))))))
        (cond
            ((variable-p L) (first (my-replace e1 e2 (list L))))
            (t (my-replace-aux e1 e2 L)))))





;;;--------------------------------------------------------------------------------------------
;;; SYMBOLS

(defconstant *variable-symbols* '(U V W X Y Z))
(defconstant *negative-symbol* '-)
(defconstant *sum-symbol* '+)
(defconstant *difference-symbol* '-)
(defconstant *product-symbol* '*)
(defconstant *quotient-symbol* '/)
(defconstant *power-symbol* 'expt)
(defconstant *log-symbol* 'log)

;;;--------------------------------------------------------------------------------------------
;;;  OPERATORS

(defun negative-operator (F)
    (first F))

(defun sum-operator (F)
    (first F))

(defun difference-operator (F)
    (first F))

(defun product-operator (F)
    (first F))

(defun quotient-operator (F)
    (first F))

(defun power-operator (F)
    (first F))


;;;--------------------------------------------------------------------------------------------
;;; OPERANDS

(defun negative-operand (F)
    (second F))

(defun sum-first-operand (F)
    (second F))

(defun sum-second-operand (F)
    (third F))

(defun difference-first-operand (F)
    (second F))

(defun difference-second-operand (F)
    (third F))

(defun product-first-operand (F)
    (second F))

(defun product-second-operand (F)
    (third F))

(defun quotient-first-operand (F)
    (second F))

(defun quotient-second-operand (F)
    (third F))

(defun power-first-operand (F)
    (second F))

(defun power-second-operand (F)
    (third F))






;;;--------------------------------------------------------------------------------------------
;;; PREDICATES

(defun variable-p (F)
    (member F *variable-symbols*))

(defun number-p (F)
    (numberp F))

(defun negative-p (F)
    (cond
        ((and (number-p F) (< F 0)) t)
        ((number-p F) nil)
        ((variable-p F) nil)
        ((difference-p F) nil)
        ((and
            (equal (negative-operator F) *negative-symbol*)
            (not (equal (negative-operand F) *negative-symbol*))) t)))

(defun mult-negative-p (F)
    (labels
        ((mult-negative-p-aux (F L)
            (cond
                ((endp F) (equal (length L) 1))
                ((equal (first F) *negative-symbol*) (mult-negative-p-aux (rest F) L))
                (t (mult-negative-p-aux (rest F) (cons (first F) L))))))
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ((negative-p F) nil)
        ((difference-p F) nil)
        ((not (listp F)) nil)
        (t (mult-negative-p-aux F '())))))

(defun sum-p (F)
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ((and
            (equal (sum-operator F) *sum-symbol*)
            (sum-first-operand F)
            (sum-second-operand F)) t)))

(defun difference-p (F)
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ((and
            (equal (difference-operator F) *difference-symbol*)
            (not (equal (difference-first-operand F) *difference-symbol*))
            (difference-second-operand F)) t)))

(defun power-p (F)
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ((and
            (equal (power-operator F) *power-symbol*)
            (variable-p (power-first-operand F))
            (number-p (power-second-operand F))) t)))











;;;--------------------------------------------------------------------------------------------
;;; CONSTRUCTORS

(defun make-variable (V)
    V)

(defun make-negative (F)
    (labels (
        (make-negative-aux (F)
            (cond
                ((number-p F) (* -1 F))
                ((negative-p F) (negative-operand F))
                (t (list *negative-symbol* F)))))
    (cond
        ((mult-negative-p F) (make-negative-aux (make-simplified-negative F)))
        (t (make-negative-aux F)))))

(defun make-simplified-negative (F)
    (labels
        ((make-simplified-negative-aux (F)
            (cond
                ((equal (mod (length F) 2) 0) (list *negative-symbol* (first (last F))))
                (t (first (last F))))))
      (cond
        ((mult-negative-p F) (make-simplified-negative-aux F))
        ((negative-p F) F))))

(defun make-sum (F G)
    (cond
        ((equal F 0) G)
        ((equal G 0) F)
        ((equal F (make-negative G)) 0)
        ((equal G (make-negative F)) 0)
        ((and (number-p F) (number-p G)) (+ F G))
        (t (list *sum-symbol* F G))))

(defun make-difference (F G)
    (cond
        ((equal F 0) (make-negative G))
        ((equal G 0) F)
        ((and (number-p F) (number-p G)) (- F G))
        ((equal F (make-negative G)) (make-sum F G))
        (t (list *difference-symbol* F G))))

(defun make-product (F G)
    (cond
        ((equal F 0) 0)
        ((equal G 0) 0)
        ((equal F 1) G)
        ((equal G 1) F)
        ((equal F -1) (make-negative G))
        ((equal G -1) (make-negative F))
        ((and (negative-p F) (negative-p G)) (make-product (make-negative F) (make-negative G)))
        ((and (number-p F) (number-p G)) (* F G))
        (t (list *product-symbol* F G))))

(defun make-quotient (F G)
    (cond
        ((equal F 0) 0)
        ((equal G 0) nil)
        ((and (number-p F) (number-p G)) (/ F G))
        (t (list *quotient-symbol* F G))))

(defun make-power (V N)
    (cond
        ((and (number-p V) (numberp N)) (expt V N))
        (t (list *power-symbol* V N))))


(defun make-log (V)
    (cond
        ((variable-p V) (list *log-symbol* V))))
