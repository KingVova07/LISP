;Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим.

(defun rep (list x y)
    (cond 
        ((null list) nil)
        ((equal (car list) y) (cons x (rep (cdr list) x y)))
        (t (cons (car list) (rep (cdr list) x y)))
    )
)

(print '(3 задача))
(print (rep '(7 2 2 1 6 4) 'a 2))
;output: (7 A A 1 6 4) 


;Определите функциюю ,порождающую по заданному натуральному числу N список, состоящий из натуральных чисел от1 до N.
(defun counter (n lst)
    (cond
        ((null lst) (counter n '(1)))
        ((equal (car lst) n) lst)
        (t (counter n (cons (+ (car lst) 1) lst)))
    )
)

(defun rev (lst lst1)
    (cond 
        ((Null lst)  lst1)
        (t (rev (cdr lst) (cons (car lst) lst1)))
    )
)

(print '(4 задача))
(print (rev (counter 7 () ) () ))
;output (1 2 3 4 5 6 7)        
;задача 4
       


;Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел.
(defun compar (lst poslst neglst)
    (cond
        ( (null lst) (list poslst neglst))
        ( (equal (< 0 (car lst)) t) (compar (cdr lst) (cons (car lst) poslst) neglst))
        ( (equal (> 0 (car lst)) t) (compar (cdr lst) poslst (cons (car lst) neglst)))
        (t (compar (cdr lst) poslst neglst))
    )
)

(print '(8 задача))

(print (compar '(1 -2 3 -4 5 -6) () ()))
;output ((5 3 1) (-6 -4 -2))        
;задача 8
       
 


;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне
(defun combine (lst lstout)
    (cond
        ( (null lst) lstout)
        ( t (combine (cdr lst) (cons (car lst) lstout) ) )
    )
)


(defun delet (lst elem lstout)
    (cond
        ( (null lst) lstout )
        ( (equal elem (car lst)) (combine lstout (cdr lst)))
        ( t (delet (cdr lst) elem (cons (car lst) lstout) ) )
    )
)
       

(print '(21 задача))

(print (delet '(1 2 3 4 6 5 6) 6 '() ) ) 
;output (1 2 3 4 5 6) 

;21 задача






;Определите функцию, которая, чередуя элементы списков (a b ...) и (1 2 ...), образует новый список (a 1 b 2 ...).
(defun mixF (lst1 lst2 lstout)
    (cond
        (( null lst1) (complete lst2 lstout))
        (t (mixF lst2 (cdr lst1) (cons (car lst1) lstout)))
    )
)


(defun complete (lst lstout)
    (cond
        (( null lst) lstout)
        (t (complete (cdr lst) (cons (car lst1) lstout)))
    )
)   

(print '(27 задача))
(print ( rev (mixF '(1 2 3 4 5) '(a b c d e) () ) ()))
;output (1 A 2 B 3 C 4 D 5 E) 
;27 задача
