#lang racket
;selam canım
(define sum-list(lambda(lst)
                    (if(null? lst) 0 (+ (car lst)(sum-list (cdr lst))))))

(define list-length(lambda(lst)
                       (if (null? lst) 0
                           (+ 1 (list-length(cdr lst))))))
(define reduce-list(lambda(lst base f)
                     (if(null? lst)
                        base
                        (f(car lst) (reduce-list(cdr lst) base f)))))

(define NEIGHBOR-LIST(lambda(pair)  (list (list (car pair) (+ (cadr pair) 1))
                                    (list (+ (car pair) 1 ) (cadr pair))
                                    (list (car pair) (- (cadr pair) 1))
                                    (list (- (car pair) 1) (cadr pair)))))
(define DIAGONAL-LIST( lambda(pair)  (list (list ( + (car pair)1) (+ (cadr pair) 1))
                                    (list (+ (car pair) 1 ) (-(cadr pair)1))
                                    (list (-(car pair)1) (+ (cadr pair) 1))
                                    (list (- (car pair) 1) (-(cadr pair)1)))))

(define ADJACENT (lambda(pair1 pair2) 
                   (or (isMember pair2(NEIGHBOR-LIST pair1))
                       (isMember pair2 (DIAGONAL-LIST pair1)) (and (= (car pair1) (car pair2))(= (cadr pair1)(cadr pair2))))))
                       

(define isMember (lambda (element list) (if (member element list) #t #f)))

(define ADJACENT-WITH-LIST(lambda(element list) (if (null? list) #f
                                                    (if (ADJACENT element (car list)) #t
                                                        (ADJACENT-WITH-LIST element (cdr list))))))
(define REPLACE-NTH(lambda (list index element) (if (= index 1) (cons element (cdr list)) ( cons (car list)  (REPLACE-NTH (cdr list) (- index 1) element)))))

(define RETURN-FIRST-NOT-FALSE(lambda(func list)(if (null? list) #f
                                                    (if (func (car list)) (func (car list)) (RETURN-FIRST-NOT-FALSE func (cdr list))))))


