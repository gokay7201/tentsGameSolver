#lang racket
;2017400072

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
(define decr-Nth(lambda(list index) (if (= index 1) (cons (- (car list) 1) (cdr list)) (cons (car list) (decr-Nth (cdr list) (- index 1))))))

(define RETURN-FIRST-NOT-FALSE(lambda(func list)(if (null? list) #f
                                                    (or (func (car list)) (RETURN-FIRST-NOT-FALSE func (cdr list))))))
(define TENTS-SOLUTION (lambda (list)
                         (neighborLoop (car list) (cadr list) (cdr (caddr list))
                          '()
                           (possibleLoop (neigEliminate (car (caddr list)) (length (car list)) (length (cadr list))) (caddr list) '() (car list) (cadr list) ))))

;row (car list)
;columns (cadr list)
;trees(caddr list)


(define neighborLoop(lambda(rows columns trees tents possible) (if (null? possible) #f
        (or (haveSolution (decr-Nth rows (car (car possible))) (decr-Nth columns (cadr (car possible))) (cdr trees)
            (cons (car possible) tents)
              (possibleLoop (neigEliminate (car trees) (length rows) (length columns)) (cdr trees) (cons (car possible) tents)
               (decr-Nth rows (car (car possible)))  (decr-Nth columns (cadr (car possible)))
            )) ;if have solution

            
            (neighborLoop rows columns trees tents (cdr possible))))))




(define haveSolution(lambda (rows columns trees tents possible) (if (null? possible)
                                                                       #f
                                                                      (if (null? trees) (cons (car possible) tents)
                                                   (neighborLoop rows columns trees tents possible)))))                                                                   
                                 

(define zeroCoordinate(lambda(rows cols candidate) (if (or(= 0(list-ref rows (- (car candidate) 1)))(= 0(list-ref cols (- (cadr candidate) 1))))
                                                     #t #f)))

;if true candidate is not a candidate anymore
(define ultimateController(lambda (trees tents rows cols candidate) (if (or (isMember candidate trees)(ADJACENT-WITH-LIST candidate tents)
                                                                             (zeroCoordinate rows cols candidate)) #t #f)))

(define possibleLoop(lambda(possibles trees tents rows cols)(if (null? possibles) '()
                      (if (ultimateController trees tents rows cols (car possibles))
                                                                 (possibleLoop (cdr possibles) trees tents rows cols)
                                                                 (cons (car possibles) (possibleLoop (cdr possibles) trees tents rows cols))))))

;(list-ref lst index) get the element at the given index

;this for proper elimination of out of map coordinates
(define neigEliminate(lambda(pair row col) (colElim(rowElim(NEIGHBOR-LIST pair) col )row)))

(define rowElim(lambda(lst col) (if (null? lst) '() (if (or(= (cadr(car lst)) 0)(= (cadr(car lst)) (+ col 1))) (rowElim (cdr lst) col)
                                                        (cons (car lst) (rowElim (cdr lst) col))))))
(define colElim(lambda(lst row) (if (null? lst) '() (if (or(= (car(car lst)) 0)(= (car(car lst)) (+ row 1))) (colElim (cdr lst) row)
                                                        (cons (car lst) (colElim (cdr lst) row))))))


