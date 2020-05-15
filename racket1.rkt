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
                                                    (if (func (car list)) (func (car list)) (RETURN-FIRST-NOT-FALSE func (cdr list))))))
(define TENTS-SOLUTION (lambda (list)
                         (helper (car list) (cadr list) (cdr (caddr list))
                          '()
                           (possibleLoop (neigEliminate (car (caddr list)) (length (car list)) (length (cadr list))) (caddr list) '() (car list) (cadr list) ))))

;row (car list)
;columns (cadr list)
;trees(caddr list)

(define helper (lambda (rows columns trees tents possible)
                 (if (null? trees) (list (car possible)) (neighborLoop rows columns trees tents possible))))

(define neighborLoop(lambda(rows columns trees tents possible) (if (null? possible) #f
        (if (haveSolution (decr-Nth rows (car (car possible))) (decr-Nth columns (cadr (car possible))) (cdr trees)
            (cons (car possible) tents)
              (possibleLoop (neigEliminate (car trees) (length rows) (length columns)) (cdr trees) (cons (car possible) tents)
               (decr-Nth rows (car (car possible)))  (decr-Nth columns (cadr (car possible)))
            )) ;if have solution

            (append (helper (decr-Nth rows (car (car possible))) (decr-Nth columns (cadr (car possible))) (cdr trees)
                (cons (car possible) tents)
              (possibleLoop (neigEliminate (car trees) (length rows) (length columns)) (cdr trees) (cons (car possible) tents)
               (decr-Nth rows (car (car possible)))  (decr-Nth columns (cadr (car possible)))
            ));falanfilan and concatanate
                    (list (car possible)))
            
            (neighborLoop rows columns trees tents (cdr possible))))))




(define haveSolution(lambda (rows columns trees tents possible) (if (null? possible)
                                                                       #f
                                                                      (if (null? trees) #t
                                                   (neighborLoop rows columns trees tents possible)))))                                                                   
      ;(neighborLoop rows columns trees forbidden possible)                           

(define zeroCoordinate(lambda(rows cols candidate) (if (or(= 0(list-ref rows (- (car candidate) 1)))(= 0(list-ref cols (- (cadr candidate) 1))))
                                                     #t #f)))

;if true candidate is not a candidate anymore
(define ultimateController(lambda (trees tents rows cols candidate) (if (or (isMember candidate trees)(ADJACENT-WITH-LIST candidate tents)
                                                                             (zeroCoordinate rows cols candidate)) #t #f)))

(define possibleLoop(lambda(possibles trees tents rows cols)(if (null? possibles) '()
                      (if (ultimateController trees tents rows cols (car possibles))
                                                                 (possibleLoop (cdr possibles) trees tents rows cols)
                                                                 (cons (car possibles) (possibleLoop (cdr possibles) trees tents rows cols))))))

;  (helper rows columns trees forbidden (cdr possible))              
;maybe we dont need it anymore
(define forbiddenInit (lambda (trees rows cols) (set-union (gatherZeros rows cols) (list->set trees))))

;do not have a bright future 
(define forbAfter(lambda(rows cols adjs) (set-union (gatherZeros rows cols) adjs)))

; (set-member? st v)
;(set-add st v)
;(list->set lst)
;(list-ref lst index) get the element at the given index


;looks like a bunch of shit
(define forRow(lambda (x length) (if (= length 0) '() (cons (list x length) (forRow x (- length 1))))))
(define forColumn(lambda (x length) (if (= length 0) '() (cons (list length x) (forColumn x (- length 1))))))

(define rowTraverse(lambda(list otLen x)(if (null? list) '() (if (= 0 (car list)) (append (forRow x otLen) (rowTraverse (cdr list) otLen (+ x 1)))
                                                                 (rowTraverse (cdr list) otLen (+ x 1))))))

(define colmTraverse(lambda(list otLen x)(if (null? list) '() (if (= 0 (car list)) (append (forColumn x otLen) (colmTraverse (cdr list) otLen (+ x 1)))
                                                                 (colmTraverse (cdr list) otLen (+ x 1))))))

(define gatherZeros(lambda(rows cols) (list->set (append(colmTraverse cols (length rows) 1)(rowTraverse rows (length cols) 1)))));list-> set may be change


;not a bright future
(define allAdjacent (lambda(x)(list->set (cons (list (car x) (cadr x)) (append (DIAGONAL-LIST x) (NEIGHBOR-LIST x))))))


;this is the one will change greatly
(define possNeighs(lambda (neighs forbidden) (if (null? neighs) '() (if (set-member? forbidden (car neighs)) (possNeighs (cdr neighs) forbidden)
                                                                        (cons (car neighs) (possNeighs (cdr neighs) forbidden))))))

;this for proper elimination of out of map coordinates
(define neigEliminate(lambda(pair row col) (colElim(rowElim(NEIGHBOR-LIST pair) col )row)))

(define rowElim(lambda(lst col) (if (null? lst) '() (if (or(= (cadr(car lst)) 0)(= (cadr(car lst)) (+ col 1))) (rowElim (cdr lst) col)
                                                        (cons (car lst) (rowElim (cdr lst) col))))))
(define colElim(lambda(lst row) (if (null? lst) '() (if (or(= (car(car lst)) 0)(= (car(car lst)) (+ row 1))) (colElim (cdr lst) row)
                                                        (cons (car lst) (colElim (cdr lst) row))))))


;(define ast (list->set '((1 2) (2 3) (3 4) (1 2))))
;(possNeighs '((1 2) (5 6) (2 3) (7 8)) ast)
(define sth(lambda(list) (define x (car list)) x))
(define else (lambda(lst) (define y (sth lst)) (list y (cadr lst)) ))
