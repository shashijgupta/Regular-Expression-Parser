#lang racket
(require parser-tools/lex
         parser-tools/yacc)
(require "declarations.rkt")
(require "utilities.rkt")

(define (nul t)
  (cond[(Epsilon? t) #t]
       [(Literal? t) #f]
       [(Or? t) (or (nul (Or-t1 t)) (nul (Or-t2 t)))]
       [(Then? t) (and (nul (Then-t1 t)) (nul (Then-t2 t)))]
       [else  #t]))
(define (buildNullable t)
  (cond[(Epsilon? t) (list (cons (Epsilon-n t) #t))]
       [(Literal? t)  (list (cons (Literal-n t) #f))]
       [(Or? t) (append (buildNullable (Or-t1 t)) (list (cons (Or-n t) (nul t))) (buildNullable (Or-t2 t)))]
       [(Then? t) (append (buildNullable (Then-t1 t)) (list (cons (Then-n t) (nul t))) (buildNullable (Then-t2 t)))]
       [else (append (buildNullable (Star-t t)) (list (cons (Star-n t) #t)) )]))

(define (first t)
  (cond[(Epsilon? t) null]
       [(Literal? t) (list (Literal-n t))]
       [(Or? t) (Union (first (Or-t1 t)) (first (Or-t2 t)))]
       [(Then? t) (if (nul (Then-t1 t)) (Union (first (Then-t1 t)) (first (Then-t2 t)))
                                        (first (Then-t1 t)))]
       [else (first (Star-t t))]))
(define (Union l1 l2)
  (define (helper l a b)
    (if (null? b) l
    (if (find a (car b)) (helper l a (cdr b)) (helper (append l (list (car b))) a (cdr b)))))
  (helper l1 l1 l2))
(define (find a b)
  (if ( null? a) #f
  (if (equal? (car a) b) #t (find (cdr a) b))))
(define tree1
(Then (Or (Then (Then (Then (Star (Or (Literal "a" 1)
(Literal "b" 2) 3) 4)
(Literal "b" 5) 6)
(Literal "b" 7) 8)
(Literal "a" 9) 10)
(Then (Literal "c" 11)
(Star (Literal "c" 12) 13) 14) 15)
(Literal "#" 16) 17))
(define (buildFirst t)
  (cond [(Epsilon? t) (list (list (Epsilon-n t)))]
        [(Literal? t) (list (list (Literal-n t) (Literal-n t)))]
        [(Or? t) (append (buildFirst (Or-t1 t)) (list (cons (Or-n t) (first t))) (buildFirst (Or-t2 t)))]
        [(Then? t) (append (buildFirst (Then-t1 t)) (list (cons (Then-n t) (first t))) (buildFirst (Then-t2 t)))]
        [else (append (buildFirst (Star-t t)) (list (cons (Star-n t) (first t))))]))
(define (last t)
  (cond[(Epsilon? t) null]
       [(Literal? t) (list (Literal-n t))]
       [(Or? t) (Union (last (Or-t1 t)) (last (Or-t2 t)))]
       [(Then? t) (if (nul (Then-t2 t)) (Union (last (Then-t1 t)) (last (Then-t2 t)))
                                        (last (Then-t2 t)))]
       [else (last (Star-t t))]))
(define (buildLast t)
   (cond [(Epsilon? t) (list (list (Epsilon-n t)))]
        [(Literal? t) (list (list (Literal-n t) (Literal-n t)))]
        [(Or? t) (append (buildLast (Or-t1 t)) (list (cons (Or-n t) (last t))) (buildLast (Or-t2 t)))]
        [(Then? t) (append (buildLast (Then-t1 t)) (list (cons (Then-n t) (last t))) (buildLast (Then-t2 t)))]
        [else (append (buildLast (Star-t t)) (list (cons (Star-n t) (last t))))]))

  (define (buildFollow t)
    (define (thenhelper t1 t2 n)
      (let*[(a(last t1))
           (b(first t2))]
      (define (f l1)
        (if (find a (car l1)) (append l1 b)
                        l1))
      (cond[(and(Epsilon? t1) (Epsilon? t2)) '()]
           [(Epsilon? t1) (list (list (car (first t2))))]
           [else(append ( map f (buildFollow t1)) (buildFollow t2))])))
    (define (orhelper t1 t2 n)
      (if (Epsilon? t1) (buildFollow t2)
                        (append (buildFollow t1) (buildFollow t2))))
    (define (starhelper t n)
      (let*[(a (last t))
            (b (first t))]
        (define (f l1)
        (if (find a (car l1)) (append l1 b)
                        l1))
        (if (Epsilon? t) null (map f (buildFollow t))))) 

         (cond[(Then? t) (thenhelper (Then-t1 t) (Then-t2 t) (Then-n t))]
         [(Or? t) (orhelper (Or-t1 t) (Or-t2 t) (Or-n t))]
         [(Star? t) (starhelper (Star-t t) (Star-n t))]
         [(Epsilon? t) '()]
         [else (list(list(Literal-n t)))]))
(define (treetosym t)
  (cond[(Epsilon? t) '()]
       [(Literal? t) (list (list (Literal-n t) (Literal-c t)))]
       [(Then? t) (append (treetosym (Then-t1 t)) (treetosym (Then-t2 t)))]
       [(Or? t) (append (treetosym (Or-t1 t)) (treetosym (Or-t2 t)))]
       [else  (treetosym (Star-t t)) ]))
  (define (searcher n l)
    (if (equal? n (car (car l))) (cdr (car l)) (searcher n (cdr l))))
(define (remove-duplicates l)
  (define (helper l1 l2)
    (if (null? l2) l1
    (if (find l1 (car l2)) (helper l1 (cdr l2)) (helper (append l1 (list (car l2))) (cdr l2)))))
  (helper '() l))
(define (removenull l)
  (define (helper l1 l2)
    (if (null? l2) l1
        (if (equal? null (car l2)) (helper l1 (cdr l2))
                                   (helper (append l1 (list (car l2))) (cdr l2)))))
  (helper '() l))
(define (buildGraph r)
  (let*[( tree ( maketree r))
        (followpos (buildFollow tree))
        (firstpos (buildFirst tree))
        (lastpos (buildLast tree))
        (nullable (buildNullable tree))
        (sym (treetosym tree))
        (lastsymbol (last tree))]
      (define (symgen l)
      (if (null? l) null
                   (append (list (car(cdr (car l)))) (symgen (cdr l)))))
    (define symbols (remove-duplicates(symgen sym)))
    (define (symbolfinder l s)
      (define (helper l1 l2)
        (cond[(null? l2) l1]
             [(equal? (searcher (car l2) sym) (list s)) (helper (append l1 (list (car l2))) (cdr l2))]
             [else (helper l1 (cdr l2))]))
      (removenull(helper '() l)))
    (define (symidentifier l)
      (map (lambda (s) (symbolfinder l s)) symbols))
    
  
  (define (nodegen l l1 l2)
    (cond [(and (null? l1) (null? l2)) l]
          [(null? l1) (nodegen l (append* (map symidentifier l2)) null)]
          [else (let* [(lis (remove-duplicates (append* (map (lambda (l3) (searcher l3 followpos)) (car l1)))))]
                      (if (find l lis)  (nodegen l (cdr l1) l2) (nodegen (append l (list lis)) (cdr l1) (append l2 (list lis)))))]))
    (define nodes (removenull(nodegen (list (first tree)) (symidentifier (first tree)) null)))
    (define symbols1 (reverse (cdr (reverse symbols))))
  
    (define (transgen l)
      (define (helper l1 l2 s)
        (cond[(null? s) l1]
             [(null? (symbolfinder l2 (car s))) (helper l1 l2 (cdr s))]
             [else (helper (append l1 (list (Trans l2 (car s) (remove-duplicates (append* (map (lambda (l3) (searcher l3 followpos)) (symbolfinder l2 (car s)) )))))) l2 (cdr s))]))
      (append* (map (lambda (a) (helper '() a symbols1)) l)))
    (define (redgen l)
      (define (helper l1 l2)
        (if (null? l2) l1
            (if (find (car l2) (car lastsymbol)) (helper (append l1 (list (car l2))) (cdr l2))
                                           (helper l1 (cdr l2)))))
      (helper '() l))
      
        
                                                                                                                
    
  (Graph (first tree) nodes (transgen  nodes) (redgen nodes) symbols)) )


