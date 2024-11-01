#lang racket

(struct despesa (nome valor prazo) #:transparent)

(define (adiciona-despesa lista despesa)

   (if
      (empty? lista)
      (list despesa)
      (cons (first lista) (adiciona-despesa (rest lista) despesa))
   )

)

(define (remove-despesa lista nome-despesa)

   (if
      (empty? lista)
      lista
      (if
         (equal? (despesa-nome (first lista)) nome-despesa)
         (rest lista)
         (cons (first lista) (remove-despesa (rest lista) nome-despesa))
      )
   )

)

(define despesa1 (despesa 1 1 1))
(define lista1 (cons despesa1 empty))
(display lista1) (newline)
(define despesa2 (despesa 2 2 2))
(adiciona-despesa lista1 despesa2)
(define lista2 (adiciona-despesa lista1 despesa2))
(define lista3 (adiciona-despesa lista2 (despesa 3 3 3)))
(display lista3) (newline)
(remove-despesa lista3 2)
(define listab1 (list 1))
(define listab2 (adiciona-despesa listab1 2))
(display listab2)
;;(adiciona-despesa listab2 3)