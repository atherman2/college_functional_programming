#lang racket

(struct despesa (id valor prazo status) #:transparent)

(define (atualiza-registro lista id-novo-valor)
  (if (empty? lista)
      lista
      (if (equal? (conta-id (first lista)) id-novo-valor)
          (cond
               [(equal? (conta-status (first lista)) #f) (cons (struct-copy conta (first lista) [status #t]) (rest lista))]
               [(equal? (conta-status (first lista)) #t) (cons (struct-copy conta (first lista) [status #f]) (rest lista))])
          (cons (first lista) (atualiza-registro(rest lista) id-novo-valor)))))





(define (filtra-registros lista criterio)
  (if (empty? lista)
      lista
      (if (equal? (conta-status (first lista)) criterio)
          (cons (first lista) (filtra-registros (rest lista) criterio))
          (filtra-registros (rest lista) criterio))))





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
         (equal? (despesa-id (first lista)) nome-despesa)
         (rest lista)
         (cons (first lista) (remove-despesa (rest lista) nome-despesa))
      )
   )

)

(define l1 (list (despesa 1 1 "" #f) (despesa 2 2 "" #t) (despesa 3 3 "" #t) (despesa 4 4 "" #f) (despesa 5 5 "" #f) (despesa 6 6 "" #f) (despesa 7 7 "" #t)))