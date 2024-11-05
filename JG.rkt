#lang racket

(struct conta (id status) #:transparent)

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
