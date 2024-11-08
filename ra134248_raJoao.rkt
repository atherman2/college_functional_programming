#lang racket

(struct despesa (id valor prazo status) #:transparent)

(define (atualiza-registro lista id-novo-valor)
  (if (empty? lista)
      lista
      (if (equal? (despesa-id (first lista)) id-novo-valor)
          (cond
               [(equal? (despesa-status (first lista)) #f) (cons (struct-copy despesa (first lista) [status #t]) (rest lista))]
               [(equal? (despesa-status (first lista)) #t) (cons (struct-copy despesa (first lista) [status #f]) (rest lista))])
          (cons (first lista) (atualiza-registro(rest lista) id-novo-valor)))))





(define (filtra-registros lista criterio)
  (if (empty? lista)
      lista
      (if (equal? (despesa-status (first lista)) criterio)
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




(define (busca-despesa lista id-buscado)
    (cond
       [(not (empty? lista))
          (cond [(equal? (despesa-id (first lista)) id-buscado) (first lista)]
                [else (busca-despesa (rest lista) id-buscado)])
       ]
    )
)





(define (menu-remover lista)
   (display "Informe o ID da despesa a ser removida:")(newline)(define id-a-remover (read-line))
   (visualizar-despesa (busca-despesa lista id-a-remover))
   (display "   Confirma a remoção da despesa exibida?(S/N)")(newline)(define opcao (read-line))
   (if (equal? (string-upcase opcao) "S") (main (remove-despesa lista id-a-remover)) (main lista))
)





(define (menu-adicionar lista)
   (display "Informe os dados da despesa a ser adicionada:")(newline)
   (display "   ID:")(newline)(define id (read-line))
   (display "   Valor:")(newline)(define valor (read-line))
   (display "   Prazo(data):")(newline)(define prazo (read-line))
   (display "   Já foi paga?(S/N)")(newline)(define status (read-line))

   (define despesa-a-adicionar
      (despesa id valor prazo
         (equal? (string-upcase status) "S")
      )
   )

   (adiciona-despesa lista despesa-a-adicionar)
)





(define (menu-visualizar lista)
   (cond [(empty? lista) (display "- Não há despesas a serem exibidas -")])
   (define (visualizar-lista lista)
      (cond [(not (empty? lista)) (visualizar-despesa (first lista)) (visualizar-lista (rest lista))])
   )
   (visualizar-lista lista)
   (newline)
   lista
)



(define (visualizar-despesa despesa)
   (display "Despesa #")(display (despesa-id despesa))(newline)
   (display "   Valor: ")(display (despesa-valor despesa))(newline)
   (display "   Prazo: ")(display (despesa-valor despesa))(newline)
   (display "   Status: ")(display (if (despesa-status despesa) "Paga" "Pendente"))(newline)
)

(define (main lista-atual)
   
   (display "Gerenciador de despesas")(newline)
   (display "Para escolher uma das opções, informe o código correspondente:")(newline)
   (display "   Visualizar despesas: v")(newline)
   (display "   Adicionar despesa: a")(newline)
   (display "   Remover despesa: r")(newline)
   (display "   Filtrar despesas: f")(newline)
   (newline)
   (define opcao (read-line))

   (cond
      [(equal? opcao "v") (main (menu-visualizar lista-atual))]
      [(equal? opcao "a") (main (menu-adicionar lista-atual))]
      [(equal? opcao "r") (main (menu-remover lista-atual))]
   )
)

(define l1 (list (despesa "1" 1 "" #f) (despesa "2" 2 "" #t) (despesa "3" 3 "" #t) (despesa "4" 4 "" #f) (despesa "5" 5 "" #f) (despesa "6" 6 "" #f) (despesa "7" 7 "" #t)))
(main l1)
(display (despesa-id (first l1)))