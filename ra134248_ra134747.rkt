#lang racket

(struct despesa (id valor prazo status) #:transparent)

(require examples)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(examples
 (check-equal? (atualiza-registro (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t)) "b") (list (despesa "a" 100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #f)))
 (check-equal? (atualiza-registro (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t)) "a") (list (despesa "a" 100.0 "10/02/2004" #t) (despesa "b" 150.0 "12/08/2024" #t))))
                                 
(define (atualiza-registro lista id-novo-valor)
  (if (empty? lista)
      lista
      (if (equal? (despesa-id (first lista)) id-novo-valor)
          (cond
               [(equal? (despesa-status (first lista)) #f) (cons (struct-copy despesa (first lista) [status #t]) (rest lista))]
               [(equal? (despesa-status (first lista)) #t) (cons (struct-copy despesa (first lista) [status #f]) (rest lista))])
          (cons (first lista) (atualiza-registro(rest lista) id-novo-valor)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(examples
 (check-equal? (filtra-registros (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) #f)
               (list (despesa "a" 100.0 "10/02/2004" #f) (despesa "c" 890.0 "29/12/2010" #f)))
 (check-equal? (filtra-registros (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) #t)
               (list (despesa "b" 150.0 "12/08/2024" #t))))

(define (filtra-registros lista criterio)
  (if (empty? lista)
      lista          
          (if (equal? (despesa-status (first lista)) criterio)
              (cons (first lista) (filtra-registros (rest lista) criterio))
              (filtra-registros (rest lista) criterio))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(examples
 (check-equal? (adiciona-despesa (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t)) (despesa "c" 890.0 "29/12/2010" #f))
               (list (despesa "a" 100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)))
 (check-equal? (adiciona-despesa (list) (despesa "a"  100.0 "10/02/2004" #f))
               (list (despesa "a" 100.0 "10/02/2004" #f))))
  
(define (adiciona-despesa lista despesa)

   (if
      (empty? lista)
      (list despesa)
      (cons (first lista) (adiciona-despesa (rest lista) despesa))
   )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(examples
 (check-equal? (remove-despesa (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) "b")
               (list (despesa "a" 100.0 "10/02/2004" #f) (despesa "c" 890.0 "29/12/2010" #f)))
 (check-equal? (remove-despesa (list (despesa "a"  100.0 "10/02/2004" #f)) "a")
               '()))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(examples
 (check-equal? (busca-despesa (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) "c")
               (despesa "c" 890.0 "29/12/2010" #f))
 (check-equal? (busca-despesa (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) "a")
               (despesa "a" 100.0 "10/02/2004" #f))
 (check-equal? (busca-despesa (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t) (despesa "c" 890.0 "29/12/2010" #f)) "d")
               "- ERRO: ID NÃO ENCONTRADO -"))

(define (busca-despesa lista id-buscado)
    (cond
       [(not (empty? lista))
          (cond [(equal? (despesa-id (first lista)) id-buscado) (first lista)]
                [else (busca-despesa (rest lista) id-buscado)])
       ]
       [else "- ERRO: ID NÃO ENCONTRADO -"]
    )
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-atualizar lista)
   (display "Informe o ID a ser atualizado:")(newline)(define id-a-ser-atualizado (read-line))
   (define despesa-a-ser-atualizada (busca-despesa lista id-a-ser-atualizado))
   (cond [(despesa? despesa-a-ser-atualizada)
            (visualizar-despesa despesa-a-ser-atualizada)
            (display "A despesa exibida terá seu valor de status atualizado de ")
            (if (despesa-status despesa-a-ser-atualizada) (display "paga") (display "pendente"))
            (display " para ")
            (if (despesa-status despesa-a-ser-atualizada) (display "pendente") (display "paga"))
            (newline)(display "Deseja prosseguir?(S/N)")(newline)(define opcao (read-line))
            (cond [(equal? (string-upcase opcao) "S") (display "- Despesa atualizada com sucesso! -") (newline) (atualiza-registro lista id-a-ser-atualizado)]
                  [else lista]
            )
         ]
         [else (display "- ERRO: ID NÃO ENCONTRADO -")]
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(examples
 (check-equal? (soma-despesa (list (despesa "a"  100.0 "10/08/2004" #f) (despesa "b" 150.0 "12/08/2004" #t) (despesa "c" 890.0 "29/12/2010" #f)) "18/08/2004")
               250.0)
 (check-equal? (soma-despesa (list (despesa "a"  100.0 "10/12/2024" #f) (despesa "b" 150.0 "12/12/2024" #t) (despesa "c" 890.0 "29/12/2024" #f)) "18/12/2024")
               1140.0))


;; falta colocar na main 
(define (soma-despesa lista prazo)
  (if (empty? lista) 0
      (if (equal? (substring (despesa-prazo (first lista)) 3) (substring prazo 3))
         (+ (despesa-valor (first lista))  (soma-despesa (rest lista) prazo))
         (soma-despesa (rest lista) prazo))))
       
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-atualizar lista)
   (display "Informe o ID a ser atualizado:")(newline)(define id-a-ser-atualizado (read-line))
   (define despesa-a-ser-atualizada (busca-despesa lista id-a-ser-atualizado))
   (cond [(despesa? despesa-a-ser-atualizada)
            (visualizar-despesa despesa-a-ser-atualizada)
            (display "A despesa exibida terá seu valor de status atualizado de ")
            (if (despesa-status despesa-a-ser-atualizada) (display "paga") (display "pendente"))
            (display " para ")
            (if (despesa-status despesa-a-ser-atualizada) (display "pendente") (display "paga"))
            (newline)(display "Deseja prosseguir?(S/N)")(newline)(define opcao (read-line))
            (cond [(equal? (string-upcase opcao) "S") (display "- Despesa atualizada com sucesso! -") (newline) (atualiza-registro lista id-a-ser-atualizado)]
                  [else lista]
            )
         ]
         [else (display "- ERRO: ID NÃO ENCONTRADO -")]
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-filtrar lista)
   (display "Insira 1 para filtrar por despesas pagas - (1) ou 2 para filtrar por despesas pendentes - (2)")(newline)(define opcao (read-line))
   (define lista-filtrada (filtra-registros lista (equal? opcao "1")))
   (cond [(empty? lista-filtrada) (display "- Não há despesas a serem exibidas -") (newline)] [else (visualizar-lista lista-filtrada)])
   lista
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (menu-remover lista)
   (display "Informe o ID da despesa a ser removida:")(newline)(define id-a-remover (read-line))
   (define despesa-a-remover (busca-despesa lista id-a-remover))
   (cond [(despesa? despesa-a-remover)
            (visualizar-despesa despesa-a-remover)
            (display "   Confirma a remoção da despesa exibida?(S/N)")(newline)(define opcao (read-line))
            (if (equal? (string-upcase opcao) "S") (remove-despesa lista id-a-remover) lista)
         ]
         [else (display "- ERRO: ID NÃO ENCONTRADO -")(newline) lista]
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visualizar-lista lista)
      (cond [(not (empty? lista)) (visualizar-despesa (first lista)) (visualizar-lista (rest lista))])
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (menu-visualizar lista)
   (cond [(empty? lista) (display "- Não há despesas a serem exibidas -")])
   (visualizar-lista lista)
   (newline)
   lista
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (visualizar-despesa despesa)
   (display "Despesa #")(display (despesa-id despesa))(newline)
   (display "   Valor: ")(display (despesa-valor despesa))(newline)
   (display "   Prazo: ")(display (despesa-prazo despesa))(newline)
   (display "   Status: ")(display (if (despesa-status despesa) "Paga" "Pendente"))(newline)
    
    
  
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main lista-atual)
   
   (display "Gerenciador de despesas")(newline)
   (display "Para escolher uma das opções, informe o código correspondente:")(newline)
   (display "   Visualizar despesas: vis")(newline)
   (display "   Adicionar despesa: add")(newline)
   (display "   Remover despesa: rem")(newline)
   (display "   Filtrar despesas: ftr")(newline)
   (display "   Atualizar despesa: atual")(newline)
   (newline)
   (define opcao (read-line))

   (cond
      [(equal? opcao "vis") (main (menu-visualizar lista-atual))]
      [(equal? opcao "add") (main (menu-adicionar lista-atual))]
      [(equal? opcao "rem") (main (menu-remover lista-atual))]
      [(equal? opcao "ftr") (main (menu-filtrar lista-atual))]
      [(equal? opcao "atual") (main (menu-atualizar lista-atual))]
   )
)

(define l1 (list (despesa "1" 1 1 #t)(despesa "2" 2 2 #f)(despesa "3" 3 3 #f)(despesa "4" 4 4 #t)(despesa "5" 5 5 #t)(despesa "6" 6 6 #t)(despesa "7" 7 7 #t)))
(main l1)