#lang racket

(struct despesa (id valor prazo status) #:transparent)
;; Despesa representa uma Despesa de uma pessoa.
;; Id: String - Identificador da despesa
;; Valor: Float - Valor da despesa
;; Prazo: String - Data de vencimento da despesa
;; Status: Booleano - Status da depesa: #f caso ela esteja pendente, #t caso ela esteja paga.

(require examples)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Recebe uma lista do tipo despesa e uma string. 
;; Caso o elemento desejado for encontrado retorna o tipo despesa com seu valor status atualizado. Se receber #t ele retorna #f e vice-versa.
;;

(examples
 (check-equal? (atualiza-registro (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t)) "b") (list (despesa "a" 100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #f)))
 (check-equal? (atualiza-registro (list (despesa "a"  100.0 "10/02/2004" #f) (despesa "b" 150.0 "12/08/2024" #t)) "a") (list (despesa "a" 100.0 "10/02/2004" #t) (despesa "b" 150.0 "12/08/2024" #t))))
                                 
(define (atualiza-registro lista id-novo-valor)
  (if (empty? lista)
      lista
      (if (equal? (despesa-id (first lista)) id-novo-valor)
          (cons (struct-copy despesa (first lista)
                   [status (not(despesa-status (first lista))) ])
                (rest lista))
          (cons (first lista)
                (atualiza-registro(rest lista) id-novo-valor)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Recebe uma lista do tipo despesa e um booleano. 
;; Filtrar uma lista e retornar outra lista com todos os elementos filtrados.
;;

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

;;
;; Recebe uma lista do tipo despesa e um tipo despesa. 
;; Adicionar  um elemento do tipo despesa em uma lista e retornar a lista.
;;

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

;;
;; Recebe uma lista do tipo despesa e um tipo despesa. 
;; Remove um elemento do tipo despesa de uma lista. 
;;

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

;;
;; Recebe uma lista do tipo despesa e uma string. 
;; Faz uma busca de uma despesa pelo id na lista, e se encontrado retorna a despesa, caso não encontrado retorna um Erro.
;;

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

;;
;; Solicita ao usuário um id para remover da lista recebida como parâmetro
;; Trata os possíveis erros
;;

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

;;
;; Recebe uma lista do tipo despesa e uma string no formato de data ex: 08/08/2004(dia/mês/ano).
;; Retorna a soma de todo o valor das despesas daquele mês específico.
;;

(examples
 (check-equal? (soma-despesa (list (despesa "a"  100.0 "10/08/2004" #f) (despesa "b" 150.0 "12/08/2004" #t) (despesa "c" 890.0 "29/12/2010" #f)) "18/08/2004")
               250.0)
 (check-equal? (soma-despesa (list (despesa "a"  100.0 "10/12/2024" #f) (despesa "b" 150.0 "12/12/2024" #t) (despesa "c" 890.0 "29/12/2024" #f)) "18/12/2024")
               1140.0))


(define (soma-despesa lista prazo)
  (if (empty? lista) 0
      (if (equal? (substring (despesa-prazo (first lista)) 3) (substring prazo 3))
         (+ (despesa-valor (first lista))  (soma-despesa (rest lista) prazo))
         (soma-despesa (rest lista) prazo))))
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Solicita ao usuário uma data
;; Chama a função soma-despesa passando a data informada e a lista recebida como parâmetro 
;;

(define (menu-total lista)
   (display "Será exibido o valor total em reais da soma do valor das despesas com prazo no mês e ano referentes à data informada")(newline)
   (display "Insira a data:")(newline)(define data (read-line))
   (display "O valor total em reais da soma das despesas com prazo no mês e ano da data informada é de ") (display (soma-despesa lista data)) (newline)
   lista
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Solicita ao usuário qual o seu desejo: filtrar por despesas pagas ou pendentes
;; Chama a função filtra-registros passando a opção do usuário e a lista recebida como parâmetro
;;

(define (menu-filtrar lista)
   (display "Insira 1 para filtrar por despesas pagas - (1) ou 2 para filtrar por despesas pendentes - (2)")(newline)(define opcao (read-line))
   (define lista-filtrada (filtra-registros lista (equal? opcao "1")))
   (cond [(empty? lista-filtrada) (display "- Não há despesas a serem exibidas -") (newline)] [else (visualizar-lista lista-filtrada)])
   lista
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Recebe uma lista do tipo despesa. Solicita ao usuário o ID de uma despesa para removê-la
;; Após exibir a despesa do ID informado e receber uma confirmação do usuário, chama a função remove despesa passando o id informado e a lista recebida como parâmetro
;;

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

;;
;; Registra uma despesa com os dados passados pelo usuário
;; Chama a função adiciona-despesa passando a despesa registrada e a lista recebida como parâmetro
;;

(define (menu-adicionar lista)
   (display "Informe os dados da despesa a ser adicionada:")(newline)
   (display "   ID:")(newline)(define id (read-line))
   (display "   Valor em reais:")(newline)(define valor (read))(read-line)
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

;;
;; Recebe uma lista de objetos do tipo despesa, e, para cada objeto deste tipo na lista, aplica nele a função visualizar despesa
;;
;;

(define (visualizar-lista lista)
      (cond [(not (empty? lista)) (visualizar-despesa (first lista)) (visualizar-lista (rest lista))])
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Chama a função visualizar-lista passando a lista recebida como parâmetro
;; Caso não hajam elementos na lista, informa ao usuário
;;

(define (menu-visualizar lista)
   (cond [(empty? lista) (display "- Não há despesas a serem exibidas -")])
   (visualizar-lista lista)
   (newline)
   lista
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Recebe um objeto despesa 
;; Exibe os atributos desse objeto: ID, valor, prazo e status
;;

(define (visualizar-despesa despesa)
   (display "Despesa #")(display (despesa-id despesa))(newline)
   (display "   Valor: ")(display (despesa-valor despesa))(newline)
   (display "   Prazo: ")(display (despesa-prazo despesa))(newline)
   (display "   Status: ")(display (if (despesa-status despesa) "Paga" "Pendente"))(newline)
    
    
  
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coordena todo o código, informando ao usuário suas opções e solicitando uma escolha
;; Chama um dos menus do código de acordo com a escolha do usuário
;; Faz a chamada do menu de forma a se iniciar recursivamente passando como parâmetro o retorno do menu adequado
;;

(define (main lista-atual)
   
   (display "Gerenciador de despesas")(newline)
   (display "Para escolher uma das opções, informe o código correspondente:")(newline)
   (display "   Visualizar despesas: vis")(newline)
   (display "   Adicionar despesa: add")(newline)
   (display "   Remover despesa: rem")(newline)
   (display "   Filtrar despesas: ftr")(newline)
   (display "   Atualizar despesa: atual")(newline)
   (display "   Visualizar valor total no mês: tot")(newline)
   (newline)
   (define opcao (read-line))

   (cond
      [(equal? opcao "vis") (main (menu-visualizar lista-atual))]
      [(equal? opcao "add") (main (menu-adicionar lista-atual))]
      [(equal? opcao "rem") (main (menu-remover lista-atual))]
      [(equal? opcao "ftr") (main (menu-filtrar lista-atual))]
      [(equal? opcao "atual") (main (menu-atualizar lista-atual))]
      [(equal? opcao "tot") (main (menu-total lista-atual))]
   )
)

(define l1 (list (despesa "1" 1 "01/01/0001" #t)(despesa "2" 2 "01/01/0001" #f)(despesa "3" 3 "01/01/0001" #f)(despesa "4" 4 "01/01/0001" #t)
                 (despesa "5" 5 "01/01/0001" #t)(despesa "6" 6 "01/01/0001" #t)(despesa "7" 7 "01/01/0001" #t)
           )
)
(main l1)
