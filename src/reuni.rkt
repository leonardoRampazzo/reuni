  #lang racket
;; Este programa encontra horários disponíveis que sejam comuns entre vários
;; horários especificados e que tenham um tamanho mínimo especificado.
;;
;; ** Conceitos **
;;  Horário
;;    Um momento no tempo, definido em termos da hora e minutos
;;  Intervalo (abreviado inter)
;;    Um intervalo no tempo, tem um horário de início e um horário de fim
;;  Disponibilidade do dia (abreviado dispo)
;;    Uma lista de intervalos que estão disponíveis em um determinado dia
;;  Disponibilidade semanal (abreviado dispo-semana)
;;    Uma lista com as disponibilidades de cada dia
;;  Lista de associações
;;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;;    elemento do par é chamado de chave e o segundo elemento é chamado de
;;    valor. Uma lista de associações é uma maneira simples de implementar uma
;;    tabela associativa (dicionário).  Ex: o dicionário
;;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado pela lista associativa
;;    (list (list 1 4) (list 20 12) (list 6 70)).
;;    A função assoc é utilizada para consultar uma lista associativa.
;;
;; ** Formatação de entrada e saída **
;; Toda operação de entrada e saída deve ser feita respeitando essas
;; formatações. A sua implementação não precisa validar as entradas. Para os
;; testes automatizados as entradas sempre serão válidas.
;;
;;  Horário (HH:MM) (sempre 5 dígitos)
;;  Exemplos
;;     08:30 =  8 horas e 30 minutos
;;     12:07 = 12 horas e  7 minutos
;;
;;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;;  Exemplos
;;     08:30-12:07 = o intervalo tem início às 8 horas e 30 minutos e tem
;;                   o fim às 12 horas e 7 minutos
;;
;;  Dias da semana
;;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;;
;;  Disponibilidade semanal
;;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;;    intervalos disponíveis naquele dia
;;  Exemplo
;;    ter 10:20-12:00 16:10-17:30
;;    sex 08:30-11:30
;;  Observe que nem todos os dias devem estar especificados. Os dias
;;  que não têm disponibilidades não devem ser especificados.


;; exporta as funções que podem ser utilizadas em outros arquivos
(provide horario
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

(struct horario (h m) #:transparent)
;; Horário representa um momento no tempo, definido em termos da hora e minutos
;;    h : Número - horas
;;    m : Número - minutos

(struct intervalo (inicio fim) #:transparent)
;; Intervalo representa um intervalo no tempo, tem um horário de início e um
;; horário de fim
;;    inicio : Horário - horário de início
;;       fim : Horário - horário de fim

;; Constante que define um intervalo vazio
(define intervalo-vazio (void))

;; Intervalo -> bool
;; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (define (funcao oper a b)
    (cond
      [(oper (horario-h a) (horario-h b)) a]
      [(and (=(horario-h a) (horario-h b)) (oper (horario-m a) (horario-m b))) a] 
      [else b]
     ))
  (define (disjuntos a b)
    (cond
      [(> (horario-h a) (horario-h b)) #t]
      [(and (=(horario-h a) (horario-h b)) (>= (horario-m a) (horario-m b))) #t] 
      [else #f]
     ))
  (define maiorDoInicio (funcao > (intervalo-inicio a) (intervalo-inicio b)))
  (define menorDoFim (funcao < (intervalo-fim a) (intervalo-fim b)))
  
  (if (disjuntos  maiorDoInicio menorDoFim) intervalo-vazio
       (intervalo maiorDoInicio menorDoFim))
)

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (define (menor a lst-b)
    (cond
      [(empty? lst-b) empty]
      [(let ([inters (intervalo-intersecao a (first lst-b))]
             [r (menor a (rest lst-b))])
       (if (intervalo-vazio? inters)
           r
           (cons inters r)))]))
  (define (maior lst-a)
    (cond
      [(empty? lst-a) empty]
      [(append (menor (first lst-a) dispo-b) (maior (rest lst-a)))])) 

  (maior dispo-a)
)
;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
;;
;; dispo-semana é uma lista de associações entre um dia (string) e a
;; disponibilidade naquele dia. Veja a definição de lista de associações no
;; início deste arquivo.
;;
;; Por exemplo, a disponibilidade semanal (dispo-semana):
;; ter 10:20-12:00 16:10-17:30
;; sex 08:30-11:30
;; é representada da seguinte maneira:
;; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;;                         (intervalo (hora 16 10) (hora 17 30))))
;;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;;
;; Observe que esta função recebe como parâmetro uma lista de disponibilidades
;; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
;; Veja os testes de unidade para exemplos de entrada e saída desta função
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (define diaSemana '("dom" "seg" "ter" "qua" "qui" "sex" "sab"))
  
  (define (tempoIntervalo a b)
    (let ([submin (- (horario-m b) (horario-m a))]
          [subhor (- (horario-h b) (horario-h a))])
    (if (negative? submin) (horario  (sub1 subhor) (+ submin 60))
        (horario subhor submin))))
  
  (define (tempoAmenorouigualB a b)
    (cond
      [(positive? (horario-h (tempoIntervalo a b)))]
      [(zero? (horario-h (tempoIntervalo a b)))]
      [else #f]))

  ;;tempo,lista de horarios --> devolve horarios maiores ou iguais ao tempo
  (define (filtraHorarios disposDiaSem)
    (cond
      [(empty? disposDiaSem) disposDiaSem]
      [(let ([inicio (intervalo-inicio (first disposDiaSem))]
             [fim    (intervalo-fim (first disposDiaSem))])
       (if (tempoAmenorouigualB tempo (tempoIntervalo inicio fim))
            (cons (first disposDiaSem) (filtraHorarios (rest disposDiaSem)))
            (filtraHorarios (rest disposDiaSem)))
           )]
     ))

  ;;lista com dias --> dias com disponibilidades filtradas
  (define (filtraInter lst)
    (define (filtr diaSemana)
      (cond
        [(empty? diaSemana) empty]
        [(eq? (assoc (car diaSemana) lst) #f) (filtr (cdr diaSemana))]
        [(empty? (filtraHorarios (cadr (assoc (car diaSemana) lst)))) (filtr (cdr diaSemana))]
        [else
         (append (list (cons (first diaSemana) (list (filtraHorarios (cadr (assoc (car diaSemana) lst))))))
                 (filtr (cdr diaSemana)))]))
    (filtr diaSemana))

  ;;lista de dispo-a,lista de dispo-b --> lista de dispo intersec
  (define (junta lst-a lst-b)
    (define (jnt diaSemana)
      (cond
        [(empty? diaSemana) empty]
        [(eq? (and (assoc (car diaSemana) lst-a) (assoc (car diaSemana) lst-b)) #f) (jnt (cdr diaSemana))]
        [else
         (let ([enc 1])
         (append (list (cons
                  (car diaSemana)
                  (list (encontrar-dispo-em-comum (cadr (assoc (car diaSemana) lst-a)) (cadr (assoc (car diaSemana) lst-b))))))
                 (jnt (cdr diaSemana)))
         )]))
     (jnt diaSemana)
    )

  ;;lista com disponibilidades--> lista de dispos em comum
  (define (percorreDispos dispos)
    (cond
      [(empty? dispos) empty]
      [(empty? (cdr dispos)) (filtraInter (car dispos))] ;;vai ser chamado quando só tiver um parametro
      [else (percorreDispos (cons (junta (first dispos) (second dispos)) (rest(rest dispos))))]   
     ) 
   )     
  
  (percorreDispos dispos) 
)

;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
;; devem ter. O tempo mínimo é especificado usando a formatação de horário.
;;
;; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
;; contêm uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
;; testes.
;;
;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.

(define (lista-dispo-arq nomearq)
  (define abre-arquivo (open-input-file nomearq))
  (define linha (read-line abre-arquivo))

  (define (montaHora hora)
    (horario (string->number(first (string-split hora ":")))
             (string->number(second (string-split hora ":")))))
  
  (define (montaInt intervalos)
    (cond
      [(empty? intervalos) empty]
      [else
       (let ([ini (montaHora (car (string-split (car intervalos) "-")))]
             [fim (montaHora (second (string-split (car intervalos) "-")))])
       (cons (intervalo ini fim) (montaInt (rest intervalos))))]))
  
  (define (montaDia linha)
    (list (substring linha 0 3) (montaInt(string-split (substring (car (string-split linha "\r")) 4) " "))))
  
  (define (leArquivo descritor linha)
    (cond
      [(eof-object? linha) empty]
      [else (cons (montaDia linha) (leArquivo descritor (read-line descritor)))]))

  (leArquivo abre-arquivo linha)
)

;;lista -> listaImpressa
(define (printaDispo disponibilidade)

  (define (montaInt inter)
    (cond
      [(empty? inter) (printf "\r")]
      [else (print (first inter)) (rest inter)])) ;;(cons inter (montaInt (rest inter)))]))
  
  (define (printaDia linha)
    (cond
      [(empty? linha) empty]
      [else (printf (car linha)) (printf " ") (montaInt (cdr linha)) ]
    ))

  (define (printa disponibilidade)
    (cond
      [(empty? disponibilidade) (printf "")]
      [else  (printaDia (first disponibilidade))
            (printaDispo (rest disponibilidade))]))
  
  (printa disponibilidade)
)


(define (main args)
  (define (formataTempo hora)
    (horario (string->number (first (string-split  hora ":"))) (string->number(second (string-split hora ":"))))
   )

  (define (dispos lst)
    (cond
      [(empty? lst) empty]
      [else (cons (lista-dispo-arq (first lst)) (dispos (rest lst)))]
      )
    )
   
  (printaDispo (encontrar-dispo-semana-em-comum (formataTempo (first args)) (dispos (rest args))))
)

(main (list "00:1" "../testes/a" "../testes/b"))
