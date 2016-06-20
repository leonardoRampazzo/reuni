#lang racket
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
  ;;operador,horario,horario -> horario
  ;;teste entre horarios (maior,menor ou igual)
  (define (funcao oper a b)
    (cond
      [(oper (horario-h a) (horario-h b)) a]
      [(and (=(horario-h a) (horario-h b)) (oper (horario-m a) (horario-m b))) a] 
      [else b]
     ))
  
  ;;Horario,Horario -> Disjuntos?
  ;;Verifica se a interseção é disjunta
  (define (disjuntos? a b)
    (cond
      [(> (horario-h a) (horario-h b)) #t]
      [(and (=(horario-h a) (horario-h b)) (>= (horario-m a) (horario-m b))) #t] 
      [else #f]
     ))
  
  (define maior-do-inicio (funcao > (intervalo-inicio a) (intervalo-inicio b)))
  (define menor-do-fim (funcao < (intervalo-fim a) (intervalo-fim b)))
  
  (if (disjuntos?  maior-do-inicio menor-do-fim) intervalo-vazio
       (intervalo maior-do-inicio menor-do-fim))
)

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  ;;dispo-b -> lista
  ;interseção de dispo-b para cada dispo-a
  (define (laco-interno a lst-b)
    (cond
      [(empty? lst-b) empty]
      [(let ([inters (intervalo-intersecao a (first lst-b))]
             [r (laco-interno a (rest lst-b))])
       (if (intervalo-vazio? inters)
           r
           (cons inters r)))]))
  
  ;;dispo-a -> lista de intervalos
  ;;interseção de dispo-a com dispo-b
  (define (laco-externo lst-a)
    (cond
      [(empty? lst-a) empty]
      [(append (laco-interno (first lst-a) dispo-b) (laco-externo (rest lst-a)))])) 

  (laco-externo dispo-a)
)

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
(define (encontrar-dispo-semana-em-comum tempo dispos)
  ;;Dias da semana
  (define dia-semana '("dom" "seg" "ter" "qua" "qui" "sex" "sab"))
  
  ;;horario,horario -> horario
  ;;retorna um intervalo em horas
  (define (tempo-intervalo a b)
    (let ([submin (- (horario-m b) (horario-m a))]
          [subhor (- (horario-h b) (horario-h a))])
    (if (negative? submin) (horario  (sub1 subhor) (+ submin 60))
        (horario subhor submin))))

  ;;horario,horario -> boolean
  ;;verifica se o tempo em horas de a é menor ou igual ao de b
  (define (tempo-A-menor-ou-igual-B a b)
    (cond
      [(positive? (horario-h (tempo-intervalo a b)))]
      [(zero? (horario-h (tempo-intervalo a b)))]
      [else #f]))

  ;;tempo,lista de horarios --> lista
  ;;devolve horarios maiores ou iguais ao tempo
  (define (filtra-horarios dispos-dia-sem)
    (cond
      [(empty? dispos-dia-sem) dispos-dia-sem]
      [(let ([inicio (intervalo-inicio (first dispos-dia-sem))]
             [fim    (intervalo-fim (first dispos-dia-sem))])
       (if (tempo-A-menor-ou-igual-B tempo (tempo-intervalo inicio fim))
            (cons (first dispos-dia-sem) (filtra-horarios (rest dispos-dia-sem)))
            (filtra-horarios (rest dispos-dia-sem)))
           )]
     ))

  ;;lista com dias -> lista
  ;;dias com disponibilidades filtradas
  (define (filtra-inters lst)
    (define (filtro diaSemana)
      (cond
        [(empty? diaSemana) empty]
        [(eq? (assoc (car diaSemana) lst) #f) (filtro (cdr diaSemana))]
        [(empty? (filtra-horarios (cadr (assoc (car diaSemana) lst)))) (filtro (cdr diaSemana))]
        [else
         (append (list (cons (first diaSemana) (list (filtra-horarios (cadr (assoc (car diaSemana) lst))))))
                 (filtro (cdr diaSemana)))]))
    (filtro dia-semana))

  ;;lista,lista -> lista 
  ;;lista de intersecao entre dispos
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
     (jnt dia-semana)
    )

  ;;lista com disponibilidades -> lista
  ;;percorre as disponibilidade e retorna a dispo em comum
  (define (percorre-dispos dispos)
    (cond
      [(empty? dispos) empty]
      [(empty? (cdr dispos)) (filtra-inters (car dispos))] 
      [else (percorre-dispos (cons (junta (first dispos) (second dispos)) (rest(rest dispos))))]   
     ) 
   )     
  
  (percorre-dispos dispos) 
)

;;arquivo -> lista
;;le o arquivo e retorna a lista de dispo
(define (lista-dispo-arq nomearq)
  ;;descritor do arquivo
  (define abre-arquivo (open-input-file nomearq))
  ;;linha  do arquivo
  (define linha (read-line abre-arquivo))

  ;;string ->horario
  (define (monta-hora hora)
    (horario (string->number(first (string-split hora ":")))
             (string->number(second (string-split hora ":")))))

  ;;string -> intervalo
  (define (monta-int intervalos)
    (cond
      [(empty? intervalos) empty]
      [else
       (let ([ini (monta-hora (car (string-split (car intervalos) "-")))]
             [fim (monta-hora (second (string-split (car intervalos) "-")))])
       (cons (intervalo ini fim) (monta-int (rest intervalos))))]))

  ;;string -> lista
  (define (monta-dia linha)
    (list (substring linha 0 3) (monta-int(string-split (substring (car (string-split linha "\r")) 4) " "))))

  ;;descritor,linhaatual -> lista
  (define (leArquivo descritor linha)
    (cond
      [(eof-object? linha) empty]
      [else (cons (monta-dia linha) (leArquivo descritor (read-line descritor)))]))

  (leArquivo abre-arquivo linha)
)

;;lista -> string
(define (transforma-em-texto disponibilidade)
  (define (monta-hora hora)
    (string-append    
    (~r (horario-h (intervalo-inicio hora)) #:min-width 2 #:pad-string "0") ":"
    (~r (horario-m (intervalo-inicio hora)) #:min-width 2 #:pad-string "0") "-"
    (~r (horario-h (intervalo-fim hora)) #:min-width 2 #:pad-string "0") ":"
    (~r (horario-m (intervalo-fim hora)) #:min-width 2 #:pad-string "0") "")
  )
    
  ;;intervalo '14:00-15:00 16:00-17:00'
  (define (monta-inter inter)
    (cond
      [(empty? inter) "~n"]
      [(empty? (cdr inter)) (string-append (monta-hora (car inter)) (monta-inter (cdr inter)))]
      [else (string-append (monta-hora (car inter)) " " (monta-inter (cdr inter)))])) 
  
  (define (monta-linha linha)
    (cond
      [(empty? linha) ""]
      [else (string-append (string-append (car linha) " " (monta-inter (car (cdr linha)))))]
    ))

  (define (printa disponibilidade)
    (cond
      [(empty? disponibilidade) ""]
      [(empty? (cdr disponibilidade)) (monta-linha (car disponibilidade))]
      [else (string-append (monta-linha (car disponibilidade)) (printa (cdr disponibilidade)))]))

  (printa disponibilidade)
)

;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
(define (main args)
  
  ;;string -> horario
  (define (formata-tempo hora)
    (horario (string->number (first (string-split  hora ":"))) (string->number(second (string-split hora ":"))))
   )

  ;;string -> lista de disponibilidades
  (define (dispos lst)
    (cond
      [(empty? lst) empty]
      [else (cons (lista-dispo-arq (first lst)) (dispos (rest lst)))]
      )
    )
   
  (define agenda-reuni(encontrar-dispo-semana-em-comum (formata-tempo (first args)) (dispos (rest args))))
  (define agenda-em-texto(transforma-em-texto agenda-reuni))
                        
  (printf agenda-em-texto)
)