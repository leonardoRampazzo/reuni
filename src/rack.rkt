#lang racket
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

(define abre-arquivo (open-input-file "../testes/b"))
(define linha (read-line abre-arquivo))
;(read-line in)
;(define linha (read-line in))


(define (arquivo-convertido-em-lista ponteiro linha)
  (cond
    [(eof-object? linha) empty]
    [else (cons (converte-em-lista (string-split linha " ")) (arquivo-convertido-em-lista ponteiro (read-line ponteiro)))]))

(define (converte-em-lista dia)
  (cond
    [(empty? dia) empty]
    [(= 3 (string-length (first dia))) (cons (first dia) (converte-em-lista (rest dia)))]
    [else (cons (transforma-intervalo (first dia)) (converte-em-lista (rest dia)))]))

(define (transforma-intervalo palavra)
  (let ([hora-inicial (transforma-horario (string-split (first (string-split palavra "-"))":"))]
        [hora-final   (transforma-horario (string-split (first (rest (string-split palavra "-")))":"))])
    (intervalo hora-inicial hora-final))) 

(define (transforma-horario palavra)
  (cond
    [(empty? palavra) empty]
    [else
     (let ([hora   (string->number (first palavra ))]
           [minuto (string->number (first(rest palavra )))])
       (horario hora minuto))]))


(define lista-importada (arquivo-convertido-em-lista abre-arquivo linha))
;; variavel lista-importada está o arquivo importado

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

(lista-dispo-arq "../testes/b")



#|
(list (list "seg" (list (intervalo (horario 08 30) (horario 10 30)) (intervalo (horario 14 03) (horario 16 00)) (intervalo (horario 17 10) (horario 18 10))))
      (list "ter" (list (intervalo (horario 13 30) (horario 15 45))))
      (list "qua" (list (intervalo (horario 11 27) (horario 13 00)) (intervalo (horario 15 00) (horario 19 00))))
      (list "sex" (list (intervalo (horario 07 30) (horario 11 30)) (intervalo (horario 13 30) (horario 14 00)) (intervalo (horario 15 02) (horario 16 00)) (intervalo (horario 17 20) (horario 18 30))))))
|#

