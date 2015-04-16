;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


(require picturing-programs)

(define bar (bitmap "Partofhang.png"))

(define word "Asdf")

(define hint "")

(define mod (list word (list ) hint))

make-revealed: string(word) string(guessed-letters) 
-> string(word-with-unguessed-as-underscores)
(define(make-revealed word knownletters)
  (cond [(empty? (string->list word))]))

; String List -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) false]
    [else (or (equal? (first l) s)
              (contains? s (rest l)))]))
(check-expect(contains? 1 (list 1 2 3))true)
(check-expect(contains? "a" (list "a" "b" "c"))true)
(check-expect(contains? 1 (list 2 2 3))false)
(check-expect(contains? "b" (list "a" "a" "c" "d" "alala"))falsw)
(check-expect(contains? "a" (list "h" "a" "n" "g" "m" "a" "n")true)

;same-or-reveal: character(letter) list-of-characters -> character
(define(sameorreveal c loc)
  (cond [(contains? l loc) l]
        [else #\_]))
(check-expect(sameorreveal "a" (list "a" "b" "c"))"a")
(check-expect(sameorreveal "a" (list "d" "b" "c"))#\_)
(check-expect(sameorreveal "h" (list "h" "a" "n" "g" "m" "a" "n"))"h")
(check-expect(sameorreveal "h" (list "h"))"h")
(check-expect(sameorreveal 1 (list "a" 1 "c"))1)

(define(keyhand))

(define(drawhand))
