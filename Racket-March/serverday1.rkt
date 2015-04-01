;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |server day 1 circle|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require picturing-programs)

(define model 15)
(define bg (rectangle 150 150 "solid" "white"))

(define(tickhand model)
  (cond [(< model 100) (+ 10 model)]
        [(> 100 model) 'inactive]
        [else model]))

(define(drawhand model)
  (cond [(< 100 model) (overlay (circle model "solid" "blue") bg)]
        [else (rectangle 150 150 "solid" "white")]))

;recieve:model msg --> model
(define (recieve model msg)
  (cond [(equal? msg 'deactivate) 'inactive]
        [(equal? msg 'activate) 0]
        [else model]))

(big-bang model
          (on-tick tickhand 1)
          (on-draw drawhand)
          (register "10.14.73.227")
          (name "asdfasdf"))

