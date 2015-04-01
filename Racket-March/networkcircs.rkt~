;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname networkcircs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require picturing-programs)
(require 2htdp/universe)

(define-struct circ (x y rad col))
(define IP "127.0.0.1")




(define model (list (make-circ 0 0 20 "blue") (make-circ 250 0 20 "red")))


(define(+x model)
  (make-circ (+ (circ-x model) 4)
             (circ-y model)
             (circ-rad model)
             (circ-col model)))
(define(-x model)
  (make-circ (-(circ-x model)4)
             (circ-y model)
             (circ-rad model)
             (circ-col model)))
(define(+y model)
  (make-circ (circ-x model)
             (+ (circ-y model) 4)
             (circ-rad model)
             (circ-col model)))
(define(-y model)
  (make-circ (circ-x model)
             (- (circ-y model)4)
             (circ-rad model)
             (circ-col model)))


(define(keyhand model key)
   (cond [(key=? key "up") (list (-y (first model)) (second model))]
         [(key=? key "down")  (list (+y (first model)) (second model))]
         [(key=? key "right")  (list (+x (first model)) (second model))]
         [(key=? key "left")   (list (-x (first model)) (second model))]
         [else model]))

(define(handlemsg msg model)
  (list (first model) msg))

(define(packager model)
  (make-package model (c-list (first model))))

(define (netkeyhand model key)
  (packager (keyhand model key)))

(define(drawhand model)
  (place-image (circle (circ-rad (second model)) "solid" (circ-col (second model))) (circ-x (second model)) (circ-y (second model)) 
               (place-image (circle (circ-rad (first model)) "solid" (circ-col (first model)))
               (circ-x (first model)) (circ-y (first model)) (rectangle 500 500 "solid" "white"))))

(define(c-list cinfo)
  (list (circ-x cinfo) (circ-y cinfo) (circ-rad cinfo) (circ-col cinfo)))
                                                                     

(define(list-c list)
  (make-circ (first list) (second list) (third list) (fourth list)))

(big-bang model
          (on-key netkeyhand)
          (on-draw drawhand)
          (register IP)
          (name "Hello"))