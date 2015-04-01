;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname universe_experiment_1b_server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

; universe structure is just a list
(define-struct uni (active all))

(define test-uni-1  (make-uni false  (list iworld1 iworld2)))
(define test-uni-1b (make-uni false (list iworld3 iworld1 iworld2)))
(define test-uni-2  (make-uni iworld1 (list iworld1 iworld2)))
(define test-uni-2b (make-uni false (list iworld2)))
(define test-uni-2c (make-uni false (list iworld2 iworld1)))
(define test-uni-3  (make-uni false '()))

(define (uni-add u world)
  (make-uni (uni-active u) 
            (cons world (uni-all u))))

(check-expect (uni-add test-uni-1 iworld3)
              test-uni-1b)

(define (uni-deactivate u world)
  (make-uni false
            (append (rest (uni-all u))
                    (list world))))

(check-expect (uni-deactivate test-uni-2 iworld1)
              test-uni-2c)

(define (uni-activate-first u)
  (make-uni (first (uni-all u))
            (uni-all u)))

(check-expect (uni-activate-first test-uni-1)
              test-uni-2)

(define (new-world ustate world)
  (uni-add ustate world))

(check-expect (new-world test-uni-1 iworld3)
              (uni-add test-uni-1 iworld3))

(define (list-of-mail-to-activate-next ustate)
  (cond [(empty? (uni-all ustate))
         '()]
        [else (list (make-mail (first (uni-all ustate)) 'activate))]))

(check-expect (list-of-mail-to-activate-next test-uni-1)
              (list (make-mail iworld1 'activate)))

(check-expect (list-of-mail-to-activate-next test-uni-3)
              '())

(define (handle-msg ustate world msg)
  (cond [(equal? msg 'deactivate)
         (make-bundle (uni-deactivate ustate world)
                      (list-of-mail-to-activate-next ustate)
                      '())]
        [else ustate]))

(define (tick-handler ustate)
  (cond [(and (false? (uni-active ustate))
              (not (empty? (uni-all ustate))))
         (make-bundle (uni-activate-first ustate)
                      (list-of-mail-to-activate-next ustate)
                      '())]
        [else ustate]))

(universe (make-uni false '())
          (on-new new-world)
          (on-msg handle-msg)
          (on-tick tick-handler 0.1))

#|
Notes:
Worlds to use for testing: iworld{1,2,3}
|#

