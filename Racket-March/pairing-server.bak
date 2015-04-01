#lang racket 

; pairing-server: This file creates a universe server that matches up 
; every two clients that connect. Once two clients are paired, any message
; that one of them sends is forwarded to the other.

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)

;; Stuff you may not be used to reading: 
;; 1. struct-copy lets you change only the fields you specify
;; 2. Hashtables have a whole section in the racket guide
;; 3. Rackunit uses check-false, check-true, check-equal? instead of check-expect
;; 4. local to define variables temporarily
;; 5. #:transparent always use this until someday you learn otherwise


; STRUCT uinfo - contains
; worlds: I ignore this but you could add the worlds you see to this list
; seeking-pairing: a world that is looking for a match
; pairings: keeps track of how to match up the pairs of worlds

(define-struct uinfo (worlds seeking-pairing pairings) #:transparent)

; make-pairings: list-of-pairs -> immutable-hash
; List of pairs is (list (cons a b) ...)
; Learn about hash tables later.
(define (make-pairings list-of-pairs)
  (make-immutable-hash list-of-pairs))

; make-universe-model: my own function to create uinfo structures.
; takes in a list of pairs instead of a hash table as the third argument
(define (make-universe-model worlds seeking list-of-pairs)
  (make-uinfo worlds seeking (make-pairings list-of-pairs)))

; make-universe-model-simple: list-of-worlds iworld -> uinfo
; Use this when constructing a universe model with no pre-existing pairings.
(define (make-universe-model-simple worlds seeking)
  (make-universe-model worlds seeking '()))

(define test-uinfo-1 (make-universe-model-simple (list iworld1) false))
(define test-uinfo-2 (make-universe-model-simple '() iworld2))

(check-equal? test-uinfo-2
	      (make-uinfo '() iworld2 (make-immutable-hash))
	      "basic make-universe-model construction")

; waiting-to-pair?: uinfo -> boolean
; true if there is a world waiting for another world to pair with it
(define (waiting-to-pair? u-model)
  (not (false? (uinfo-seeking-pairing u-model))))

(check-false (waiting-to-pair? test-uinfo-1))
(check-true  (waiting-to-pair? test-uinfo-2))

; pair-with: universe-model iworld -> universe-model
; Preconditions: (waiting-to-pair? universe-model) is true
; Post: the world that is waiting to pair and the second-world parameter
; are paired together in the returned universe-model's pairings.
(define (pair-with u-model second-world)
  (local [(define one-world (uinfo-seeking-pairing u-model))
	  (define old+new-pairings 
	    (hash-set* (uinfo-pairings u-model)
		       one-world second-world
		       second-world one-world))]
    (struct-copy uinfo u-model 
		 [seeking-pairing false]
		 [pairings        old+new-pairings])))


(define test-uinfo-2-pairing-world2+3 
  (make-universe-model '() false
		       (list (cons iworld3 iworld2)
			     (cons iworld2 iworld3))))

(check-equal? (pair-with test-uinfo-2 iworld3)
	      test-uinfo-2-pairing-world2+3
	      "pair-with did not generate the expected pairing")


; get-pairing: universe-model iworld -> iworld
; Return the world that is paired with the world you give it.
(define (get-pairing u-model iworld)
  (hash-ref (uinfo-pairings u-model) iworld))

(check-equal? (get-pairing test-uinfo-2-pairing-world2+3 iworld2)
	      iworld3
	      "In test-uinfo-2, iworld2 is not paired with iworld3")

; is-paired?: universe-model iworld -> boolean
; true if the given iworld has a pairing
(define (is-paired? u-model iworld)
  (not (false? (hash-ref (uinfo-pairings u-model) iworld false))))

(check-true (is-paired? test-uinfo-2-pairing-world2+3 iworld3))
(check-false (is-paired? test-uinfo-2-pairing-world2+3 iworld1))

; pair-or-wait: universe-model iworld -> universe-model
; if there is no world waiting to pair in the universe model, 
; place the given world in the waiting to pair slot of the universe.
; OTHERWISE pair with the world in the waiting to pair slot.
(define (pair-or-wait u-model iworld)
  (if (waiting-to-pair? u-model)

      (pair-with u-model iworld)

      (struct-copy uinfo u-model 
		   [seeking-pairing iworld])))

(check-equal? (pair-or-wait test-uinfo-1 iworld2) 
	      (struct-copy uinfo test-uinfo-1 [seeking-pairing iworld2])
	      "Did not remember which world was waiting for pairing")

(define test-uinfo-1-pairing-stage1 (pair-or-wait test-uinfo-1 iworld2))

(check-equal? (pair-or-wait test-uinfo-1-pairing-stage1 iworld3) 
	      (make-universe-model (list iworld1) false 
				   (list (cons iworld2 iworld3) 
					 (cons iworld3 iworld2)))
	      "Did not match up two worlds for pairing")


; new-world: universe-model iworld -> universe-model
(define (new-world u-model iworld)
  (pair-or-wait u-model iworld))

; make-mail-list: universe-model iworld msg -> list of mail
(define (make-mail-list u-model iworld msg)
  (list (make-mail (get-pairing u-model iworld) msg)))

; handle-message: universe-model iworld msg -> bundle 
(define (handle-message u-model iworld msg)
  (if (is-paired? u-model iworld)
      (make-bundle u-model
		   (make-mail-list u-model iworld msg)
		   '())
      ; no pairing? ignore the message
      u-model))

;;;;

(define UNIVERSE-STARTING-STATE (make-universe-model-simple '() false))

(universe UNIVERSE-STARTING-STATE
	  (on-new new-world)
	  (on-msg handle-message))

