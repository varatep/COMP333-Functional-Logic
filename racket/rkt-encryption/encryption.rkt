#lang racket
(require math)

;used in vigenere cipher
(define chr integer->char)
(define ord char->integer)

;used in caesar cipher
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define a (char->integer #\a))
(define z (char->integer #\z))

;begin caesar cipher
(define (rotate c n)
  (define cnum (char->integer c))
  (define (shift base) (integer->char (+ base (modulo (+ n (- cnum base)) 26))))
  (cond [(<= A cnum Z) (shift A)]
        [(<= a cnum z) (shift a)]
        [else c]))

(define (caesar s n)
  (string-upcase (list->string (for/list ([c (in-string s)]) (rotate c n)))))

(define (encrypt s n) (caesar s n))
(define (decrypt s n) (caesar s (- 0 n)))
;end caesar cipher

;begin vigenere cipher
(define (v-encrypt msg key)
  (define cleaned
    (list->string
     (for/list ([c (string-upcase msg)]
                #:when (char-alphabetic? c)) c)))
  (list->string
   (for/list ([c cleaned] [k (in-cycle key)])
     (chr (+ (modulo (+ (ord c) (ord k)) 26) (ord #\A))))))

(define (v-decrypt msg key)
  (list->string
   (for/list ([c msg] [k (in-cycle key)])
     (chr (+ (modulo (- (ord c) (ord k)) 26) (ord #\A))))))
;end vigenere cipher

;examples:
;(define text "this class is awesome")
;(v-encrypt text "varatep")
;(v-decrypt text "varatep")


;(define text (encrypt "Comp 333 is awesome." 1))
;(decrypt text)


;(define text "this class is awesome")
;(encrypt text 1)
;(decrypt text 1)