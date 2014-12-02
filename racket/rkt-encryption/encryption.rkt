#lang racket
(require math)

;used in vigenere cipher to convert char to ascii
;the char of the ordinance
(define chr integer->char)
;used in vigenere cipher to convert ascii to char
;the ordinance of the char
(define ord char->integer)

;used in caesar cipher
;define the beginning and ends so i can bounce the char in the last index to the beginning
; using the ordinance
(define A (char->integer #\A))
(define Z (char->integer #\Z))
(define a (char->integer #\a))
(define z (char->integer #\z))

;begin part 1: caesar cipher

;;this will rotate the letter an 'n' amount
;;@param c is the character
;;@param n is the amount to rotate
(define (shift c n)
  ;convert the character into an integer to do math on
  (define ascii_rep (char->integer c))
  ;actual computation for shifting the character an 'n' amount
  ;define a shift helper function to move letters over a certain 'n'
  (define (shift_rec base) (integer->char (+ base (modulo (+ n (- ascii_rep base)) 26))))
  ;check edge cases: char:Z -> char:A and char:z -> char:a
  ; otherwise return self / do nothing
  (cond [(<= A ascii_rep Z) (shift_rec A)]
        [(<= a ascii_rep z) (shift_rec a)]
        ;return self if not in the range of A-Z or a-z
        ; need to figure out a way to convert symbols safely
        ;  symbols aren't generally used in encryption/decryption
        [else c]))

;;for every item in the string, rotate/shift the letter
;;helper function for encrypt and decrypt
;;@param s is the string
;;@param n is the amount to rotate
(define (caesar s n)
  ;make sure the string is uppercase as a result
  (string-upcase (list->string (for/list ([c (in-string s)]) (shift c n)))))

;do the actual caesar encryption
(define (c-encrypt s n) (caesar s n))
;do the actual caesar decryption
(define (c-decrypt s n) (caesar s (- 0 n)))
;end caesar cipher

;begin part 2: vigenere cipher
;;@param msg is the message
;;@param key is the key used to create a ciphertext
;to encrypt, result = (msg[index] + key[index]) mod 26
(define (v-encrypt msg key)
  (define cleaned
    ;combine list of chars once done
    (list->string
     ;loop through the message list
     (for/list ([c (string-upcase msg)]
                #:when (char-alphabetic? c)) c)))
  ;combine list of chars once done
  (list->string
   ;loop through the key list
   (for/list ([c cleaned] [k (in-cycle key)])
     ;do the actual encryption formula
     (chr (+ (modulo (+ (ord c) (ord k)) 26) (ord #\A))))))

;;@param msg is the message
;;@param key is the key used to create a plaintext
;to decrypt, result = (msg[index] - key[index]) mod 26
(define (v-decrypt msg key)
  ;combine list of chars once done
  (list->string
   ;loop through the key list
   (for/list ([c msg] [k (in-cycle key)])
     ;do the actual decryption formula
     (chr (+ (modulo (- (ord c) (ord k)) 26) (ord #\A))))))
;end vigenere cipher

;examples:

;NOTE: the text and the key should be the same length for the norm of vigenere ciphers

;;vigenere
;(v-decrypt (v-encrypt text key) key) where @param text is a string message and @param key is the key
;(v-decrypt (v-encrypt "hello world" "asdfg asdfg") "asdfg asdfg")

;;caesar
;(define text "this class is awesome")
;(c-encrypt text 1) moves each char in "this class is awesome" right by one
;(c-decrypt text 1) moves each char in "this class is awesome left by one
;(c-decrypt (c-encrypt "hello world" 1) 1) will return "hello world" - it encrypts it and decrypts it again and outputs