#lang racket
; a start on implementing the b4 virtual machine in racket
(require (for-syntax racket/syntax))
(provide make-b4-cpu)

(define todo println)
(define @ compose)

(struct b4-cpu
  [(C )       ; control registers (mini "dictionary")
   (D )       ; data stack
   (R )       ; return stack
   (M )       ; memory
   (END)      ; size of memory
   (P #:auto) ; program counter
   (G #:auto) ; flags
   (X #:auto) ; user register
   (Y #:auto) ; user
   (Z #:auto)]; user register
  #:auto-value 0
  #:mutable)
(define (make-b4-cpu [size 1024])
  (b4-cpu (make-vector 32 0) '() '() (make-bytes size 0) size)) ; C D R M END

(define-syntax-rule (make-accessor obj slot)
  (define-syntax (slot stx)
    (syntax-case stx ()
      [(_)
       (with-syntax ([get (format-id #'slot "b4-cpu-~a" #'slot)])
         #'(get obj))]
      [(_ x)
       (with-syntax ([set (format-id #'slot "set-b4-cpu-~a!" #'slot)])
         #'(set obj x))])))

(define-syntax (make-accessors stx)
  (syntax-case stx ()
    ([_ obj slot ...]
     (syntax/loc stx
       (begin (make-accessor obj slot) ...)))))


(define cpu (make-parameter (make-b4-cpu)))
(make-accessors (cpu) D R M P X Y Z C)

; microcode
(define (dput x) (D (cons x (D))))
(define (rput x) (R (cons x (R))))
(define (dtos) (first (D)))
(define (dnos) (second (D)))
(define (dswp) (let* [(d0 (D)) (a (first d0)) (b (second d0))] (D (list* b a d0))))
(define (dpop) (match (D) [(list x xs ...) (D xs) x] [else 0]))
(define (rpop) (match (R) [(list x xs ...) (R xs) x] [else 0]))
(define (bget x) (vector-ref (M) x))  ; fetch a byte (u8)    !! TODO: make sure it's a byte?
(define (sget x) (vector-ref (M) x))  ; TODO: should be signed!
;bput =: {{ M =: x (a. { 256#:y) } M }}     NB. write y as u8 (_1 as 255)
;mget =: {{ (4#256)#.a.i.(y+i.4) { M }}     NB. fetch i32
(define (mget a) (todo "mget only gets one byte, should get 4 bytes") (vector-ref (M) a))
;mput =: {{ M =: x (a.{~(4#256)#:y) } M }}  NB. store i32
;skpy =: {{ G =: (G AND NOT SKIPPY) VEL SKIPPY * y }}  NB. set skippy bit to y
(define (incp) (P (+ 1 (P))))
(define (inc4) (let [(p0 (P))] (P (+ 4 p0)) (+ 1 p0)))
(define (xinc) (let [(x0 (X))] (X (+ 1 x0)) x0)) ; y++
(define (yinc) (let [(y0 (Y))] (Y (+ 1 y0)) y0)) ; x++

; new helper
(define cpop (@ (λ(x) (AND x CMASK) dpop)))

; these are redundant but let us compare directly with the j syntax
(define (xget) (X)) (define (xset v) (X v))
(define (yget) (Y)) (define (yset v) (Y v))
(define (zget) (Z)) (define (zset v) (Z v))
(define (pget) (P)) (define (pset v) (P v))

; there are 32 "C" registers, so cget/set take an argument
; todo: contract that i e.i.32
(define (cget i) (vector-ref (C) i)) (define (cset i v) (vector-set! (C) i v))

(define (mask x) x ) ; TODO: 32 bit
(define (mfork: f g h) (λ (y) (g (f y) (h y))))
(define (dy: u) (λ (y) (dput (mask (u (dpop) (dpop))))))
(define (mo: u) (λ (y) (dput (mask (u (dpop))))))

; instruction set
; stack operations:
(define-syntax-rule (defop op micros ...) (define op (@ micros ...)))
(defop si dput sget incp) ; push next short int (signed byte) to data stack
(defop li dput mget inc4) ; literal/longint -> push next 4 bytes to data stack as i32
(defop sw dswp)           ; swap: xy->yx
(defop du dput dtos)      ; dup: x->xx
(defop ov dput dnos)      ; over: xy->xyx
(defop zp dpop)           ; zap: xy->x
(defop dr rput dpop)      ; data -> return stack
(defop rd dput rpop)      ; return -> data stack

; numeric instructions
(define ad (dy: +))  (define sb (dy: -))
(define ml (dy: *))  (define dv (dy: quotient))  (define md (dy: modulo))
(define ng (mo: -))  ; TODO : sl/sr (shift left right)

; comparison instructions (needs to cast bool to int)
(define-syntax-rule (defboo op fn) (define op (dy: (λ(x y) (if (fn x y) 1 0)))))
(defboo gt >)  (defboo lt <)  (defboo eq =)
(defboo ge >=) (defboo le <=) (define ne (dy: (λ(x y) (if (= x y) 0 1))))

; register instructions
(defop dx xset dpop) (defop dy yset dpop) (defop dz zset dpop)  (define dc (mfork: dpop cset cpop))
(defop xd dput xget) (defop yd dput yget) (defop zd dput zget)  (defop  cd dput cget cpop)

; control flow instructions
;hl =: pset@END                  NB. halt
;jm =: pset@<:@mget@inc4         NB. jump to M[P+1 2 3 4]-1 (because every op is followed by p++)
;hp =: pset@<:@(pget+bget)@incp  NB. hop to P+M[P+1]-1 (relative short jump)
;h0 =: hp`incp@.(0-:dpop)        NB. hop if tos==0 else skip addr
;h1 =: hp`incp@.(0~:dpop)        NB. hop if tos!=0 else skip addr
;nx =: (hp@rput@<:)`incp@.(0-:])@ rpop  NB. 'next': if (rtos--)==0 proceed, else hop
;cl =: jm@rput@pget              NB. call. like jump, but push P to return stack first
;rt =: pset&(rpop-1:)            NB. return
;r0 =: rt^:(0-:dpop)             NB. return if tos==0
;r1 =: rt^:(0~:dpop)             NB. return if tos!=0
;ev =: pset@<:@dpop@rput@pget    NB. eval. like call, but take address from stack instead of M[1+P]
