;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; retro-mode for emacs
;;
;;  version: 0.01
;;   author: michal j wallace (using retro.vim as reference)
;;  license: ISC (same as retro itself)
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A rudimentary major mode for editing retro files:
;;
;; http://www.retroforth.org/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'generic-x)


;----------------------------------------------------------
; syntax table : 
; This is is because retro uses lots of odd characters as
; parts of a word, and we want to match them in our regexps.
; ref: http://www.emacswiki.org/emacs/EmacsSyntaxTable

(defvar retro-table)
(setq retro-table ; by default, make everything a word constituent:
      (make-char-table 'syntax-table (string-to-syntax "w")))
;; whitespace characters:
(modify-syntax-entry 10 "-" retro-table) ; cr
(modify-syntax-entry 13 "-" retro-table) ; lf
(modify-syntax-entry 32 "-" retro-table) ; space


;; comment chars:
;(modify-syntax-entry 40 "<" retro-table) ; 40 = open paren -- no way to escape, afaik
;(modify-syntax-entry 41 ">" retro-table) ; 41 = close paren
;; delimiters: ( for quotes )
;(modify-syntax-entry 91 "(]" retro-table) ; open bracket
;(modify-syntax-entry 93 ")[" retro-table) ; close bracket
;; string chars:



;----------------------------------------------------------
; face definitions:

(defface retro-opword-face 
  '((default (:foreground "cyan" )))
  "Face for math/logic operators in retro.")

(defface retro-memword-face 
  '((default (:foreground "cyan" )))
  "Face for memory-related words in retro.")

(defface retro-stackword-face
  '((default (:foreground "cyan" )))
  "Face for stack-related words in retro.")

(defface retro-condword-face
  '((default (:foreground "gold" :bold t )))
  "Face for condition-related words in retro.")

(defface retro-loopword-face
  '((default (:foreground "gold" :bold t )))
  "Face for loop-related words in retro.")

(defface retro-defword-face
  '((default (:foreground "#c63" :bold t)))
  "Face for defining words in retro.")

(defface retro-number-face
  '((default (:foreground "limegreen")))
  "Face for numbers in retro.")

(defface retro-primitive-face
  '((default (:background "#116" :foreground "#99f" )))
  "Face for primitives in retro.")

(defface retro-module-face
  '((default (:foreground "tomato")))
  "Face for module names in retro.")

;----------------------------------------------------------
; a generic function to make it easy to define all the
; keywords below

(defun space-bound (rx)
  "wrap the regexp in word-boundary markers"
  (concat "\\<" rx "\\>" ))

(defun make-facewords (faceword-alist)
  "[ ( face , [ word ] ) ] -> "
  (let ((facewords
         (lambda (face words)
           (mapcar (lambda (word)                     
                     (cons (space-bound
                            (if (stringp word) word 
                              (regexp-quote (symbol-name word))))
                           face))
                   words))))
    (apply 'nconc (mapcar (lambda (alist) (apply facewords alist))
                          faceword-alist))))

(assert (equal 
         '(("\\<\\+\\>" . 'f0)
           ("\\<b\\>" . 'f0)
           ("\\<c\\>" . 'f0) 
           ("\\<x\\>" . 'f1)
           ("\\<y\\>" . 'f1)
           ("\\<z\\>" . 'f1))
          (make-facewords 
           '(('f0 (+ b c))
             ('f1 ("x" "y" "z"))))))
(assert (equal '(("\\<[a-z]+:\\>" quote f0))
               (make-facewords '(('f0 ("[a-z]+:" ))))))


;----------------------------------------------------------
; retro-mode

(define-generic-mode
    'retro-mode
  
  ;; comment chars go here, but we need more than just chars 
  '(  )
    
  ;; keywords
  '( "[" "]" "[[" "]]" ) ; simple keywords could go here

  (make-facewords 
   
   '(
     ('font-lock-comment-face
      (list "(.*?)"))

     ('retro-number-face
      (list "-?[0-9][0-9a-fA-F]*"))
     
     ('font-lock-string-face
      (list "\\\".*?\\\""))

     ('retro-primitive-face
      (list nop, dup, drop, swap, push, pop, loop, jump, return,
            >jump, <jump, !jump, =jump, @, !, +, -, *, /mod, 
            and, or, xor, <<, >>, "0;," 1+, 1-, in, out, wait, ))

     ('retro-opword-face 
      (list + - * / mod /mod neg abs min
            and or xor not << >> 1+ 1- ++ --
            < <> = > <= >= within getLength withLength compare ))
     
     ('retro-stackword-face
      (list drop nip dup over tuck swap rot -rot tib
            dip sip bi bi@ bi* tri tri@ tri* each each@
            2dup 2drop
            push pop r rdrop))
     
     ('retro-memword-face
      (list "@" ! +! @+ !+ -! on off
            keepString tempString accept
            "#mem" allot here zallot heap
            fill copy))
     
     ('retro-condword-face
      (list "if" "if;" else ";then" then ahead when whend
            =if <if !if >if ifTrue ifFalse))

     ('retro-module-face
      (list "\\w+'"))

     ('retro-loopword-face
      (list repeat again ";;" "0;" times iter iterd while for next ))
     

     ('retro-defword-face
      (list "[a-z]+:" "'" ":" ";[a-z]*" with-class 
            ".word" ".macro" ".data" ".primitive"
            eval ifNotDefined ifDefined 
            constant variable variable: elements
            create value "\\:is" is does> immediate
            compile-only compile "`" do
            literal literal, ":devector" devector
            find "," 
            "{{" "}}" "---reveal---"
            "with" "without" "needs" "global" "save" "bye"
            )) ))

    
  ;; auto-mode list (filename matchers)
  '("\\.rx$" )

  ;; startup hook
  (list (lambda () (set-syntax-table retro-table)))

  ;; docstring
  "A rudimentary major mode for editing retro files" )
