;; b3a-mode for emacs
;; syntax highlighting for b3 assembler

;; TODO: strip out anything that isn't in b3a
;;
;; !! I started this when writing b3a-mode but then
;;    extended it when working on the bios, so it
;;    is (as of this writing) exactly the same as
;;    b4a-mode. The intent is to include ONLY the
;;    opes supported by b3a.
;;

(require 'generic-x)
(define-generic-mode 'b3a-mode
  '(" #") ; comments start with # but we need to allow '#
  '("ad" "sb" "ml" "dv" "md" "sh"
    "an" "or" "xr" "nt" "eq" "lt"
    "du" "sw" "ov" "zp" "dc" "cd"
    "rv" "wv" "lb" "li" "jm" "hp"
    "h0" "cl" "rt" "nx"
    "vb" "vi"
    "io" "db" "hl")
  '(("^#.*" . 'font-lock-comment-face)
    ("|.*" . 'font-lock-escape-face)
    ("[.][][a-z^\\\\]" . 'font-lock-preprocessor-face)
    ("`." . 'font-lock-builtin-face)
    ("[@!+^`][]\\@[A-Z_]" . font-lock-builtin-face)
    ("[:?][]:\\@[A-Z_]" . font-lock-preprocessor-face)
    ("%." . 'font-lock-preprocessor-face)
    ("[']." . 'font-lock-string-face)
    (":[^ ]+" . 'font-lock-function-name-face)
    ("$[^ ]+" . 'font-lock-constant-face)
    ("\\(\\<[0-9A-Z][0-9A-Z]\\)+\\>" . 'font-lock-constant-face))
  '("\\.b3a$")             ; which files
  nil                      ; other functions
  "mode for editing b3a files (b4 bootstrap assembler")
