; b4a-mode for emacs
; syntax highlighting for b4 assembler
(require 'generic-x)
(define-generic-mode 'b4a-mode
  '(" #") ; comments start with # but we need to allow '#
  '("ad" "sb" "ml" "dv" "md" "sh"
    "an" "or" "xr" "nt" "eq" "lt"
    "du" "sw" "ov" "zp" "dc" "cd"
    "ri" "rb" "wi" "wb" "lb" "li"
    "jm" "hp" "h0" "cl" "rt" "nx"
    "c0" "c1" "c2" "n1" "c4"
    "io" "db" "hl")
  '(("^#.*" . 'font-lock-comment-face)
    ("[']." . 'font-lock-string-face)
    ("|.*" . 'font-lock-escape-face)
    ("[.]." . 'font-lock-preprocessor-face)
    ("[@!+^`][]\\@[A-Z_]" . font-lock-builtin-face)
    ("[:?][]:\\@[A-Z_]" . font-lock-preprocessor-face)
    ("%." . 'font-lock-preprocessor-face)
    (":[^ ]+" . 'font-lock-function-name-face)
    ("`[^ ]+" . 'font-lock-constant-face)
    ("\\(\\<[0-9A-Z][0-9A-Z]\\)+\\>" . 'font-lock-constant-face))
  '("\\.b4a$")             ; which files
  nil                      ; other functions
  "mode for editing b4a files (b4 assembler")
