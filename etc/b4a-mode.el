; b4a-mode for emacs
; syntax highlighting for b4 assembler
(require 'generic-x)
(define-generic-mode 'b4a-mode
  '(" #") ; comments start with # but we need to allow '#
  '("lb" "li" "sw" "du" "ov" "zp" "dr" "rd"
    "ad" "sb" "ml" "dv" "md" "ng" "sl" "sr"
    "an" "or" "xr" "nt"
    "eq" "ne" "gt" "lt" "ge" "le"
    "dx" "xd" "dy" "yd" "dz" "zd" "dc" "cd"
    "hl" "jm" "hp" "h0" "cl" "rt" "r0" "nx" "ev"
    "rb" "wb" "ri" "wi" "yr" "zw" "wp" "rp" "qp"
    ) ; bw go)
  '(("^#.*" . 'font-lock-comment-face)
    ("\\^[][a-z]" . 'font-lock-variable-name-face)
    ("bw\\|go\\|ev"  . 'font-lock-variable-name-face)
    ("[.][][a-z^\\\\]" . 'font-lock-preprocessor-face)
    ("wb\\|zw" . 'font-lock-function-name-face)
    ("[']." . 'font-lock-string-face)
    ("[0-9A-Z][0-9A-Z]" . 'font-lock-constant-face))
  '("\\.b4a$")             ; which files
  nil                      ; other functions
  "mode for editing b4a files (b4 assembler")
