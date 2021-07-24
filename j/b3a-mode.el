; b3a-mode for emacs
; syntax highlighting for b3 assembler (in this directory)
(require 'generic-x)
(define-generic-mode 'b3a-mode
  '(" #") ; comments start with # but we need to allow '#
  '("si" "du" "ad" "su" "ov" "sw" "zd" "dz" "dr" "rd" "hp" "h0" "rt" "eq" "zp"
    "dy" "yd" "ry" "eq" "ne" "nx" "le" "rb" "an" "sl" "nt")
  '(("^#.*" . 'font-lock-comment-face)
    ("[!][a-z]" . 'font-lock-preprocessor-face)
    ("\\^[][a-z]" . 'font-lock-variable-name-face)
    ("bw\\|go\\|ev"  . 'font-lock-variable-name-face)
    ("\\![][a-z]" . 'font-lock-preprocessor-face)
    ("wb\\|zw" . 'font-lock-function-name-face)
    ("[']." . 'font-lock-string-face)
    ("[0-9A-Z][0-9A-Z]" . 'font-lock-constant-face))
  '("\\.b3a$")             ; which files
  nil                      ; other functions
  "mode for editing b3a files (b4 bootstrap assembler")
