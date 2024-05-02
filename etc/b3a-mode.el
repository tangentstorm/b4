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
  '("\\.b3a$")             ; which files
  nil                      ; other functions
  "mode for editing b3a files (b4 bootstrap assembler")
