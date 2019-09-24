#lang racket
(require 2htdp/image 2htdp/universe)

(define (arrow sym)
  (overlay/offset 
   (text (symbol->string sym) 18 "Pink")
   15 15
   (beside/align "center"
                 (rectangle 100 10 "solid" "green")
                 (rotate 270 (triangle 40 "solid" "red")))))
