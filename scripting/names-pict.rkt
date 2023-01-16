#lang racket

(require 2htdp/image
         "name-completion.rkt")

;; data just copied 
(define d faculty-present-bools)

(define side 15)
(define solid-box (rectangle side side "solid" "blue"))
(define empty-box (rectangle side side "solid" "white"))

(define graph
  (apply
   above
   (for/list ([elt (in-list d)])
     (beside
      (overlay/align "right" "middle"
                     (text (first elt) 10 "black")
                     (rectangle 150 0 "solid" "white"))
      (apply
       beside
       (for/list ([b (in-list (second elt))])
         (cond [b solid-box]
               [else empty-box])))))))

(define graph-with-backdrop
  (overlay graph
           (rectangle (image-width graph)
                      (image-height graph)
                      "solid"
                      "white")))

graph-with-backdrop
(save-image 
 graph-with-backdrop
 "/tmp/facultys.png")
