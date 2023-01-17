#lang racket

(require 2htdp/image
         mrlib/cache-image-snip)


(define (process-file inpath outpath)
  (printf "converting ~s -> ~s\n" inpath outpath)
  (define ip
    (open-input-file inpath))
  
  (define bw-image-regexp 
    #px#"\n% Image geometry\n([[:digit:]]+) ([[:digit:]]+) 8\n[^\n]*\n[^\n]*\n[^\n]*\n[^\n]*\nimage\n")
  (define color-image-regexp
    #px#"\n% Image geometry\n([[:digit:]]+) ([[:digit:]]+) 8\n[^\n]*\n[^\n]*\n[^\n]*\ncolorimage\n")
  
  (match-define (list glop xdim/str ydim/str)
    (regexp-match color-image-regexp ip))
  
  (define xdim (string->number (bytes->string/utf-8 xdim/str)))
  (define ydim (string->number (bytes->string/utf-8 ydim/str)))
  
  (define data (apply bytes-append
                      (sequence->list (in-bytes-lines ip))))
  
  (close-input-port ip)
  
  (unless (<= (* xdim ydim) (/ (bytes-length data) 2))
    (error "not enough data."))
  
  (define argb-vec
    (make-vector (* xdim ydim 4) 0))

  (define pixel-chars 6)
  
  (for ([y (in-range ydim)])
    (define local-data (subbytes data 
                                 (* pixel-chars xdim y)
                                 (* pixel-chars xdim (add1 y))))
    (for ([x (in-range xdim)])
      (define idx (* 4 (+ (* xdim y) x)))
      (vector-set! argb-vec idx 0)
      (define v1 (string->number 
                  (bytes->string/utf-8
                   (subbytes local-data 
                             (* x pixel-chars) 
                             (+ 2 (* x pixel-chars))))
                  16))
      (vector-set! argb-vec (+ 1 idx) v1)
      (define v2 (string->number 
                  (bytes->string/utf-8
                   (subbytes local-data 
                             (+ 2 (* x pixel-chars)) 
                             (+ 4 (* x pixel-chars))))
                  16))
      (vector-set! argb-vec (+ 2 idx) v2)
      (define v3 (string->number 
                  (bytes->string/utf-8
                   (subbytes local-data 
                             (+ 4 (* x pixel-chars)) 
                             (+ 6 (* x pixel-chars))))
                  16))
      (vector-set! argb-vec (+ 3 idx) v3)))
  
  (define leftover-data
    (subbytes data (* pixel-chars xdim ydim) (bytes-length data)))
  
  (printf "leftover data: ~e\n" leftover-data)
  
  
  (save-image (argb->bitmap (make-argb argb-vec xdim ydim))
              outpath))

(for/list ([f (in-directory "/Users/clements/csc-history-pre")]
      #:when (regexp-match #rx#".*\\.ps" f)
      )
  
  (define-values (path final dc) (split-path f))
  (with-handlers ([exn:fail? (lambda (exn)
                               (eprintf "failed on file: ~s with error: ~s\n" 
                                        f
                                        (exn-message exn)))])
    (define out-final (regexp-replace #px".ps" (path->string final) ".png" ))
    (process-file (build-path path final)
                  (build-path "/Users/clements/csc-history-images/" 
                              out-final))
    #;(delete-file f)))





