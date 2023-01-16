#lang typed/racket

(require racket/runtime-path
         )

(require/typed sugar
               [slicef-at ((Listof String)
                           (String -> Boolean)
                           ->
                           (Listof (Listof String)))])

(define-runtime-path HERE ".")

(define lines (file->lines (build-path HERE "..//data/catalog-faculty-lists.txt")))

(define unparsed-catalogs
  (slicef-at lines
             (λ ([l : String])
               (and (regexp-match #px"^[0-9]{4}-[0-9]{2,4}$" l) #t))))

(struct catalog1 ([start-year : Natural]
                  [end-year : Natural]
                  [chair : (U Lastname String)]
                  [other-people : (Listof (U Lastname String))])
  #:transparent)

(struct catalog ([start-year : Natural]
                  [end-year : Natural]
                  [chair : String]
                  [other-people : (Listof String)])
  #:transparent)

(define-type Lastname (List 'lastname String))

(define (parse-name [s : String]) : (U Lastname String)
  (match s
    [(regexp #px"^[A-Za-z]+$") (list 'lastname s)]
    [other s]))

(define pre-catalogs
  (for/list : (Listof catalog1) ([c (in-list unparsed-catalogs)])
    (match (first c)
      [(regexp #px"^([0-9]{4})-([0-9]{2,4})$" (list _ begin end))
       (define full-end
         (cast
          (string->number
           (cond [(equal? (string-length (cast end String)) 2)
                  (string-append (substring (cast begin String) 0 2) (cast end String))]
                 [else (cast end String)]))
          Natural))
       (define begin-num
         (cast (string->number (cast begin String)) Natural))
       (define-values (comments non-comments)
         (partition (λ ([l : String]) (or (equal? (string-trim l) "")
                                          (regexp-match #px"^#" l)))
                    (rest c)))
       (printf "discarding empty or comment lines, check these: ~v\n"
               comments)
       (define-values (chair-or-heads others)
         (partition (λ ([p : String]) (regexp-match #px"\\((Chair|Head)\\)$" p)) non-comments))
       (unless (= (length chair-or-heads) 1)
         (error 'unexpected "zero or multiple chairs or heads for catalog ~a: ~v"
                begin-num
                chair-or-heads))
       (define chair-name
         (match (first chair-or-heads)
           [(regexp #px"^(.*) \\(" (list _ name)) (cast name String)]))
       (define people (map parse-name others))
       (catalog1 begin-num full-end (parse-name chair-name) people)])))

(define all-names
  (remove-duplicates
   (append
    (map catalog1-chair pre-catalogs)
    (apply append (map catalog1-other-people pre-catalogs)))))

(: lastnames (Listof (U Lastname String)))
(: fullnames (Listof String))
(define-values (fullnames lastnames)
  (partition string? all-names))

(define name-pairs
  (for/list : (Listof (Pair String String)) ([name (in-list fullnames)])
    (match name
      [(regexp #px"^(.*) ([A-Za-z]+)$" (list _ _ lastname))
       (cons (cast lastname String) name)])))

(define repeated-lastnames
  (map (inst car String)
       (filter (λ ([l : (Listof String)]) (< 1 (length l)))
               (group-by (λ (x) x) (map (inst car String) name-pairs)))))

(printf "disallowing mapping of these duplicated last names: ~v\n"
        repeated-lastnames)

(define name-hash
  (make-immutable-hash
   (filter (λ ([pr : (Pair String String)]) (not (member (car pr) repeated-lastnames)))
           name-pairs)))

(define (resolve-lastname [lastname : (U Lastname String)])
  (match lastname
    [(list 'lastname ln)
     (hash-ref name-hash ln)]
    [(? string? s) s]))

(define catalogs
  (for/list : (Listof catalog) ([c (in-list pre-catalogs)])
    (match c
      [(catalog1 start-year end-year chair other-people)
       (catalog start-year end-year
                (resolve-lastname chair)
                (map resolve-lastname other-people))])))

(for ([c1 (in-list catalogs)]
      [c2 (in-list (rest catalogs))])
  (unless (< (catalog-start-year c1) (catalog-start-year c2))
    (error 'zzz "another bad error message"))
  (unless (<= (catalog-end-year c1) (catalog-start-year c2))
    (error 'zzz "FIXME bad error message")))

(define year-present
  (for/hash : (Immutable-HashTable Natural (Setof String))
    ([c (in-list catalogs)])
    (values (catalog-start-year c)
            (list->set
             (cons (catalog-chair c)
                   (catalog-other-people c))))))


;; are all of the #t's in a list of boolean in a contiguous block?
(define (contiguous-trues? [lob : (Listof Boolean)]) : Boolean
  (cond [(empty? lob) #t]
        [else
         (cond [(first lob) (contiguous-trues/2? (rest lob))]
               [else (contiguous-trues? (rest lob))])]))

;; does this list contain zero or more #t's followed by all #f's?
(define (contiguous-trues/2? [lob : (Listof Boolean)]) : Boolean
  (cond [(empty? lob) #t]
        [else
         (cond [(first lob) (contiguous-trues/2? (rest lob))]
               [else (contiguous-trues/3? (rest lob))])]))

;; does this list contain all falses?
(define (contiguous-trues/3? [lob : (Listof Boolean)]) : Boolean
  (andmap not lob))

;; guaranteed not to contain duplicates by above check:
(define catalog-start-years (sort (map catalog-start-year catalogs) <))

(define faculty-present-bools
  (for/list : (Listof Any)
    ([f (in-list fullnames)])
    #;(for/fold ([range]))
    (define present-bools
      (for/list : (Listof Boolean) ([year (in-list catalog-start-years)])
        (set-member? (hash-ref year-present year) f)))
    #;(unless (contiguous-trues? present-bools)
      (eprintf "faculty member ~a has discontiguous presences\n" f))
    (list f present-bools)))







