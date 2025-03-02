#lang typed/racket/shallow

(require "lib/fits.rkt"
         "lib/attr.rkt"
         "lib/util.rkt"
         "lib/extension/ascii-table.rkt"
         "lib/extension/binary-table.rkt"
         "lib/extension/image-table.rkt")

(provide read-fits
         read-hdu
         read-data
         read-data-col
         read-data-row
         BinaryTableElement
         AsciiTableElement
         $>)

(: read-fits (Input-Port -> fits))
(define (read-fits p)
  (: recur-read-fits (Input-Port (Listof (Pairof Header-Attr Extension-Table)) -> fits))
  (define (recur-read-fits po lfits)
    (let ([header (read-header po)])
      (if (false? header)
          (fits (list->vector (reverse lfits)))
          (cond [(eqv? (hash-ref header "SIMPLE" #f) #t)
                 (recur-read-fits po (cons (cons header 'primary) lfits))]
                [(eqv? (hash-ref header "XTENSION" #f) "IMAGE")
                 (recur-read-fits po (cons (cons header (build-image-table po header)) lfits))]
                [(eqv? (hash-ref header "XTENSION" #f) "TABLE")
                 (recur-read-fits po (cons (cons header (build-ascii-table po header)) lfits))]
                [(eqv? (hash-ref header "XTENSION" #f) "BINARY")
                 (recur-read-fits po (cons (cons header (build-binary-table po header)) lfits))]
                [else (error "Wrong hdu syntax :" (pretty-format header))]))))
  (recur-read-fits p '()))