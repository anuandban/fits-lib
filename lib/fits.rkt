#lang typed/racket

(require typed/rackunit
         math
         "./attr.rkt"
         "./extension/ascii-table.rkt"
         "./extension/binary-table.rkt"
         "./extension/image-table.rkt")

(module+ test
  (define tess (open-input-file "../test/tess2024249191853-s0083-0000010001363713-0280-s_lc.fits"))
  (define prim_h (read-header tess))
  (test-case
   "Binary table reader test"
   (let* ([ext_h (read-header tess)]
          [ext_table (build-binary-table tess ext_h)]
          [outfile (open-output-file "../test/test.txt" #:exists 'replace)])
     (pretty-print ext_h outfile)
     (println "----------TEST HEADER----------" outfile)
     (let-values ([(h w) (matrix-shape (binary-table-data ext_table))])
       (check-eq? h 17967)
       (check-eq? w 20))
     (pretty-print (binary-table-asize ext_table) outfile)
     (println "----------TEST HEADER----------" outfile)
     (pretty-print (binary-table-mapping ext_table) outfile)
     (println "----------TEST HEADER----------" outfile)
     (pretty-print (binary-table-tfields ext_table) outfile)
     (println "----------TEST HEADER----------" outfile)
     (pretty-print (binary-table-data ext_table) outfile))))

(define tess (open-input-file "e:/project/fits-lib/test/tess2024249191853-s0083-0000010001363713-0280-s_lc.fits"))
(define _h (read-header tess))
(define ext-h (read-header tess))
(define ext-t (build-binary-table tess ext-h))
(define ttime
  (vector-map (lambda ([x : (Listof Table-Element)]) : Real (cast (car x) Real))
              (matrix->vector (bt-read-field ext-t "TIME"))))
(define tflux
  (vector-map (lambda ([x : (Listof Table-Element)]) : Real (cast (car x) Real))
              (matrix->vector (bt-read-field ext-t "PDCSAP_FLUX"))))

(require plot)
(plot-new-window? #t)
(plot
 (points
  (vector-map
   (lambda ([x : Real] [y : Real]) (vector x y))
   ttime
   tflux)
  #:sym 'bullet))
