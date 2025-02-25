#lang typed/racket

(require math
         typed/rackunit
         "../util.rkt"
         "../attr.rkt")

(provide AsciiTableElement
         ascii-table
         ascii-table-shape
         ascii-table-ttype
         ascii-table-data
         build-image-table)

;;;
;   ASCII表相关定义
;;;

(define-type AsciiTableElement
  (U Char Integer Real))

(struct ascii-table
  ([shape : (Pairof Nonnegative-Integer Nonnegative-Integer)]
   [ttype : (HashTable String Nonnegative-Integer)]
   [data : (Matrix AsciiTableElement)])
  #:prefab)

;;;
;   ASCII表操作函数
;;;

(: build-image-table (-> Input-Port Header-Attr ascii-table))
(define (build-image-table p at)
  ; 根据规范进行检查
  (when
      (or (not (eqv? 8 (attr-val (hash-ref at "BITPIX"))))
          (not (eqv? 2 (attr-val (hash-ref at "NAXIS"))))
          (not (eqv? 0 (attr-val (hash-ref at "PCOUNT"))))
          (not (eqv? 1 (attr-val (hash-ref at "GCOUNT"))))
          (not (and (integer? (attr-val (hash-ref at "TFIELDS")))
                    (<= (cast (attr-val (hash-ref at "TFIELDS")) Integer) 999))))
    (error 'ASCII-extension "wrong attribute value (BITPIX, GCOUNT, PCOUNT, NAXIS, TFIELDS)"))
  ; 准备初始化的解析条件
  (let* (;  对于解析表必须的属性值
         [naxis1 (cast (attr-val (hash-ref at "NAXIS1")) Integer)]
         [naxis2 (cast (attr-val (hash-ref at "NAXIS2")) Integer)]
         [tfields (cast (attr-val (hash-ref at "TFIELDS") Integer))]
         [tbcols (take-fields-named at "TBCOL" #t)]
         [tform (take-fields-named at "TFORM" #t)]
         [ttype (take-fields-named at "TTYPE")]
         ;  读出表的二进制字节形式
         [data_bytes
          (read-bytes-aligned p (* naxis1 naxis2))])
    ))