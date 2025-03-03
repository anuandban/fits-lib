#lang typed/racket/shallow

(require math
         "./attr.rkt"
         "./extension/ascii-table.rkt"
         "./extension/binary-table.rkt"
         "./extension/image-table.rkt")

(provide Extension-Table
         HDU
         fits
         read-hdu
         read-data
         read-data-row
         read-data-col)

(define-type Extension-Table (U ascii-table binary-table image-table 'primary))
(define-type HDU (Pairof Header-Attr Extension-Table))

(struct fits ([hdu : (Vectorof HDU)]) #:prefab)

; 读取HDU单元
(: read-hdu (fits Integer -> HDU))
(define (read-hdu f idx)
  (vector-ref (fits-hdu f) idx))

; 读取HDU单元的数据部分
; 读取HDU单元数据部分的某一列(使用TTYPE定义的名称或者序数下标)
(: read-data (case->
              [HDU -> Any]
              [HDU (U Integer String) -> (U (Vectorof BinaryTableElement) (Vectorof AsciiTableElement))]))
(define read-data
  (case-lambda
    [(hdu)
     (cond [(image-table? (cdr hdu)) (image-table-data (cdr hdu))]
           [(binary-table? (cdr hdu)) (binary-table-data (cdr hdu))]
           [(ascii-table? (cdr hdu)) (ascii-table-data (cdr hdu))]
           [(eq? 'primary (cdr hdu)) (error "Cannot apply 'read-data' on Primary HDU")])]
    [(hdu id)
     (cond [(image-table? (cdr hdu)) (error "Cannot use index to read IMAGE table")]
           [(binary-table? (cdr hdu))
            (bt-read-field-col (cdr hdu) id)]
           [(ascii-table? (cdr hdu))
            (at-read-field-col (cdr hdu) id)]
           [(eq? 'primary (cdr hdu)) (error "Cannot apply 'read-data' on Primary HDU")])]))

; 读取HDU单元数据部分的某一行
(: read-data-row (HDU Integer -> (Vectorof Any)))
(define (read-data-row hdu row_id)
  (cond [(image-table? (cdr hdu)) (error "Cannot use read-data-row in IMAGE table")]
        [(binary-table? (cdr hdu))
         (matrix->vector (matrix-row (binary-table-data (cdr hdu)) row_id))]
        [(ascii-table? (cdr hdu))
         (matrix->vector (matrix-row (ascii-table-data (cdr hdu)) row_id))]
        [(eq? 'primary (cdr hdu)) (error "Cannot apply 'read-data-row' on Primary HDU")]))

; 读取HDU单元数据部分的某一列
(: read-data-col (HDU Integer -> (U (Vectorof BinaryTableElement) (Vectorof AsciiTableElement))))
(define (read-data-col hdu col_id)
  (read-data hdu col_id))