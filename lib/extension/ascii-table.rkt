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
         build-ascii-table)

;;;
;   ASCII表相关定义
;;;

(define-type AsciiTableElement
  (U String Integer Real))

(struct ascii-table
  ([shape : (Pairof Integer Integer)]
   [ttype : (HashTable String Integer)]
   [data : (Matrix AsciiTableElement)])
  #:prefab)

;;;
;   工具函数
;;;

; 根据TFORM格式化二进制串为表类型
(: format-bytes-value (-> Bytes String AsciiTableElement))
(define (format-bytes-value val tform)
  (match tform
    ; ASCII字符
    ["Aw" (bytes->string/utf-8 val)]
    ; 整数
    ["Iw" (cast (string->number (bytes->string/utf-8 val)) Integer)]
    ; 浮点数
    ["Fw.d" (cast (string->number (bytes->string/utf-8 val)) Real)]
    ; 科学计数法
    ["Ew.d" (cast (string->number (bytes->string/utf-8 val)) Real)]
    ["Dw.d" (cast (string->number (string-replace (bytes->string/utf-8 val) "D" "E")) Real)]))

; 解析二进制串为矩阵的函数
(: bytes->matrix
   (-> Bytes (Pairof Integer Integer) Integer (Listof Integer) (Listof String)
       (-> Bytes String AsciiTableElement)
       (Matrix AsciiTableElement)))
(define (bytes->matrix b ax_p naxis1 tbcols tforms fn)
  ; 将标记着一行里每个字段起始点的序列转化为各个字段长度的序列
  (: row-fields-length (->* ((Listof Integer)) ((Listof Integer)) (Listof Integer)))
  (define (row-fields-length col [rlt '()])
    (if (= (length col) 1)
        (reverse (cons (- naxis1 (sub1 (car col))) rlt))
        (row-fields-length
         (cdr col)
         (cons (- (sub1 (cadr (take col 2))) (sub1 (car (take col 2)))) rlt))))
  ; 构建矩阵
  (define idx_acc 0)
  (for/matrix (car ax_p) (cdr ax_p)
    ([idx
      (apply append
             (for/list ([_ (in-range (* (car ax_p) (cdr ax_p)))]) : (Listof (Listof Integer))
               (row-fields-length tbcols)))]
     [idx_tform
      (apply append
             (for/list ([_ (in-range (car ax_p))]) : (Listof (Listof String))
               tforms))])
    : AsciiTableElement
    (let ([fi_idx idx_acc])
      (set! idx_acc (+ idx_acc idx))
      (fn (subbytes b fi_idx (+ fi_idx idx)) idx_tform))))

(module+ test
  (test-case
   "bytes->matrix test"
   (let ([test_matrix
          (bytes->matrix #"123456"
                         (cons 3 2)
                         2
                         '(1 2)
                         '("Iw" "Iw")
                         format-bytes-value)])
     (check-eq? 3 (matrix-num-rows test_matrix))
     (check-eq? 2 (matrix-num-cols test_matrix)))
   (let ([test_matrix
          (bytes->matrix #"123456789"
                         (cons 3 2)
                         3
                         '(1 2)
                         '("Iw" "Aw")
                         format-bytes-value)])
     (check-eq? 3 (matrix-num-rows test_matrix))
     (check-eq? 2 (matrix-num-cols test_matrix)))))

;;;
;   ASCII表操作函数
;;;

(: build-ascii-table (-> Input-Port Header-Attr ascii-table))
(define (build-ascii-table p at)
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
         [tfields (cast (attr-val (hash-ref at "TFIELDS")) Integer)]
         [tbcols (take-fields-named at tfields "TBCOL" #t)]
         [tform (take-fields-named at tfields "TFORM" #t)]
         [ttype (take-fields-named at tfields "TTYPE")]
         ;  读出表的二进制字节形式
         [data_bytes
          (assert (read-bytes-aligned p (* naxis1 naxis2)) bytes?)]
         ;  内部函数：把Hashtable形式的tform和ttype转化为List
         [thash->tlist : (-> (HashTable Integer attr) (Listof AttrValue))
                       (lambda (thash)
                         (map (lambda ([v : (Pairof Integer attr)]) (attr-val (cdr v)))
                              (sort (hash->list thash)
                                    (lambda ([x : (Pairof Integer attr)] [y : (Pairof Integer attr)])
                                      (< (car x) (car y))))))])
    (ascii-table
     (cons naxis1 naxis2)
     (hash-map/copy ttype
                    (lambda ([k : Integer] [v : attr]) : (Values String Integer)
                      (values (assert (attr-val v) string?) k)))
     (bytes->matrix data_bytes
                    (cons naxis2 tfields)
                    naxis1
                    (cast (thash->tlist tbcols) (Listof Integer))
                    (cast (thash->tlist tform) (Listof String))
                    format-bytes-value))))