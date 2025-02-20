#lang typed/racket

(require math
         typed/rackunit
         "../util.rkt"
         "../attr.rkt")

(provide Table-Element
         binary-table
         binary-table-asize
         binary-table-data
         binary-table-mapping
         binary-table-tfields
         build-binary-table
         (rename-out [read-field bt-read-field]
                     [write-data bt-write-data]
                     [update-data bt-update-data]))

;;;
; 二进制表相关的类型定义       
;;;

(define-type Bits-Array (List Integer))

(define-type Array-Descriptor Bytes)

(define-type Table-Element
  (U Boolean Bits-Array Bytes String Integer Float Float-Complex Number Array-Descriptor))

(struct binary-table
  (; 由TFIELD描述的各个字段在一行中类型名, 占据的字节长度, 字段内值数量.
   [tfields : (Vectorof (List String Integer Integer))]
   ; 由NAXIS1和NAXIS2决定的表尺寸(行字节宽度*列字节高度)
   [asize : (Pairof Nonnegative-Integer Nonnegative-Integer)]
   ; 可选的由TTYPE决定的每个字段的别名
   [mapping : (HashTable String Nonnegative-Integer)]
   ; 原始字节解析后的矩阵
   [data : (Matrix (Listof Table-Element))])
  #:prefab)

;;;
;   二进制表操作函数
;;;

;;  -----工具函数-----

;   field-offset的协助函数
;   根据f的内容确认其映射的长度是多少，注意单位是字节
;   一个字段可以包含多个值，因此返回的还包括值的数量
(: binary-format-ref (-> String (List String Integer Integer)))
(define (binary-format-ref f)
  (let* (; 匹配逻辑，比特，字节，16/32/64位整数，单双精度浮点数和复数
         [retf1 #px"^(0?|[1-9]*)([LXBIJKAEDCM])(.?) *"]
         ; 匹配32/64位数组描述符
         [retf2 #px"^([01]?)([PQ])(.?) *"]
         [p1 (regexp-match retf1 f)]
         [p2 (regexp-match retf2 f)])
    (cond
      [(pair? p1)
       (let* ([size (assert (cadr p1) string?)]
              [tchar (assert (caddr p1) string?)]
              [c (if (string=? "" size)
                     1
                     (assert (string->number size) nonnegative-integer?))]
              [b (match tchar
                   ["L" 1] ; 布尔值
                   ["X" 1] ; 位数，具体数值不在此出示，对齐到所需的最小字节
                   ["B" 1] ; 无符号字节
                   ["I" 2] ; 16位整数
                   ["J" 4] ; 32位整数
                   ["K" 8] ; 64位整数
                   ["A" 1] ; ASCII字符
                   ["E" 4] ; 单精度浮点数
                   ["D" 8] ; 双精度浮点数
                   ["C" 8] ; 单精度复数
                   ["M" 16]; 双精度复数
                   [else 0])]
              [len (* b c)])
         (with-asserts ([len integer?]
                        [len exact?]
                        [c integer?]
                        [c exact?])
           (list tchar len c)))]
      [(pair? p2)
       (let* ([size (assert (cadr p2) string?)]
              [tchar (assert (caddr p2) string?)]
              [c (if (string=? "" size)
                     (ann 1 Nonnegative-Integer)
                     (assert (string->number size) nonnegative-integer?))]
              [b (match tchar
                   ["P" 8] ; 32位数组描述符
                   ["Q" 16] ; 64位数组描述符
                   )]
              [len (* b c)])
         (with-asserts ([len exact?]
                        [len integer?]
                        [c integer?]
                        [c exact?])
           (list tchar (* b c) c)))]
      [else (raise "Wrong TFORM data types")])))

(module+ test
  (test-case "binary-format-ref-test"
    ;; 测试binary-format-ref函数
    ; 读取逻辑值
    (check-equal? (binary-format-ref "8L") '("L" 8 8))
    (check-equal? (binary-format-ref "0L") '("L" 0 0))
    (check-equal? (binary-format-ref "L") '("L" 1 1))
    ; 读取位值
    (check-equal? (binary-format-ref "X") '("X" 1 1))
    ; 读取无符号字节
    (check-equal? (binary-format-ref "B") '("B" 1 1))
    ; 读取16位整数
    (check-equal? (binary-format-ref "12I") '("I" 24 12))
    (check-equal? (binary-format-ref "I") '("I" 2 1))
    ; 读取32位整数
    (check-equal? (binary-format-ref "1J") '("J" 4 1))
    (check-equal? (binary-format-ref "J") '("J" 4 1))
    ; 读取64位整数
    (check-equal? (binary-format-ref "1K") '("K" 8 1))
    (check-equal? (binary-format-ref "K") '("K" 8 1))
    ; 读取ASCII字符
    (check-equal? (binary-format-ref "A") '("A" 1 1))
    ; 读取单精度浮点数
    (check-equal? (binary-format-ref "E") '("E" 4 1))
    ; 读取双精度浮点数
    (check-equal? (binary-format-ref "D") '("D" 8 1))
    ; 读取单精度复数
    (check-equal? (binary-format-ref "C") '("C" 8 1))
    ; 读取双精度复数
    (check-equal? (binary-format-ref "M") '("M" 16 1))
    ; 读取32位数组描述符
    (check-equal? (binary-format-ref "1P") '("P" 8 1))
    (check-equal? (binary-format-ref "P") '("P" 8 1))
    (check-equal? (binary-format-ref "0P") '("P" 0 0))
    ; 读取64位数组描述符
    (check-equal? (binary-format-ref "Q") '("Q" 16 1))))

;   从TFROMS属性中总结出一行上各字段的长度
(: field-offset (-> (HashTable Integer String) (Vectorof (List String Integer Integer))))
(define (field-offset tform)
  (let* ([_ref_map : (HashTable Integer (List String Integer Integer))
          (hash-map/copy
           tform
           (lambda ([k : Integer] [v : String])
             (values k (binary-format-ref v))))]
         [_ref_list
          (map (lambda ([v : (Pairof Integer (List String Integer Integer))]) (cdr v))
               (hash->list _ref_map))])
    (list->vector _ref_list)))

; 将字节串转换为Racket类型
(: format-bytes-value (-> String Bytes Table-Element))
(define (format-bytes-value type data)
  (match type
    ["L" ; 逻辑/布尔值
     (cond
       [(bytes=? data #"\0") #f]
       [(bytes=? data #"\1") #t]
       [else (error (string-append "Unknowen logical value: " (bytes->string/utf-8 data)))])]
    ; 位值
    ["X" data]
    ; 无符号字节
    ["B" data]
    ; 16位整数
    ["I" (integer-bytes->integer data #t)]
    ; 32位整数
    ["J" (integer-bytes->integer data #t)]
    ; 64位整数
    ["K" (integer-bytes->integer data #t)]
    ; ASCII字符
    ["A" (bytes->string/utf-8 data)]
    ; 单精度浮点数
    ["E" (floating-point-bytes->real data)]
    ; 双精度浮点数
    ["D" (floating-point-bytes->real data)]
    ; 单精度复数
    ["C" (+ (floating-point-bytes->real data #f 0 4) (* 0+1.0i (floating-point-bytes->real data #f 4 8)))]
    ; 双精度复数
    ["M" (+ (floating-point-bytes->real data #f 0 8) (* 0+1.0i (floating-point-bytes->real data #f 8 16)))]))

(module+ test
  (test-case
   "format-bytes-value-test"
   ; 测试逻辑值
   (check-equal? (format-bytes-value "L" #"\0") #f)
   (check-equal? (format-bytes-value "L" #"\1") #t)
   (check-exn exn:fail? (lambda () (format-bytes-value "L" #"\114")))
   ; 测试复数
   (check-equal?
    (format-bytes-value "C" (bytes-append (real->floating-point-bytes 1.0 4) (real->floating-point-bytes 0.0 4)))
    1.0+0.0i)
   (check-equal?
    (format-bytes-value "M" (bytes-append (real->floating-point-bytes 1.0 8) (real->floating-point-bytes 0.0 8)))
    1.0+0.0i)))

;   从对齐区块中读取数据矩阵, 所需参数将在build-binary-table中清洗出来
(: load-table-from 
   (-> Input-Port (Vectorof (List String Integer Integer)) Integer Integer
       (Matrix (Listof Table-Element))))
(define (load-table-from p tform naxis1 naxis2)
  (let ([byte_table
         (subbytes
          (assert (read-bytes-aligned p (* naxis1 naxis2)) bytes?)
          0 (* naxis1 naxis2))]
        [split-fields
         : (-> Bytes (Pairof Number (Array (Listof Table-Element))))
         (lambda (row)
           (foldl (lambda ([idx : Integer] [offset_res : (Pairof Integer (Array (Listof Table-Element)))])
                    (let* ([id_type (assert (list-ref (vector-ref tform idx) 0) string?)]
                           [id_len (assert (list-ref (vector-ref tform idx) 1) exact-integer?)]
                           [id_count (assert (list-ref (vector-ref tform idx) 2) exact-integer?)]
                           [field_bytes (subbytes row (car offset_res) (+ (car offset_res) id_len))]
                           [field_bytevals
                            (if (string=? "X" id_type)
                                '() ; 位串有自己的解析方式, 见下个定义
                                (map (lambda ([i : Integer])
                                       (subbytes field_bytes (* i (quotient id_len id_count)) (* (add1 i) (quotient id_len id_count))))
                                     (range 0 id_count)))]
                           [tbit_field_vals
                            (if (string=? "X" id_type)
                                (take
                                 (apply append
                                        (map (lambda ([intb : Integer])
                                               (map (lambda ([i : Integer])
                                                      (bitwise-bit-field intb i (add1 i)))
                                                    (reverse (range 0 (integer-length intb)))))
                                             (bytes->list field_bytes)))
                                 id_count)
                                '())])
                      (if (string=? "X" id_type)
                          (cons (+ (car offset_res) id_len)
                                (array-append* (list (cdr offset_res) (array #[field_bytevals]))))
                          (cons (+ (car offset_res) id_len)
                                (array-append*
                                 (list (cdr offset_res)
                                       (array
                                        #[(map (lambda ([v : Bytes])
                                                 (format-bytes-value id_type v))
                                               field_bytevals)])))))))
                  (cons 0 (ann (array #[]) (Array (Listof Table-Element))))
                  (range 0 (vector-length tform))))]
        [_row_set? : Boolean #f]
        [_row_id! : Integer 0]
        [_row_fields! : (Array (Listof Table-Element)) (array #[])])
    (for/matrix naxis2 (vector-length tform)
      ([ix (in-range (* naxis2 (vector-length tform)))])
      : (Listof Table-Element)
      (define row_ix (quotient ix (vector-length tform)))
      (define col_ix (remainder ix (vector-length tform)))
      (unless _row_set?
        (set! _row_set? #t)
        (set! _row_id! 0)
        (set! _row_fields! (cdr (split-fields (subbytes byte_table (* row_ix naxis1) (* (add1 row_ix) naxis1))))))
      (when (= (sub1 row_ix) _row_id!)
        (set! _row_id! row_ix)
        (set! _row_fields! (cdr (split-fields (subbytes byte_table (* row_ix naxis1) (* (add1 row_ix) naxis1))))))
      (array-ref _row_fields! (vector col_ix)))))
;
;;  ---------------

;;  -----对外接口函数-----
;;  从对齐区块input-port的字节流中构建二进制表
(: build-binary-table (-> Input-Port Header-Attr binary-table))
(define (build-binary-table p at)
  (let* ([naxis1 (assert (attr-val (hash-ref at "NAXIS1")) exact-nonnegative-integer?)]
         [naxis2 (assert (attr-val (hash-ref at "NAXIS2")) exact-nonnegative-integer?)]
         [tfield (assert (attr-val (hash-ref at "TFIELDS")) exact-integer?)]
         [tform
          (hash-map/copy
           (take-fields-named at tfield "TFORM" #t)
           (lambda ([k : Integer] [v : attr]) : (Values Integer String)
             (values k (assert (attr-val v) string?))))]
         [ttype_map : (HashTable String Nonnegative-Integer)
          (hash-map/copy
           (take-fields-named at tfield "TTYPE")
           (lambda ([k : Integer] [v : attr]) : (Values String Nonnegative-Integer)
             (values (cast (attr-val v) String)
                     (assert k nonnegative-integer?))))])
    (binary-table
     (field-offset tform)
     (cons naxis1 naxis2)
     ttype_map
     (load-table-from p (field-offset tform) naxis1 naxis2))))

;;  读取操作
;   除了用TTYPE别名读取的操作外的其他读取操作直接使用matrix库的方案

(: read-field (-> binary-table String (Matrix (Listof Table-Element))))
(define (read-field bt field)
  (let ([idx (hash-ref (binary-table-mapping bt) field)])
    (matrix-col (binary-table-data bt) (sub1 idx))))

;; 写入操作
;  目前只支持对数据矩阵整体更改, 更多操作有待实现

(: write-data (-> binary-table (Matrix (Listof Table-Element)) binary-table))
(define (write-data bt nd)
  (struct-copy binary-table bt [data nd]))

;; 更新操作
;  目前只支持对数据矩阵整体更改, 更多操作有待实现

(: update-data
   (-> binary-table
       (-> (Matrix (Listof Table-Element)) (Matrix (Listof Table-Element))) binary-table))
(define (update-data bt fn)
  (struct-copy binary-table bt [data (fn (binary-table-data bt))]))
