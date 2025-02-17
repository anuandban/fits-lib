#lang racket
(require rackunit)

;;
;; 契约类型
;;
;; 属性 契约类型
(define attr?
  (hash/c (string-len/c 9) ; 属性名
          (cons/c (or/c (string-len/c 81) real? boolean? complex? 'undefined) ; 属性值
                  string?))) ; 属性注释

;; 拓展 契约类型
;; 以下类型需要全部改写
;; 拓展表格式的储存将改为使用列类型对应的偏移量从二进制字节中直接读出

(define image-table?
  ; 图像拓展格式
  (and/c (list/c (symbols 'IMAGE) ; 标识
                 (or 8 16 32 64 -32 -64) ; BITPIX关键字确定的最小数据单元大小
                 (integer-in 0 999) ; NAXIS关键字确定的维度数量
                 (vectorof nonnegative-integer?) ; NAXISn关键字确定的各个维度的容量大小
                 hash? ; 额外的和信息段有关的属性
                 (listof number?)) ; 数据单元数组，每个单元的大小范围由BITPIX关键字决定
         (lambda (l) ; 验证NAXIS的值和NAXISn的数量是否一致
           (= (list-ref l 2) (vector-length (list-ref l 3))))
         (lambda (l) ; 验证数据单元大小和BITPIX确定的值是否一致
           (= (list-ref l 1) (list-ref l 4)))))

(define ascii-table?
  ; ASCII字符表格式
  (and/c (list/c (symbols 'TABLE) ; 标识
                 8 ; BITPIX关键字确定的最小数据单元大小
                 2 ; NAXIS关键字确定的行和列容量
                 nonnegative-integer? ; (字符/字节) NAXIS1确定每个行容纳的ASCII字符数量
                 nonnegative-integer? ; (字符/字节) NAXIS2确定的列高度容量
                 nonnegative-integer? ; TFIELDS确定一行内有多少字段
                 ; TBCOLn确定的各个字段在行内开始的字符位置，TFORMn确定的字段值的形式
                 (hash/c nonnegative-integer? (string-len/c 9))
                 hash? ; 额外的和字段有关的属性
                 (listof (listof bytes?))) ; 二维字符表数组
         (lambda (l) ; 验证字段数量和TFIELDS确定的值是否一致
           (= (list-ref l 5) (length (list-ref l 6))))))

(define binary-table?
  ; 二进制表格式
  (and/c (list/c (symbols 'BINTABLE) ; 标识
                 8 ; BITPIX关键字确定的最小数据单元大小
                 2 ; NAXIS关键字确定的行和列容量
                 nonnegative-integer? ; (字节) NAXIS1确定的行宽度容量
                 nonnegative-integer? ; (字节) NAXIS2确定的列高度容量
                 nonnegative-integer? ; TFIELDS确定一行内的字段数量
                 ; TFORMn确定的各个字段的表达格式，和具体格式的映射关系参考binary-format-ref函数
                 (hash/c nonnegative-integer? (string-len/c 9))
                 hash? ; 额外的和字段有关的属性
                 ; 数据单元二维矩阵，每个单元的大小范围由TFORM关键字决定
                 (listof (listof any/c)))
         (lambda (l) ; 验证数据单元大小和TFIELDS确定的值是否一致
           (= (list-ref l 5) (hash-count (list-ref l 6))))))

(define ext-table? (or/c image-table? ascii-table? binary-table? '()))

(define fits? (listof (cons/c attr? ext-table?)))

;;
;; Header and Data Unit, each HDU contains a number of blocks(2880 bytes)
;;
;; TODO: 完善针对藏在注释内单位的特殊操作

;; 属性解析部分

(define (pair-filter context re_val re_val_box re_c)
  ; 用正则表达式切割值和注释的工具函数
  ; 返回的pair中值类型依旧是字符串，所以还需要进一步清洗
  (let* ([pos (car (regexp-match-positions re_val context))]
         [val_part (substring context (car pos) (cdr pos))]
         [comment_part (substring context (cdr pos))])
    (cons (string-trim val_part re_val_box) (string-trim (string-trim comment_part re_c) #rx" *$"))))

(module+ test 
  (test-case "pair-filter-test"
    ;; 测试pair-filter函数
    ; 读取带注释的实数
    (check-equal? (pair-filter "  1.0 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* */"
                                #rx" *| */$"
                                #rx"^ ")
                  '("1.0" . "1.0"))
    ; 读取带注释的字符串
    (check-equal? (pair-filter "  '1.0' / 1.0" #rx"^ *'[^']*' */"
                                #rx" *'|' */"
                                #rx"^ ")
                  '("1.0" . "1.0"))
    ; 读取带注释的布尔值
    (check-equal? (pair-filter "  T / 1.0" #rx"^ *[TF] */"
                                #rx" *| */$"
                                #rx"^ ")
                  '("T" . "1.0"))
    ; 读取带注释的科学计数法实数
    (check-equal?
     (pair-filter "  1.0E1 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"
                  #rx" *| */$"
                  #rx"^ ")
     '("1.0E1" . "1.0"))
    (check-equal?
     (pair-filter "  1.0D1 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"
                  #rx" *| */$"
                  #rx"^ ")
     '("1.0D1" . "1.0"))
    (check-equal?
     (pair-filter "  1.0E-1 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"
                  #rx" *| */$"
                  #rx"^ ")
     '("1.0E-1" . "1.0"))
    (check-equal?
     (pair-filter "  1.0D+1 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"
                  #rx" *| */$"
                  #rx"^ ")
     '("1.0D+1" . "1.0"))))



(define/contract (split-val-comment context)
  (-> string?
      (cons/c (or/c string? number? boolean? 'undefined) string?))
  ; 正则表达式捕获值类型
  ; 注意这个捕获会把含有/的注释头包裹进去
  (let ([re_str #rx"^ *'[^']*' *$"]
        [re_str_c #rx"^ *'[^']*' */"]
        ;
        [bool-format (lambda (p)
                       (cons (match (car p)
                               ["T" #t]
                               ["F" #f])
                             (cdr p)))]
        [re_bool #rx"^ *[TF] *$"]
        [re_bool_c #rx"^ *[TF] */"]
        ;
        [num-format (lambda (p)
                      (cons (string->number (string-replace (string-trim (car p)) "D" "E"))
                            ; racket内置函数只支持解析E中缀科学计数法，FITS中支持的D中缀将被转换为E
                            ; D中缀表示该数字精度比32位浮点数精度上限更高，不过值类型和E中缀是一样的
                            (cdr p)))]
        [re_purenum #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* *$"]
        [re_purenum_c #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* */"]
        [re_exponum #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ *$"]
        [re_exponum_c #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"]
        ;
        [complex-format (lambda (p)
                          (cons (let* ([p1 (string-trim (car p))]
                                       [p2 (string-split p1 #px", *")])
                                  (+ (string->number (string-trim (car p2)))
                                     (* 0+1i (string->number (string-trim (cadr p2))))))
                                (cdr p)))]
        [re_complex_int_c_p #px"^ *\\( *([-]?[0-9]+ *, *[-]?[0-9]+ *) *\\) *(/.*)?"]
        [re_complex_float_c_p
         #px"^ *\\( *([-]?[0-9]+\\.[0-9]+ *, *[-]?[0-9]+\\.[0-9]+ *) *\\) *(/.*)?"]
        ;
        [re_blank_c #px"^ *$|^ */(.*)$"])
    (cond
      ; 字符串
      [(regexp-match? re_str context) (cons (string-trim context #rx" *'|' *$") "")]
      ; 字符串/注释
      [(regexp-match? re_str_c context) (pair-filter context re_str_c #rx" *'|' */" #rx"^ ")]
      ; 布尔值
      [(regexp-match? re_bool context) (bool-format (cons (string-trim context) ""))]
      ; 布尔值/注释
      [(regexp-match? re_bool_c context)
       (bool-format (pair-filter context re_bool_c #rx" *| */$" #rx"^ "))]
      ; 纯数字&指数
      [(or (regexp-match? re_purenum context) (regexp-match? re_exponum context))
       (num-format (cons context ""))]
      ; 纯数字&指数/注释
      [(regexp-match? re_purenum_c context)
       (num-format (pair-filter context re_purenum_c #rx" *| */$" #rx"^ "))]
      [(regexp-match? re_exponum_c context)
       (num-format (pair-filter context re_exponum_c #rx" *| */$" #rx"^ "))]
      ; 整数复数/可选注释
      [(regexp-match? re_complex_int_c_p context)
       (complex-format (let* ([rg (regexp-match re_complex_int_c_p context)]
                              [val (cadr rg)]
                              [comment (caddr rg)])
                         (cons val (if comment (string-trim comment #px"^/ *| *$") ""))))]
      ; 浮点数复数/可选注释
      [(regexp-match? re_complex_float_c_p context)
       (complex-format (let* ([rg (regexp-match re_complex_float_c_p context)]
                              [val (cadr rg)]
                              [comment (caddr rg)])
                         (cons val (if comment (string-trim comment #px"^/ *| *$") ""))))]
      ; 未定义
      [(regexp-match? re_blank_c context)
       (cons 'undefined
             (let ([c (cdr (regexp-match re_blank_c context))])
               (if (car c)
                   (string-trim (car c))
                   "")))]
      ; 无法识别，错误
      [else (error 'syntax)])))

(module+ test
  (test-case "split-val-comment-test"
    ;; 测试split-val-comment函数
    ; 读取字符串
    (check-equal? (split-val-comment "  '1.0' / 1.0") '("1.0" . "1.0"))
    (check-equal? (split-val-comment "  '1.0abc' ") '("1.0abc" . ""))
    ; 读取布尔值
    (check-equal? (split-val-comment "  T / 1.0") '(#t . "1.0"))
    (check-equal? (split-val-comment "  F / 1.0") '(#f . "1.0"))
    (check-equal? (split-val-comment "  T ") '(#t . ""))
    (check-equal? (split-val-comment "  F ") '(#f . ""))
    ; 读取实数
    (check-equal? (split-val-comment "  1.0 / 1.0") '(1.0 . "1.0"))
    (check-equal? (split-val-comment "  -1.0 / 1.0") '(-1.0 . "1.0"))
    (check-equal? (split-val-comment "  1 / 1.0") '(1 . "1.0"))
    (check-equal? (split-val-comment "  -1 / 1.0") '(-1 . "1.0"))
    (check-equal? (split-val-comment "  1.0 ") '(1.0 . ""))
    (check-equal? (split-val-comment "  -1.0 ") '(-1.0 . ""))
    ; 读取科学计数法实数
    (check-equal? (split-val-comment "  1234567890.0423E1 / 1.0") '(12345678900.423 . "1.0"))
    (check-equal? (split-val-comment "  1234567890.0432D0 / 1.0") '(1234567890.0432 . "1.0"))
    (check-equal? (split-val-comment "  1.0E-1 / 1.0") '(0.1 . "1.0"))
    (check-equal? (split-val-comment "  1.0D+1 / 1.0") '(10.0 . "1.0"))
    (check-equal? (split-val-comment "  1.0D-1 ") '(0.1 . ""))
    (check-equal? (split-val-comment "  1.0E+1 ") '(10.0 . ""))
    ; 读取整数复数
    (check-equal? (split-val-comment "  (1, 1) / 1.0") '(1+1i . "1.0"))
    (check-equal? (split-val-comment "  (-1, -1) / 1.0") '(-1-1i . "1.0"))
    (check-equal? (split-val-comment "  (1, -1) ") '(1-1i . ""))
    (check-equal? (split-val-comment "  (1, 1) ") '(1+1i . ""))
    ; 读取浮点数复数
    (check-equal? (split-val-comment "  (1.0, 1.0) / 1.0") '(1.0+1.0i . "1.0"))
    (check-equal? (split-val-comment "  (1.0, -1.0) ") '(1.0-1.0i . ""))
    ; 读取未定义
    (check-equal? (split-val-comment "  / 1.0") '(undefined . "1.0"))
    (check-equal? (split-val-comment "  /") '(undefined . ""))
    (check-equal? (split-val-comment "  ")'(undefined . ""))))



(define (dispense-attr attr key_str op context)
  (let ([p (split-val-comment context)]
        [key (string-trim key_str)])
    (cons (cons key p) attr)))

(define (dispense-continue attr context)
  (let* ([p (split-val-comment context)]
         [val (car p)]
         [comment (cdr p)]
         [last_val (car (cdar attr))]
         [last_comment (cdr (cdar attr))])
    ;; TODO: 针对不正确的CONTINUE格式抛出错误
    (cons (cons (caar attr)
                (cons (string-append (string-trim last_val #px"&$") val)
                      (string-append last_comment comment)))
          attr)))

(define/contract (dispense-state lines offset_block [attr '()] [id_line 1])
  (->* ((listof string?) nonnegative-integer?) (list? nonnegative-integer?)
       (cons/c (or/c 'continue 'end) list?))
  ; 输入的lines已经被倒序排列，以便car和cdr操作
  ; 考虑到一次读块的条目数量不会超过2880/80=36条，递归耗尽堆栈的问题几乎不可能出现，这里直接递归搞状态机
  ; 如果你的堆栈被耗尽了，说明你该换台电脑了
  (with-handlers ([exn:fail? (lambda (exn) (cons exn (+ id_line (* offset_block 36))))])
    (if (empty? lines)
        (cons 'continue attr)
        (let* ([next_l (car lines)]
               [key (substring next_l 0 8)]
               [op (substring next_l 8 10)]
               [context (substring next_l 10)])
          (match key
            ["CONTINUE"
             (dispense-state (cdr lines) offset_block (dispense-continue attr context) (+ id_line 1))]
            ["END     " (cons 'end attr)]
            ["        " (dispense-state (cdr lines) offset_block attr (+ id_line 1))]
            ["COMMENT " (dispense-state (cdr lines) offset_block attr (+ id_line 1))] ; TODO
            ["HISTORY " (dispense-state (cdr lines) offset_block attr (+ id_line 1))] ; TODO
            [else
             (dispense-state (cdr lines)
                             offset_block
                             (dispense-attr attr key op context)
                             (+ id_line 1))])))))



;; 拓展（Extension）解析部分
;; FITS支持三种扩展：图像（IMAGE___），ASCII字符（TABLE___），二进制表（BINTABLE）

;;; 工具函数

(define/contract (binary-format-ref f)
  (-> string? (list/c string? nonnegative-integer? nonnegative-integer?))
  ; 根据f的内容确认其映射的长度是多少，注意单位是字节
  ; 一个字段可以包含多个值，因此返回的还包括值的数量
  (let (; 匹配逻辑，比特，字节，16/32/64位整数，单双精度浮点数和复数
        [retf1 #px"^(0?|[1-9]*)([LXBIJKAEDCM])(.?) *"]
        ; 匹配32/64位数组描述符
        [retf2 #px"^([01]?)([PQ])(.?) *"])
    (cond
      [(regexp-match? retf1 f)
       (let* ([grp (regexp-match retf1 f)]
              [c (if (string=? "" (second grp))
                     1
                     (string->number (second grp)))]
              [b (match (third grp)
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
                   ["M" 16])] ; 双精度复数
              [_ (fourth grp)]) ; 标准未定义的一个字符
         (list (third grp)
               (* b c)
               c))]
      [(regexp-match? retf2 f)
       (let* ([grp (regexp-match retf2 f)]
              [c (if (string=? "" (second grp))
                     1
                     (string->number (second grp)))]
              [b (match (third grp)
                   ["P" 8] ; 32位数组描述符
                   ["Q" 16] ; 64位数组描述符
                   )]
              [_ (fourth grp)]) ; 标准未定义的一个字符
         (list (third grp) (* b c) c))]
      [else (error "Wrong TFORM data types")])))

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



(define (read-ext-table p bytes_size)
  ; 以块为单位读取出一片连续的字节，用于读取拓展表
  (letrec ([bfn (lambda (s [acc 0])
                  (if (<= s 2880)
                      (add1 acc)
                      (bfn (- s 2880) (add1 acc))))]
           [bn (bfn bytes_size)])
    (read-bytes (* 2880 bn) p)))

(define (has-attr? attr keys)
  (foldr (lambda (x y) (and x y)) #t (map (lambda (k) (hash-has-key? attr k)) keys)))

(define (has-attr-match? attr key_form)
  (foldr (lambda (x y) (or x y))
         #f
         (map (lambda (k) (regexp-match? (pregexp (string-append "^" key_form "[1-9][0-9]*$")) k))
              (hash-keys attr))))

(define (match-count? attr key_count key_form)
  ; 检查某类属性是否有足够的数量
  (let ([req_kn (car (hash-ref attr key_count))]
        [kn (length (filter (lambda (s)
                              (regexp-match? (pregexp (string-append "^" key_form "[1-9][0-9]*$")) s))
                            (hash-keys attr)))])
    (= req_kn kn)))

(define/contract (take-fields-named attr c key_form [continued? #f])
  (->* (attr? nonnegative-integer? string?) (boolean?) (hash/c nonnegative-integer? any/c))
  ; 提取一类属性出来，这类属性序数最大值不超过c，不然会报错
  ; continued?开启时，强制要求属性序数必须覆盖完1~c的所有范围，出现缺失则报错
  ; 注意返回值是个以序数为其键值的哈希表
  (let* ([re_catch (pregexp (string-append "^" key_form "([1-9][0-9]*$)"))]
         [ats (filter (lambda (s) (regexp-match? re_catch s)) (hash-keys attr))]
         [ats_pair (map (lambda (s)
                          (let ([p (regexp-match re_catch s)])
                            (cons (car p) (string->number (cadr p)))))
                        ats)]
         [ats_hash (foldr (lambda (p h)
                            (if (hash-has-key? h (car p))
                                (error (string-append "repeat attr: " (car p)))
                                (hash-set h (car p) (cdr p))))
                          #hash()
                          ats_pair)]
         [ids_hash (hash-map/copy ats_hash
                                  (lambda (k n)
                                    (if (<= n c)
                                        (values n (hash-ref attr k)) ; 默认调用函数者知道原来属性名，因此抛弃属性名只保留序数
                                        (error (string-append "Id number overflow: " key_form)))))])
    (if continued?
        (if (= c (hash-count ids_hash))
            ids_hash
            (error (string-append "Not match count: " key_form)))
        ids_hash)))

(module+ test
  (test-case
   "take-fields-named-test"
   ;; 测试take-fields-named函数
   (check-equal?
    (take-fields-named
     (hash "TFORM1" '("1I" . "")
           "TFORM2" '("1J" . "")
           "TFORM3" '("1K" . ""))
     3 "TFORM")
    (hash 1 '("1I" . "")
          2 '("1J" . "")
          3 '("1K" . "")))
   ; 让它报错
   (check-exn
    exn:fail?
    (lambda ()
      (take-fields-named
       (hash "TFORM1" '("1I" . "")
             "TFORM2" '("1J" . "")
             "TFORM3" '("1K" . ""))
       4 "TFORM" #t))))
  
  ;; 测试用文件
  (define tess (open-input-file "../test/tess2024249191853-s0083-0000010001363713-0280-s_lc.fits"))
  (define lamost (open-input-file "../test/dr10_v2.0_LRS_catalogue.fits"))
  ;; 接下来的测试将使用这两个文件作为基准
  (test-case
   "take-fields-named-test-file"
   ; TODO
   ))



(define (format-binary-value type data)
  ; 将字节串转换为Racket类型
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
   "format-binary-value-test"
   ; 测试逻辑值
   (check-equal? (format-binary-value "L" #"\0") #f)
   (check-equal? (format-binary-value "L" #"\1") #t)
   (check-exn exn:fail? (lambda () (format-binary-value "L" #"\114")))
   ; 测试复数
   (check-equal?
    (format-binary-value "C" (bytes-append (real->floating-point-bytes 1.0 4) (real->floating-point-bytes 0.0 4)))
    1.0+0.0i)
   (check-equal?
    (format-binary-value "M" (bytes-append (real->floating-point-bytes 1.0 8) (real->floating-point-bytes 0.0 8)))
    1.0+0.0i)))


;; TODO: DEL
(define/contract (format-binary-row tforms data)
  ; 从tforms规定一行的格式中读取出一行属性表
  ; TODO: 用Typed Racket重写时记得使用Math库的矩阵作替换
  (-> (hash/c nonnegative-integer? string?) bytes? (cons/c (listof (listof any/c)) bytes?))
  (let ([fgrp (foldl (lambda (k lod)
                       (let* ([tlc (binary-format-ref (hash-ref tforms k))]
                              ; 新字节偏移量
                              ;[offset (+ (second tlc) (second lod))]
                              ; 字段类型单个值的大小, 单位为字节
                              [si (if (= 0 (second tlc))
                                      #f
                                      (/ (second tlc) (third tlc)))]
                              ; 字段整体
                              [fi (subbytes (third lod) 0 (second tlc))]
                              ; 字段内所有值（如果字段不为空）
                              [fi_vals (if (= 0 (bytes-length fi))
                                           '()
                                           (foldl (lambda (i l)
                                                    (cons (subbytes fi (* i si) (* (add1 i) si)) l))
                                                  '()
                                                  (range 0 (third tlc))))]
                              ; 剩余字节
                              [nd (subbytes (third lod) (second tlc))])
                         (list (cons (map (lambda (v) (format-binary-value (first tlc) v)) fi_vals)
                                     (first lod))
                               0
                               nd)))
                     ; 属性表，字节偏移量，剩余列表
                     (list '() 0 data)
                     (range 1 (add1 (hash-count tforms))))])
    (cons (first fgrp) (third fgrp))))

;;; 读取函数

(define/contract (disp-read-img p attr)
  (-> input-port? attr? image-table?)
  '()) ; TODO

(define/contract (disp-read-ascii p attr)
  (-> input-port? attr? ascii-table?)
  '()) ; TODO

;; TODO: REWRITE
(define/contract (disp-read-bin p attr)
  (-> input-port? attr? binary-table?)
  ; 硬性检查，以下这些属性是二进制表必备的属性，没有会引起报错
  (if (and (has-attr? attr
                      '("XTENSION" "BITPIX" "NAXIS" "NAXIS1" "NAXIS2" "PCOUNT" "GCOUNT" "TFIELDS"))
           ; 检查TFORM数量和TFIELDS规定的数字是否一致
           (match-count? attr "TFIELDS" "TFORM")
           ; 检查BITPIX和NAXIS关键字和预设是否一致
           (= 8 (car (hash-ref attr "BITPIX")))
           (= 2 (car (hash-ref attr "NAXIS"))))
      ; 错误的文件可能把NAXIS1,2写成其他类型，这里我们暂时不管它，交给contract来报错
      (let* ([naxis1 (car (hash-ref attr "NAXIS1"))] ; 一行的字段量
             [naxis2 (car (hash-ref attr "NAXIS2"))] ; 一张表有多少行
             [tfields (car (hash-ref attr "TFIELDS"))] ; 字段数量
             ; BINTABLE各个字段的格式(和可选属性不同, 注释已经被清洗掉了)
             [tform (hash-map/copy (take-fields-named attr tfields "TFORM" #t)
                                   (lambda (k v) (values k (car v))))]
             ; BINTABLE各个字段的可选属性
             [unmap_other_tkey (filter (lambda (k) (has-attr-match? attr k))
                                       '("TTYPE" "TUNIT" "TSCAL" "TZERO" "TNULL" "TDISP" 
                                                 "THEAP" "TDIM" "TDMIN" "TDMAX" "TLMIN" "TLMAX"))]
             [other_tkey ; 从以上可选属性中找出那些存在的属性记录下来
              (map (lambda (k) (cons k (take-fields-named attr tfields k))) unmap_other_tkey)]
             ; BINTABLE一行长多少字节
             [row_len (foldr (lambda (tlc l) (+ (second tlc) l))
                             0
                             (map (lambda (tf) (binary-format-ref tf)) (hash-values tform #t)))]
             ; 从port里读出一片已经对齐的字节串，这就是要解析的表格
             [bintable (read-ext-table p (* row_len naxis2))]
             [unt_bintable ; 未转置的二进制表（行，列），抛弃对齐字节串填补字节
              (reverse (car (foldr (lambda (_ btl)
                                     (let ([p_row_bt (format-binary-row tform (cdr btl))])
                                       (cons (cons (car p_row_bt) (car btl)) (cdr p_row_bt))))
                                   (cons '() bintable)
                                   (range 0 naxis2))))]
             [format_bintable ; 最终二进制表（列，行）
              (foldr (lambda (col l) (cons (map (λ (row) (list-ref row col)) unt_bintable) l))
                     '()
                     (range 0 tfields))])
        (list 'BINTABLE 8 2 naxis1 naxis2 tfields tform (make-hash other_tkey) format_bintable))
      (error "Bad syntax")))

(module+ test
  (test-case
   "disp-read-bin-test"
   ; TODO
   (check-eq? 44 (hash-count (car (read-header tess))))
   (let* ([attr (car (read-header tess))]
          [table (disp-read-bin tess attr)]
          [outfile (open-output-file "../test/test.txt" #:exists 'replace)])
     (pretty-print table outfile))
   (pretty-print (read-header tess))
   (check-equal? 1 1)))

;; Header 解析部分

(define (attr-line attr_str)
  (letrec ([take-line (lambda (l rest_str)
                        (if (string=? rest_str "")
                            l
                            (take-line (cons (substring rest_str 0 80) l) (substring rest_str 80))))])
    (reverse (take-line '() attr_str))))

(define/contract (read-header p)
  (-> input-port? (cons/c attr? input-port?))
  (letrec ([read-block (lambda (attr rest_part)
                         (let* ([attr_str (bytes->string/utf-8 (read-bytes 2880 rest_part))]
                                [attr_block (dispense-state (attr-line attr_str) 0)])
                           (case (car attr_block)
                             ['continue (read-block (append (cdr attr_block) attr) rest_part)]
                             ['end (cons (make-hash (reverse (append (cdr attr_block) attr))) rest_part)])))])
    (read-block '() p)))

(module+ test
  (check-eq? 6 (hash-count (car (read-header lamost)))))

;; TODO: 临时读取hdu的函数，没有对tess文件外的格式的读取方法，需要补充
(define/contract (read-data p unmap_h)
  (-> input-port? list? (cons/c attr? ext-table?))
  (let* ([kv_h (foldr (lambda (np nh) (hash-set nh (car np) (cdr np))) #hash() unmap_h)]
         [t (cond
              [(hash-has-key? kv_h "XTENSION") (hash-ref kv_h "XTENSION")]
              [(hash-has-key? kv_h "SIMPLE") "SIMPLE"]
              [else (error "Unknown header")])])
    (match t
      ["SIMPLE" (cons kv_h '())]
      ["BINTABLE" (cons kv_h (disp-read-bin p kv_h))]
      ["IMAGE" (cons kv_h '())]
      ["TABLE" (cons kv_h '())]
      [else (error "Unknown header")])))

;; FITS主体结构

(define empty-fits '())

(define (build-fits fits_port)
  '())

;; 对外读写接口

(define (load-fits p)
  '())

(define (hdu->list f)
  1)

(define (hdu-attr f idx)
  1)

(define (hdu-data f idx)
  1)
