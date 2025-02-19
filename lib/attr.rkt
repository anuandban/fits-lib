#lang typed/racket
(require typed/rackunit
         "./util.rkt")

(provide attr
         attr-val
         attr-comment
         Header-Attr
         AttrValue
         take-fields-named)

(define-type AttrValue (U Boolean String Number 'undefined))

(struct attr ([val : AttrValue] [comment : String]) #:mutable #:prefab)

(define-type Header-Attr (HashTable String attr))

;;  -----解析原始字节-----

;;  错误类型
(define-type ErrorMsg (Pairof Any Integer))

;   用正则表达式切割值和注释的工具函数
;   返回的pair中值类型依旧是字符串，所以还需要进一步清洗
(: pair-filter (-> String Regexp Regexp Regexp (Pairof String String)))
(define (pair-filter context re_val re_val_box re_c)
  (let* ([pos (car (assert (regexp-match-positions re_val context) pair?))]
         [val_part (substring context (car pos) (cdr pos))]
         [comment_part (substring context (cdr pos))])
    (cons (string-trim val_part re_val_box) (string-trim (string-trim comment_part re_c) #rx" *$"))))

(module+ test 
  (test-case "pair-filter-test"
    ;; 测试pair-filter函数
    ; 读取带注释的实数
    (check-equal? 
     (pair-filter "  1.0 / 1.0" #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* */" 
                  #rx" *| */$"
                  #rx"^ ")
     '("1.0" . "1.0"))
    ; 读取带注释的字符串
    (check-equal?
     (pair-filter "  '1.0' / 1.0" #rx"^ *'[^']*' */"
                  #rx" *'|' */"
                  #rx"^ ")
     '("1.0" . "1.0"))
    ; 读取带注释的布尔值
    (check-equal?
     (pair-filter "  T / 1.0" #rx"^ *[TF] */"
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

;   正则表达式捕获值类型
;  注意这个捕获会把含有/的注释头包裹进去
(: split-val-comment (-> String attr))
(define (split-val-comment context)
  (let ([re_str #rx"^ *'[^']*' *$"]
        [re_str_c #rx"^ *'[^']*' */"]
        ;
        [bool-format
         : (-> (Pairof String String) attr)
         (lambda (p)
           (attr (match (car p)
                   ["T" #t]
                   ["F" #f])
                 (cdr p)))]
        [re_bool #rx"^ *[TF] *$"]
        [re_bool_c #rx"^ *[TF] */"]
        ;
        [num-format
         : (-> (Pairof String String) attr)
         (lambda (p)
           (attr (string->number (string-replace (string-trim (car p)) "D" "E"))
                 ; racket内置函数只支持解析E中缀科学计数法，FITS中支持的D中缀将被转换为E
                 ; D中缀表示该数字精度比32位浮点数精度上限更高，不过值类型和E中缀是一样的
                 (cdr p)))]
        [re_purenum #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* *$"]
        [re_purenum_c #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]* */"]
        [re_exponum #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ *$"]
        [re_exponum_c #px"^ *[-+]{0,1}[0-9]+[.]{0,1}[0-9]*[E|D][-+]?[0-9]+ */"]
        ;
        [complex-format
         : (-> (Pairof String String) attr) 
         (lambda (p)
           (attr (let* ([p1 (string-trim (car p))]
                        [p2 (string-split p1 #px", *")])
                   (+ (assert (string->number (string-trim (car p2))) number?)
                      (* 0+1i (assert (string->number (string-trim (cadr p2))) number?))))
                 (cdr p)))]
        [re_complex_int_c_p #px"^ *\\( *([-]?[0-9]+ *, *[-]?[0-9]+ *) *\\) *(/.*)?"]
        [re_complex_float_c_p
         #px"^ *\\( *([-]?[0-9]+\\.[0-9]+ *, *[-]?[0-9]+\\.[0-9]+ *) *\\) *(/.*)?"]
        ;
        [re_blank_c #px"^ *$|^ */(.*)$"])
    (cond
      ; 字符串
      [(regexp-match? re_str context) (attr (string-trim context #rx" *'|' *$") "")]
      ; 字符串/注释
      [(regexp-match? re_str_c context)
       (let ([fp (pair-filter context re_str_c #rx" *'|' */" #rx"^ ")])
         (attr (car fp) (cdr fp)))]
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
       (complex-format (let* ([rg (assert (regexp-match re_complex_int_c_p context) pair?)]
                              [val (cast (cadr rg) String)]
                              [comment (caddr rg)])
                         (cons val (if comment (string-trim comment #px"^/ *| *$") ""))))]
      ; 浮点数复数/可选注释
      [(regexp-match? re_complex_float_c_p context)
       (complex-format (let* ([rg (assert (regexp-match re_complex_float_c_p context) pair?)]
                              [val (cast (cadr rg) String)]
                              [comment (caddr rg)])
                         (cons val (if comment (string-trim comment #px"^/ *| *$") ""))))]
      ; 未定义
      [(regexp-match? re_blank_c context)
       (attr 'undefined
             (let ([c (cdr (assert (regexp-match re_blank_c context) pair?))])
               (if (car c)
                   (string-trim (assert (car c) string?))
                   "")))]
      ; 无法识别，错误
      [else (error 'syntax)])))

(module+ test
  (test-case "split-val-comment-test"
    ;; 测试split-val-comment函数
    ; 读取字符串
    (check-equal? (split-val-comment "  '1.0' / 1.0") (attr "1.0" "1.0"))
    (check-equal? (split-val-comment "  '1.0abc' ") (attr "1.0abc" ""))
    ; 读取布尔值
    (check-equal? (split-val-comment "  T / 1.0") (attr #t "1.0"))
    (check-equal? (split-val-comment "  F / 1.0") (attr #f "1.0"))
    (check-equal? (split-val-comment "  T ") (attr #t ""))
    (check-equal? (split-val-comment "  F ") (attr #f ""))
    ; 读取实数
    (check-equal? (split-val-comment "  1.0 / 1.0") (attr 1.0 "1.0"))
    (check-equal? (split-val-comment "  -1.0 / 1.0") (attr -1.0 "1.0"))
    (check-equal? (split-val-comment "  1 / 1.0") (attr 1 "1.0"))
    (check-equal? (split-val-comment "  -1 / 1.0") (attr -1 "1.0"))
    (check-equal? (split-val-comment "  1.0 ") (attr 1.0 ""))
    (check-equal? (split-val-comment "  -1.0 ") (attr -1.0 ""))
    ; 读取科学计数法实数
    (check-equal? (split-val-comment "  1234567890.0423E1 / 1.0") (attr 12345678900.423 "1.0"))
    (check-equal? (split-val-comment "  1234567890.0432D0 / 1.0") (attr 1234567890.0432 "1.0"))
    (check-equal? (split-val-comment "  1.0E-1 / 1.0") (attr 0.1 "1.0"))
    (check-equal? (split-val-comment "  1.0D+1 / 1.0") (attr 10.0 "1.0"))
    (check-equal? (split-val-comment "  1.0D-1 ") (attr 0.1 ""))
    (check-equal? (split-val-comment "  1.0E+1 ") (attr 10.0 ""))
    ; 读取整数复数
    (check-equal? (split-val-comment "  (1, 1) / 1.0") (attr 1+1i "1.0"))
    (check-equal? (split-val-comment "  (-1, -1) / 1.0") (attr -1-1i "1.0"))
    (check-equal? (split-val-comment "  (1, -1) ") (attr 1-1i ""))
    (check-equal? (split-val-comment "  (1, 1) ") (attr 1+1i ""))
    ; 读取浮点数复数
    (check-equal? (split-val-comment "  (1.0, 1.0) / 1.0") (attr 1.0+1.0i "1.0"))
    (check-equal? (split-val-comment "  (1.0, -1.0) ") (attr 1.0-1.0i ""))
    ; 读取未定义
    (check-equal? (split-val-comment "  / 1.0") (attr 'undefined "1.0"))
    (check-equal? (split-val-comment "  /") (attr 'undefined ""))
    (check-equal? (split-val-comment "  ")(attr 'undefined ""))))

;   分派函数：解析属性行
(: dispense-attr
   (-> (Listof (Pairof String attr)) String String String
       (Listof (Pairof String attr))))
(define (dispense-attr at key_str op context)
  (let ([p (split-val-comment context)]
        [key (string-trim key_str)])
    (cons (cons key p) at)))

;   分派函数：解析跨行的字符属性行
(: dispense-continue
   (-> (Listof (Pairof String attr)) String
       (Listof (Pairof String attr))))
(define (dispense-continue at context)
  (let* ([p (split-val-comment context)]
         [val (assert (attr-val p) string?)]
         [comment (assert (attr-comment p) string?)]
         [last_val (assert (attr-val (cdar at)) string?)]
         [last_comment (assert (attr-comment (cdar at)) string?)])
    ;; TODO: 针对不正确的CONTINUE格式抛出错误
    (cons (cons (caar at)
                (attr (string-append (string-trim last_val #px"&$") val)
                      (string-append last_comment comment)))
          at)))

;   分派函数：递归解析启动入口
;   输入的lines已经被倒序排列，以便car和cdr操作
;   考虑到一次读块的条目数量不会超过2880/80=36条，递归耗尽堆栈的问题几乎不可能出现，这里直接递归搞状态机
;   如果你的堆栈被耗尽了，说明你该换台电脑了
(: dispense-state
   (->* ((Listof String) Integer)
        ((Listof (Pairof String attr)) Integer)
        (Pairof (U 'continue 'end) (Listof (Pairof String attr)))))
(define (dispense-state lines offset_block [at '()] [id_line 1])
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (error (string-append (assert exn string?)
                                           (number->string (+ id_line (* offset_block 36))))))])
    (if (empty? lines)
        (cons 'continue at)
        (let* ([next_l (car lines)]
               [key (substring next_l 0 8)]
               [op (substring next_l 8 10)]
               [context (substring next_l 10)])
          (match key
            ["CONTINUE"
             (dispense-state (cdr lines) offset_block (dispense-continue at context) (+ id_line 1))]
            ["END     " (cons 'end at)]
            ["        " (dispense-state (cdr lines) offset_block at (+ id_line 1))]
            ["COMMENT " (dispense-state (cdr lines) offset_block at (+ id_line 1))] ; TODO
            ["HISTORY " (dispense-state (cdr lines) offset_block at (+ id_line 1))] ; TODO
            [else
             (dispense-state (cdr lines)
                             offset_block
                             (dispense-attr at key op context)
                             (+ id_line 1))])))))

;   Header 解析部分

;   切割一个单元的字符串至行
(: split-attr-line (-> String (Listof String)))
(define (split-attr-line attr_str)
  (letrec ([take-line 
            : ((Listof String) String -> (Listof String))
            (lambda (l rest_str)
              (if (string=? rest_str "")
                  l
                  (take-line (cons (substring rest_str 0 80) l) (substring rest_str 80))))])
    (reverse (take-line '() attr_str))))

(: read-header (-> Input-Port Header-Attr))
(define (read-header p)
  (letrec ([read-block
            : ((Listof (Pairof String attr)) -> Header-Attr)
            (lambda (at)
              (let* ([attr_str (bytes->string/utf-8 (assert (read-blocks p 1) bytes?))]
                     [attr_block (assert (dispense-state (split-attr-line attr_str) 0) pair?)])
                (case (car attr_block)
                  ['continue (read-block (append (cdr attr_block) at))]
                  ['end (make-hash (reverse (append (cdr attr_block) at)))])))])
    (read-block '())))

;;  ---------------

;;  -----读取Attr相关-----
;   提取一类属性出来，这类属性序数最大值不超过c，不然会报错
;   continued?开启时，强制要求属性序数必须覆盖完1~c的所有范围，出现缺失则报错
;   注意返回值是个以序数为其键值的哈希表
(: take-fields-named (->* (Header-Attr Integer String) (Boolean) (HashTable Integer attr)))
(define (take-fields-named a c key_form [continued? #f])
  (let* ([re_catch (pregexp (string-append "^" key_form "([1-9][0-9]*$)"))]
         [ats (filter (lambda ([s : String]) (regexp-match? re_catch s)) (hash-keys a))]
         [ats_pair : (Listof (Pairof String Integer))
          (map
           (lambda ([s : String]) 
             (let ([p (assert (regexp-match re_catch s) pair?)])
               (cons (car p) (assert (string->number (assert (cadr p) string?)) exact-integer?))))
           ats)]
         [ats_hash : (HashTable String Integer)
          (foldr
           (lambda ([p : (Pairof String Integer)] [h : (HashTable String Integer)]) 
             (if (hash-has-key? h (car p))
                 (error (string-append "repeat attr: " (car p)))
                 (hash-set h (car p) (cdr p))))
           (ann #hash() (HashTable String Integer))
           ats_pair)]
         [ids_hash : (HashTable Integer attr)
          (hash-map/copy
           ats_hash
           (lambda ([k : String] [n : Integer]) : (Values Integer attr)
             (if (<= n c)
                 (values n (hash-ref a k)) ; 默认调用函数者知道原来属性名，因此抛弃属性名只保留序数
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
     (hash "TFORM1" (attr "1I" "")
           "TFORM2" (attr "1J" "")
           "TFORM3" (attr "1K" ""))
     3 "TFORM")
    (hash 1 (attr "1I" "")
          2 (attr "1J" "")
          3 (attr "1K" "")))
   ; 让它报错
   (check-exn
    exn:fail?
    (lambda ()
      (take-fields-named
       (hash "TFORM1" (attr "1I" "")
             "TFORM2" (attr "1J" "")
             "TFORM3" (attr "1K" ""))
       4 "TFORM" #t))))
  
  ;; 测试用文件
  (define tess (open-input-file "../test/tess2024249191853-s0083-0000010001363713-0280-s_lc.fits"))
  (define lamost (open-input-file "../test/dr10_v2.0_LRS_catalogue.fits"))
  ;; 接下来的测试将使用这两个文件作为基准
  (test-case
   "read-header-test"
   (pretty-print (read-header tess)))
)
