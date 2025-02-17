#lang typed/racket

(provide attr
         attr-val
         attr-comment
         Attr
         AttrValue
         take-fields-named)

(define-type AttrValue (U Boolean String Number 'undefined))

(struct attr ([val : AttrValue] [comment : String]) #:mutable #:prefab)

(define-type Attr (HashTable String attr))

;   提取一类属性出来，这类属性序数最大值不超过c，不然会报错
;   continued?开启时，强制要求属性序数必须覆盖完1~c的所有范围，出现缺失则报错
;   注意返回值是个以序数为其键值的哈希表
(: take-fields-named (->* (Attr Integer String) (Boolean) (HashTable Integer attr)))
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