#lang typed/racket

(require typed/rackunit
         "../util.rkt"
         "../attr.rkt")

(provide image-table
         image-table-data
         image-table-shape
         build-image-table
         Image)

;;;
;   IMAGE表相关类型定义
;;;

(define-type Image
  (U (Vectorof Image) Bytes))

(struct image-table
  ; image表允许超出两个维度的形状，所以这里储存不再使用Matrix，而是多层Vector
  ([shape : (Vectorof Integer)]
   [data : Image])
  #:prefab)

;;;
;   IMAGE表操作函数
;;;

;;  -----对外接口函数-----
;;  从对齐区块的字节流中构建IMAGE表
(: build-image-table (-> Input-Port Header-Attr image-table))
(define (build-image-table p at)
  ; 根据规范进行检查
  (when
      (or (not (valid-bitpix? at))
          (not (eqv? 0 (attr-val (hash-ref at "PCOUNT"))))
          (not (eqv? 1 (attr-val (hash-ref at "GCOUNT"))))
          (not (and (integer? (attr-val (hash-ref at "NAXIS")))
                    (<= (cast (attr-val (hash-ref at "NAXIS")) Integer) 999))))
    (error 'IMAGE-extension "wrong attribute value (BITPIX, GCOUNT, PCOUNT, NAXIS)"))
  ; 准备初始化的解析条件
  (let* ([bitpix (quotient (cast (attr-val (hash-ref at "BITPIX")) Integer) 8)]
         [naxis_c (cast (attr-val (hash-ref at "NAXIS")) Integer)]
         [naxis_ax
          (map (lambda ([a : (Pairof Integer attr)])
                 (cast (attr-val (cdr a)) Exact-Nonnegative-Integer))
               (hash->list (take-fields-named at naxis_c "NAXIS" #t)))]
         [data_bytes
          (read-bytes-aligned
           p
           (foldr (lambda ([x : Integer] [y : Integer]) (* x y)) bitpix naxis_ax))])
    ;   定义一个在这里用的工具函数，负责递归解析
    (: recur-decode-axis (-> (Listof Integer) Bytes Image))
    (define (recur-decode-axis ax b)
      (if (= 1 (length ax))
          ; 最后一维
          (let ([bvec (split-bytes-vec b bitpix)])
            (if (= (vector-length bvec) (car ax)) (cast bvec Image)
                (error "Wrong naxis number")))
          ; 递归解析多维
          (let ([subvec
                 (split-bytes-vec
                  b
                  (foldr (lambda ([x : Integer] [y : Integer]) (* x y)) bitpix (cdr ax)))])
            (for/vector ([sv subvec]) : Image
              (recur-decode-axis (cdr ax) sv)))))
    ;   解析开始
    (image-table (list->vector naxis_ax)
                 (recur-decode-axis naxis_ax (assert data_bytes bytes?)))))