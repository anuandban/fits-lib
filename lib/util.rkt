#lang typed/racket

;;; 工具宏
(provide $>)

;;  管道运算符
;       用初始值定义一个中间变量, 在之后的表达式序列中,将这个中间变量带入上一个表达式的值, 嵌套进下一个表达式
;   格式:
;   ($> [x some_fn]
;       (fn1 x)
;       (fn2 x)
;       (fn3 x)
;       ...)
;   等价于:
;   (fn3 (fn2 (fn1 somefn)))

;;; 警告: 尚未经过严格测试, 不要在这里嵌套宏, 我不知道那会发生什么

(define-syntax ($> stx)
  (define dat (syntax->datum stx))
  (define mid_var (caadr dat))
  (define first_apply (cadadr dat))
  (define rest_apply (cddr dat))
  (datum->syntax
   stx
   (foldl
    (lambda (fn fn_chain)
      (map (lambda (id) (if (eq? id mid_var) fn_chain id)) fn))
    first_apply rest_apply)))



;;; 工具函数
(provide read-blocks
         read-bytes-aligned)

;;  -----读取Input-Port相关-----
;   从input-port中读取N个区块的字节，如果出现了EOF，直接返回#f
(: read-blocks (-> Input-Port Integer (Option Bytes)))
(define (read-blocks p bl_n)
  (let ([bt? (read-bytes (* 2880 bl_n) p)])
    (if (bytes? bt?)
        bt?
        #f)))

;   从input-port中读取未对齐的字节数量，但是读取时自动对齐并舍弃那些没用的空字节
(: read-bytes-aligned (-> Input-Port Integer (Option Bytes)))
(define (read-bytes-aligned p by_n)
  (let ([bt? (read-blocks p (exact-ceiling (/ by_n 2880)))])
    (if (bytes? bt?)
        (subbytes bt? 0 by_n)
        #f)))
;;  ---------------
