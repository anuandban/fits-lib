#lang typed/racket
;;; 工具宏
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

(provide $>)
