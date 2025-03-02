#lang info

(define collection "fits-lib")

;; 第三方库依赖
(define deps 
  '("base"
    "typed-racket-lib"))

;; 构建依赖
(define build-deps 
  '("scribble-lib"
    "racket-doc"
    "rackunit-typed"))

;; Scribble 文档
(define scribblings 
  '(("scribblings/fits.scrbl" ())))

(define pkg-desc "Description Here")

;; 测试相关
(define test-include-paths
  '(("lib" all)))

(define version "0.1")
(define pkg-authors '(anuandban))
(define license '(MIT))
