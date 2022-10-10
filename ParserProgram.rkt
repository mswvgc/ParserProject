#lang racket
(require parser-tools/lex
         parser-tools/yacc
         megaparsack
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr
         global)
         


(define-tokens value-tokens (id number))
(define-empty-tokens op-tokens (newline assign plus minus times div OEF))
(define-empty-tokens delim-tokens (lparen rparen))

  
(port-count-lines-enabled #t)


;; Modified code for lexer from blog post @ https://matt.might.net/articles/lexers-in-racket/
(define calc-lexer
  (lexer

   [":="
    (cons `(assign ,(position-line start-pos)) (calc-lexer input-port))]
 
   [#\+
    (cons `(plus ,(position-line start-pos)) (calc-lexer input-port))]
   
   [#\- 
    (cons `(minus ,(position-line start-pos)) (calc-lexer input-port))]
   
   [#\* 
    (cons `(times ,(position-line start-pos)) (calc-lexer input-port))]
   
   [#\/ 
    (cons `(div ,(position-line start-pos)) (calc-lexer input-port))]
   
   [#\( 
    (cons `(lparen ,(position-line start-pos)) (calc-lexer input-port))]
      
   [#\)
    (cons `(rparen ,(position-line start-pos)) (calc-lexer input-port))]
   
   ["write"
    (cons `(write ,(position-line start-pos)) (calc-lexer input-port))]
   
   ["read"
    (cons `(read ,(position-line start-pos)) (calc-lexer input-port))]
      
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))  ;(:: letter (:* (:or letter digit)))
    (cons `(id ,(position-line start-pos)) (calc-lexer input-port))]

   [(:+ (char-range #\0 #\9)) ;(:+ digit)
    (cons `(number ,(position-line start-pos)) (calc-lexer input-port))]
    
   [whitespace 
    (calc-lexer input-port)]

    ["$$"
    (cons `(eof ,(position-line start-pos)) (calc-lexer input-port))]
   
   [(eof)
    (list `(eof ,(position-line start-pos)))]))

(define (match expec tokens)
  (cond
    [(equal? (first(first tokens)) expec)
     (rest tokens)]
    [else (error "parse error on line " (second (first tokens)))]))

(define (program tokens)
  (cond
    [(or (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'read)
         (equal? (first (first tokens)) 'write)
         (equal? (first (first tokens)) 'eof))
    (match 'eof (stmt_list tokens))]

    [else (error "parse error on line " (second (first tokens)))]))


 ;;based off students code shared in group message.      
(define (stmt_list tokens)
  (cond
    [(or (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'read)
         (equal? (first (first tokens)) 'write))
     (stmt_list (stmt tokens))]
    
    [(equal? (first (first tokens)) 'eof) tokens]

    [else (error "parse error on line " (second (first tokens)))]))

(define (stmt tokens)
  (cond
    [(equal? (first (first tokens)) 'id) (expr(match 'assign (match 'id tokens)))]
    [(equal? (first (first tokens)) 'read) (match 'id (match 'read tokens))]
    [(equal? (first (first tokens)) 'write) (expr(match 'write tokens))]

    [else (error "parse error on line " (second (first tokens)))]))

(define (expr tokens)
  (cond
   [(or (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'number)
         (equal? (first (first tokens)) 'lparen))
     (term_tail(term tokens))]
   
    [else (error "parse error on line " (second (first tokens)))]))

(define (term_tail tokens)
  (cond
    [(or (equal? (first (first tokens)) 'plus)
         (equal? (first (first tokens)) 'minus))
     (term_tail(term(add_op tokens)))]
    
    [(or (equal? (first (first tokens)) 'rparen)
         (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'read)
         (equal? (first (first tokens)) 'write)
         (equal? (first (first tokens)) 'eof))
     tokens]

    [else (error "parse error on line " (second (first tokens)))]))

(define (term tokens)
  (cond
    [(or (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'number)
         (equal? (first (first tokens)) 'lparen))
     (factor_tail(factor tokens))]

    [else (error "parse error on line " (second (first tokens)))]))

(define (factor_tail tokens)
  (cond
    [(or (equal? (first (first tokens)) 'times)
         (equal? (first (first tokens)) 'div))
     (factor_tail(factor(mult_op tokens)))]
    
    [(or (equal? (first (first tokens)) 'plus)
         (equal? (first (first tokens)) 'minus)
         (equal? (first (first tokens)) 'rparen)
         (equal? (first (first tokens)) 'id)
         (equal? (first (first tokens)) 'read)
         (equal? (first (first tokens)) 'write)
         (equal? (first (first tokens)) 'eof))
     tokens]

    [else (error "parse error on line " (second (first tokens)))]))

(define (factor tokens)
  (cond
    [(equal? (first (first tokens)) 'id) (match 'id tokens)]
    [(equal? (first (first tokens)) 'number) (match 'number tokens)]
    [(equal? (first (first tokens)) 'lparen) (match 'rparen (expr (match 'lparen tokens)))]
    
    [else (error "parse error on line " (second (first tokens)))]))

(define (add_op tokens)
  (cond
    [(equal? (first (first tokens)) 'plus) (match 'plus tokens)]
    [(equal? (first (first tokens)) 'minus) (match 'minus tokens)]

    [else (error "parse error on line " (second (first tokens)))]))

(define (mult_op tokens)
  (cond
    [(equal? (first (first tokens)) 'times) (match 'times tokens)]
    [(equal? (first (first tokens)) 'div) (match 'div tokens)]

    [else (error "parse error on line " (second (first tokens)))]))


(define (parse input)
  (define tokens (calc-lexer (open-input-file input)))
  (displayln tokens)
(program tokens) (displayln "Accept"))

(parse "input05.txt")