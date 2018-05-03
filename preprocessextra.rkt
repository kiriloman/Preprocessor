#lang racket
(provide add-active-token def-active-token process-string)
(require srfi/13)

;;Hashtable with tokens and functions
(define associations (make-hash))

;;Adds a token with respective function to a hashmap for later use in process-string
(define (add-active-token str function)
  (hash-set! associations str function))

;;process-string
(define (process-string str)
  (if (equal? str "")
      ""
      (if (not (empty? (token-to-execute str)))
            (string-append (substring str 0 (string-contains str (token-to-execute str)))
                                     (process-string ((hash-ref associations (token-to-execute str))
                                                      (substring str (string-contains str (token-to-execute str))))))
            str))
  )

;;token-to-execute decides which token (if any) comes first in the string 
(define (token-to-execute str)
  (let ([token null])
    (let ([position (string-length str)])
      (for ([(key value) associations]) ;;For each token and function do
        (when (string-contains str key) ;;If token exists in string
          (when (< (string-contains str key) position)
            (set! position (string-contains str key))
            (set! token key)))))
    token))

;;def-active-token macro
(define-syntax-rule (def-active-token token str body)
  (hash-set! associations token 
             (lambda str body)))

;;Tokens implementation

;;Local Type Inference
(def-active-token "var" (str)
 (string-append (substring str (+ (string-contains str "new ") 4) (string-contains str "(")) (substring str 3)))




;;String Interpolation


;;Type Aliases




;;Coisas irrelevantes neste momento

(define (def-active-token0 token str . body) ;;Body can have any amount of 'arguments'
  (hash-set! associations token 
             (lambda str body)))

(define (token-positions str)
  (let ([positions-list '()])
    (for ([(key value) associations]) ;;For each token and function do
      (let ([token-length (string-length key)])  ;;token-length is the token's length
        (when (string-contains str key) ;;If token exists in string
            (set! positions-list (append positions-list (list key (+ (string-contains str key) token-length)))))) ;;Append token and its pos+1
      )
    positions-list))


(define (string-after-newline str)
(or (for/or ((c (in-string str))
(i (in-naturals)))
(and (char=? c #\newline)
(substring str (+ i 1))))
""))

(define (foo str)
  (if (not (equal? (string-length str) 0))
      (substring str 1)
      ""))