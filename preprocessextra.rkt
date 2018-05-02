#lang racket
(provide add-active-token def-active-token process-string)
(require srfi/13)

;;Hashtable with tokens and functions
(define associations (make-hash))

;;Adds a token with respective function to a hashmap for later use in process-string
(define (add-active-token str function)
  (hash-set! associations str function))

(define (process-string str)
  (if (equal? str "")
      ""
      (if (not (empty? (token-to-execute str)))
            (string-append (substring str 0 (string-contains str (token-to-execute str)))
                                     (process-string ((hash-ref associations (token-to-execute str))
                                                      (substring str (string-contains str (token-to-execute str))))))
            str))
  )

(define (token-to-execute str)
  (let ([token null])
    (let ([position (string-length str)])
      (for ([(key value) associations]) ;;For each token and function do
        (when (string-contains str key) ;;If token exists in string
          (when (< (string-contains str key) position)
            (set! position (string-contains str key))
            (set! token key)))))
    (display token)
    token))

(define (def-active-token token list body)
  "s")

;;Coisas irrelevantes neste momento

(define (token-positions str)
  (let ([positions-list '()])
    (for ([(key value) associations]) ;;For each token and function do
      (let ([token-length (string-length key)])  ;;token-length is the token's length
        (when (string-contains str key) ;;If token exists in string
            (set! positions-list (append positions-list (list key (+ (string-contains str key) token-length)))))) ;;Append token and its pos+1
      )
    positions-list))

(define (foo str)
  (if (not (equal? (string-length str) 0))
      (substring str 1)
      ""))