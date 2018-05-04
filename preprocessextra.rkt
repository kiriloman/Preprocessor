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
(def-active-token "#" (str)
  (if (not (equal? (second (string->list str)) #\"))
    (string-append "\" + ("
                    (substring str (+ (string-contains str "{") 1) (string-contains str "}"))
                    ") + \""
                    (substring str (+ (string-contains str "}") 1)))
    (substring str 1)))

;;Type Aliases
;(def-active-token "alias" (str)
 ; (let ([alias (substring str (+ (string-contains str "alias ") 6) (string-contains str " ="))])
  ;  (regexp-replace* (string-append "[^a-zA-Z0-9]" alias "[^a-zA-Z0-9]") (substring str (+ (string-contains str ";") 1)) (string-append "[^a-zA-Z0-9]" (substring str (+ (string-contains str "= ") 2) (string-contains str ";")) "[^a-zA-Z0-9]")))
  ;)

;(def-active-token "alias" (str)
 ; ((let ([alias (substring str (+ (string-contains str "alias ") 6) (string-contains str " ="))]
  ;      [value (substring str (+ (string-contains str "= ") 2) (string-contains str ";"))])
   ;  (set! str (substring str (+ (string-contains str ";") 1)))
    ; (while (not (string=? str (string-replace-substring str alias value)))
     ;       (set! str (string-replace-substring str alias value))))
 ; str)
  ;)

;(def-active-token "alias" (str)
 ; (let ([alias (substring str (+ (string-contains str "alias ") 6) (string-contains str " ="))]
  ;      [value (substring str (+ (string-contains str "= ") 2) (string-contains str ";"))])
   ; (string-append 
  ;)

(define (find-substring-position str substring)
  (regexp-match-positions (string-append "[^a-zA-Z0-9]" substring "[^a-zA-Z0-9]") str)
  )

(def-active-token "alias" (str)
  (let ([alias (substring str (+ (string-contains str "alias ") 6) (string-contains str " ="))]
        [type (substring str (+ (string-contains str "= ") 2) (string-contains str ";"))])
    (set! str (substring str (+ (string-contains str ";") 1)))
    (string-replace-substring str alias type))
  )

(define (string-replace-substring str substr newsubstring)
   (if (not (equal? (regexp-match-positions (string-append "[^a-zA-Z0-9]" substr "[^a-zA-Z0-9]") str) #f))
       (let ([fromindex (caar (regexp-match-positions (string-append "[^a-zA-Z0-9]" substr "[^a-zA-Z0-9]") str))]
            [toindex (cdar (regexp-match-positions (string-append "[^a-zA-Z0-9]" substr "[^a-zA-Z0-9]") str))])
        (string-append (string-append (substring str 0 (+ fromindex 1)) newsubstring)
                       (string-replace-substring (substring str (- toindex 1)) substr newsubstring)))
       str))





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