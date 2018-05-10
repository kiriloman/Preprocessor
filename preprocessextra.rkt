#lang racket
(provide add-active-token def-active-token process-string)
(require srfi/13)

;Hashtable with tokens and functions
(define associations (make-hash))

;Adds a token with respective function to a hashmap for later use in process-string
(define (add-active-token str function)
  (hash-set! associations str function))

;Recursively applies first token found in a string appending the results together
(define (process-string str)
  (let ([token (token-to-execute str)])
    (cond
      [(empty? token) str]
      [else (string-append (substring str 0 (string-contains str token))
                           (process-string ((hash-ref associations token)
                                            (substring str (string-contains str token)))))])))

;token-to-execute decides which token (if any) comes first in the string and returns its position in str
(define (token-to-execute str)
  (let ([token null]
        [position (string-length str)])
    (for ([(key value) associations])
      (let ([key-position (regexp-match-positions (string-append key "[^0-9]") str)])
        (cond
          [(and key-position (equal? (caar key-position) 0))  (set! position 0) (set! token key)]
          [else (set! key-position (regexp-match-positions (string-append "[^a-zA-Z0-9]" key "[^a-zA-Z0-9]") str))
                (cond
                  [(and key-position (< (caar key-position) position)
                        (set! position (caar key-position))
                        (set! token key))])])))
    token))

;def-active-token macro
(define-syntax-rule (def-active-token token str body)
  (hash-set! associations token 
             (lambda str body)))


;Extra Tokens implementation

;Getters and Setters Token
;Example: GS int n = 2; -> int n = 2;
;                        public void setN(int n) {
;                            this.n = n;
;                        }
;                        public int getN() {
;                            return n;
;                        }
(def-active-token "GS" (str)
  (let ([type (string-trim-all (substring str 2 (+ (string-contains (substring str 3) " ") 3)))]
        [var-name (string-trim-all (substring str (+ (string-contains (substring str 3) " ") 3)
                                              (if (string-contains str "=")
                                                  (min (string-contains str "=") (string-contains str ";"))
                                                  (string-contains str ";"))))])
    (string-append (substring str 3) (implement-setter type var-name) (implement-getter type var-name))))


;Implements a setter for given var-name of given type
(define (implement-setter type var-name)
  (string-append "\npublic void set" (string-titlecase var-name) "(" type " " var-name ") {\n\tthis." var-name " = " var-name ";\n}"))

;Implements a getter for given var-name of given type
(define (implement-getter type var-name)
  (string-append "\npublic " type " get" (string-titlecase var-name) "() {\n\treturn " var-name ";\n}"))


;#include Token
;Given #include <file-name> writes the content of file-name and removes #include <file-name> line
;The file-name must be in source's directory
(def-active-token "#include" (str)
  (let ([file-name (substring str (+ (string-contains str "<") 1) (string-contains str ">"))])
    (if (file-exists? (string-append (path->string (current-directory)) (string-trim-all file-name)))
        (string-append (file->string (string-append (path->string (current-directory)) (string-trim-all file-name)))
                       (substring str (+ (string-contains str ">") 1)))
        "No such file in directory.")))


;Helpers
;Trims left side of the given string from spaces
(define (string-trim-all-left str)
  (if (or (equal? (string-contains str " ") 0) (equal? (string-contains str "\n") 0) (equal? (string-contains str "\t") 0)) ;se tem espaço no inicio
      (string-append (string-trim-all-left (substring str 1)))
      str))

;Trims left and right side of the given string from spaces
(define (string-trim-all str)
  (list->string (reverse
                 (string->list (string-trim-all-left (list->string (reverse
                                                                    (string->list (string-trim-all-left str)))))))))