#lang web-server/insta
 


 (require "pronunciationlib.rkt")
; A blog is a (listof post)
; and a post is a (post title body)
(struct post (body))
 
; BLOG: blog
; The static blog.
(define BLOG
  (list (post " ")
        (post " ")))
 
; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG)]
          [else
           BLOG]))
  (render-blog-page a-blog request))
 
 
; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-post? bindings)
  
       (exists-binding? 'body bindings))
 
 
; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-post bindings)
  (post
        (find-rhymes (extract-binding/single 'body bindings) cmudict)))
 
; render-blog-page: blog request -> response
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page a-blog request)
  (response/xexpr
   `(html (head (title "Find Rhyme"))
          (body
           (h1 "Find Rhyme")
           
           (form
            
            (input ((name "body")))
            (input ((type "submit"))))
           ,(render-posts a-blog)))))
 
; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-post a-post)
  `(div ((class "post"))
        
        (p ,(post-body a-post))))
 
 
; render-posts: blog -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))



(define (find-rhymes str1 dict)
  (ap (make-rhymes (search str1 dict) dict)))

(define (make-rhymes pron1 dict)
  (cond[(empty? dict) empty]
       [(equal? pron1 (first (rest (first dict)))) (make-rhymes pron1 (rest dict))]
       [(equal? (comp-list pron1) (comp-list (first (rest (first dict)))))
        (append (list (first (first dict))) (make-rhymes pron1 (rest dict)))]
       [else (make-rhymes pron1 (rest dict))]))

(define (comp-list pron)
  (cond[(empty? pron) empty]
       [(and (not (symbol? (first pron))) (equal? (rest (first pron)) (list 1)))
        (append (list (first pron)) (rest pron))]
       [else (comp-list (rest pron))]))

(define (search str1 dict)
  (cond[(empty? dict) empty]
       [(string=? str1 (first (first dict))) (first (rest (first dict)))]
       [else (search str1 (rest dict))]))

(define (ap list1)
  (cond[(empty? list1) ""]
       [else (string-append (first list1) " " (ap (rest list1)))]))
