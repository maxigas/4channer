#lang racket
(require srfi/1 net/url json racket/date (planet jaymccarthy/mongodb:1:12) (planet vyzo/gnuplot:1:3))
(date-display-format 'iso-8601)

(define (4get)
  "Download and save 4chan's /r/ board."
  (let* ([urls (for/list ([i 1]) (string-append "http://api.4chan.org/b/" (number->string i) ".json"))]
         [jsons (for/list ([url urls]) (string->jsexpr (port->string (get-pure-port (string->url url)))))]
         [4chan (make-mongo-db (create-mongo) "4chan")]
         [timestamp (string-replace (date->string (current-date) (current-seconds)) " " "_")]
         [snapshot (mongo-db-create-collection! 4chan timestamp #:capped? #f #:size 0)])
    (mongo-collection-insert-docs! snapshot jsons)
    (close-mongo! m)))

(4get)

(define (replies)
  (let ([4chan (make-mongo-db (create-mongo) "4chan")]
        [collections] (mongo-db-collections 4chan))
    (current-mongo-db 4chan)
    (for/list (collection collections) (mongo-dict-query col query))))

(display (replies))

(define (4plot n)
  "Plots replies per thread using n recent snapshots, writes image to /tmp/4chan-replies-per-thread.png"
  (let* ([plotter (gnuplot-spawn)]
         [sin+cos (build-list 200 (lambda (x) (let ((x (/ (* x pi) 100))) (list x (sin x) (cos x)))))]
         [mydata (gnuplot-data sin+cos)]
         [mysin (gnuplot-item mydata '(using (seq: 1 2) title (str "sin(x)") with line 1))]
         [mycos (gnuplot-item mydata '(using (seq: 1 3) title (str "cos(x)") with line 2))])
    ;; this is just a gnuplot example, but it works:
    (gnuplot-set g '(title (str "4chan /r/ board. Replies per thread.")))
    (gnuplot-plot g #:range `((#f ,(* 2 pi)) ()) mysin mycos)
    (gnuplot-hardcopy g "/tmp/4chan-replies-per-thread.png" #:term '(png))
    ))

(define (4inspect)
  "Not very useful function that can be customised later for displaying query results."
  (let* ([4chan (make-mongo-db (create-mongo) "4chan")]
         [collections (mongo-db-collections 4chan)]
         [firstcollection (make-mongo-collection 4chan (first collections))])
    (display (mongo-collection-find firstcollection '()))))

(4inspect)
  
