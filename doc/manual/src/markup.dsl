<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY dbstyle SYSTEM "docbook.dsl" CDATA DSSSL>
]>

<style-sheet>
<style-specification use="docbook">
<style-specification-body>

;; HTML:

(define %footnotes-at-end%
  ;; Should footnotes appear at the end of HTML pages?
  #t)

(define %html-ext% 
  ;; Default extension for HTML output files
  ".html")

(define %root-filename%
  ;; Name for the root HTML document
  "index")

(define %css-decoration%
  ;; Enable CSS decoration of elements
  #t)

(define %stylesheet%
  ;; Name of the stylesheet to use
  "markup.css")

;; printing:

(define bop-footnotes
  ;; Make "bottom-of-page" footnotes?
  #t)

;; both:

(define %section-autolabel%
   ;; Are sections enumerated?
   #t)

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="dbstyle">
</style-sheet>
