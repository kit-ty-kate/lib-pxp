<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!-- The default is the print stylesheet. Call 'jade' with option '-ihtml'
     to select the HTML stylesheet.
  -->

<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl SYSTEM "docbook.dsl" CDATA dsssl>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl SYSTEM "docbook.dsl" CDATA dsssl>
]]>
]>
<style-sheet>
<style-specification use="docbook">
<style-specification-body> 

;; HTML:

<![%html;[

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

(define %graphic-default-extension%
  ;; Default extension for graphic FILEREFs
  "gif")

(define %default-title-end-punct%
  ;; Only a space after run-in titles:
  " ")

(define (toc-depth nd)
  (if (string=? (gi nd) (normalize "book"))
      4
      1))
]]>

;; printing:

<![%print;[

(define bop-footnotes
  ;; Make "bottom-of-page" footnotes?
  #t)

(define %graphic-default-extension%
  ;; Default extension for graphic FILEREFs
  "ps")

]]>

;; both:

(define %section-autolabel%
   ;; Are sections enumerated?
   #t)

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
