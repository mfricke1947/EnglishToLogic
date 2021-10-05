;/*
;As far as I am concerned, anyone can make any use of this software as they wish. To
;formalize that slightly....

;Copyright (C) 2021 Martin Frické (mfricke@arizona.edu https://softoption.us mfricke@softoption.us mfricke1947@gmail.com)

;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
;files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
;modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the 
;Software is furnished to do so, subject to the following conditions:

;The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
;WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR 
;COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR 
;OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;*/


; 10/3/21 mf from older code, initially Pascal code 1991 Martin Frické mfricke@arizona.edu
;
; This translates English into Logic, primarily in the context of
; teaching logic in a college.

; There are 3 ideas behind what is going on.
; 1) The well known AI program Eliza can produce
;     a chatbot that produce one-step one-reply
;     conversations like:
;       you: I feel sick
;       Eliza: Please tell me why you feel sick
;       etc.
;    What is going on here is that Eliza has rules
;    containing patterns eg
;      Input: I feel (?X) Reply: Please tell me why you feel (?X)
;    and a pattern matcher (and a randomizer).
;    Eliza matches the input, if it can, and
;    returns a random choice among the matching outputs
; 2) Symbolizing logic to predicate calculus level can
;    be done by a repeated one-step divide and conquer
;    technique e.g.
;       We reduce unemployment and we increase health costs
;       We reduce unemployment & we increase health costs
;       R & we increase health costs
;       R & I
; 3) Each step in (2) can be done by a pattern matching rule
;     e.g.  Input: (?P1) and (?P2) Reply: (?P1) & (?P2)
;    However, this is going to be more complicated than
;    Eliza because not any old pattern match will do. In the
;    example case, the (?P)s have to be propositions i.e. there
;    is a guard or predicate on the pattern which has to be
;    satisfied.
;    Then it just gets a lot more complicated. For example,
;    the rule
;       (general-match ( ((? Term) (lambda (s) (term-p s)))
;                      ((? RV) (lambda (s) (rv-p s))) ((? _)
;                      (lambda (s) (equal? '(NEITHER) s )))
;                      ((? Subj1) (lambda (s) (subject-p s))) ((? _)
;                      (lambda (s) (equal? '(NOR) s )))
;                      ((? Subj2) (lambda (s) (subject-p s))) ) 
;                      (  chNeg ((? Term) (? RV) (? Subj1) chOr (? Term) (? RV) (? Subj2))))
;    uses patterns with guards for terms, subjects,relational verbs, literal  "neither" and literal "nor".
;    And it will translate, e.g.
;    Input: Betty is taller than neither Ann nor Chloe
;    Output: ~ ((Betty is taller than Ann) v (Betty is taller than Chloe))

; This code, or similar, is buried in a host (e.g. a web page). The host
; can supply selected text and overwrite the selected text (say on button
; press). The User just selects and overwrites, selects and overwrites, etc.
; until the entire initial English sentence is symbolized.

; The host also has a LISP interpreter, which runs the LISP. One consequence
; of this is that the LISP code uses a limited subset of what would be
; available in a fully-fledged LISP system. So the LISP code here
; may be considered to be long-winded.

; Attention needs to be paid to UPPER and LOWER case. Of course, the User
; will write whatever the User wishes. The LISP will be case-sensitive
; in its matching e.g. "not" won't match with "NOT". What happens in
; the running systems is that the input is massaged a little (e.g. periods
; are discarded (LISP does not like them)). The tokens in the input are
; supplied to the LISP separated by singles spaces. The text is all converted
; to UPPER case. This does not effect the User's text on the page.
; But the overwrite is UPPER case (which slightly highlights it).
; [It is standard in Common-LISP for the reader to convert all input to
; UPPER case internally, so it appears to be case insensitive.]

; In the plain Scheme dialect of LISP running here, there is no massaging 
; in the example tests. So there may be some oddity in making sure the
; text is in the right form for the rules.

; Racket can run different Schemes, even case insensitive ones.
; But they mostly have other features that our code is
; inconsistent with. In Racket, the code here has been run
; using the 'Pretty Big' choice of language. That probably
; would be the best choice for you.

; The host (web page, say) looks after the symbols for the logical connectives
; which can be different for the different systems. Here
; you might see 'chNeg' (for the negation character).
;
; You can look at the program running in a Web page
; at https://softoption.us/node/610 . Scroll down
; and watch the video.

; Some of the grammar rules were inspired by a paper
; I heard at the 1990(?) CAP Conference. I would cite
; that paper here but I cannot remember its title
; or authors (Hannah?).

; Press the green Run triangle top-right. Then scroll down
; and enter some of the test examples into the REPL.

; This may help you with Racket https://www.youtube.com/watch?v=oQD01ybB1z4

; Sorry about some of the formatting. There aren't any good LISP
; formatters, and the Scheme here was brought in inline from its
; initial home in Pascal code.



; globals
(define *adjectives* '())
(define *assertions* '())
(define *binary-adjectives* '())
(define *intransitive-verb* '())
(define *names* '())
(define *nouns* '())
(define *passive-verbs* '())
(define *propositions* '())
(define *rules* '())
(define *symbolizations* '())
(define *transitive-verb* '())

(define gEngVariables '())

;(define empty '())  ; sometimes this needs to be defined, sometimes not as it is built in
; depends on the dialect of LISP or SCHEME

(define (remember-adjective english-name-list logical-name) (set! *adjectives* (cons (list english-name-list logical-name) *adjectives*)))
(define (remember-binadj english-name-list logical-name) (set! *binary-adjectives* (cons (list english-name-list logical-name) *binary-adjectives*)))
(define (remember-iverb english-name-list logical-name) (set! *intransitive-verb* (cons (list english-name-list logical-name) *intransitive-verb*)))
(define (remember-pverb english-name-list logical-name) (set! *passive-verbs* (cons (list english-name-list logical-name) *passive-verbs*)))
(define (remember-tverb english-name-list logical-name) (set! *transitive-verb* (cons (list english-name-list logical-name) *transitive-verb*)))
(define (remember-name english-name-list logical-name) (set! *names* (cons (list english-name-list logical-name) *names*)))
(define (remember-noun english-name-list logical-name) (set! *nouns* (cons (list english-name-list logical-name) *nouns*)))
(define (remember-proposition english-name-list logical-name) (set! *propositions* (cons (list english-name-list logical-name) *propositions*)))

; some data to start with

; (temporary) propositional data
(set! *propositions* '(
  ((PHILOSOPHY IS HARD) H)
  ((PHILOSOPHY IS INTERESTING) I)
  ((LOGIC IS HARD) L)
  ((LOGIC IS INTERESTING) M)
  ((WE RUN A WAR) W)
  ((WE REDUCE UNEMPLOYMENT) U)
  ((WE INCREASE HEALTH COSTS) C)
  ))

;;; (temporary) predicate calculus data
(set! *adjectives* '( ((ANGRY) A)((BOLD) B)((CHEERFUL) C)))
(set! *binary-adjectives* '( ( (RUDER THAN) R)((SMARTER THAN) S)))
(set! gEngVariables '( ((X) x) ((Y) y) ((Z) z) ((W) w)))
(set! *intransitive-verb* '(((STUDIES) S)((THINKS) T)))
(set! *passive-verbs* '( ( (DRIVEN BY) D)((ENCOURAGED BY) E)))
(set! *names* '( ( (ARTHUR) a) ((BERYL) b) ((CHARLES) c) ))
(set! *nouns* '( ((NINCOMPOOP) N) ((PHILOSOPHER) P)))
(set! *transitive-verb* '( ((ANNOYS) A) ((BRINGS) B) ))

; matching support functions

(define *anonymous* '_)

;(define empty '()) sometimes need this defined sometimes not

(define (extract-variable variable-expr) (cadr variable-expr))

(define (variable-p variable-expr) (if (list? variable-expr) (if (= (length variable-expr) 2) (if (eqv? (car variable-expr) '?) #t #f) #f) #f))

(define (make-binding variable datum symbolization) (list variable datum symbolization))

(define (extract-key binding) (car binding))

(define (extract-value binding) (cadr binding))

(define (extract-symbolization binding) (caddr binding))

(define (find-binding variable-expr bindings) (assoc (extract-variable variable-expr) bindings))

(define (add-binding variable-expr datum symbolization bindings) (if (eqv? *anonymous* (extract-variable variable-expr)) bindings (cons (make-binding (extract-variable variable-expr) datum symbolization) bindings)))

(define (extract-var var-pair) (car var-pair))

(define (extract-test var-pair) (cadr var-pair))

(define (add-binding-to-each-member-of-list variable-expr datum symbolization list-of-assoc-lists) (if (null? list-of-assoc-lists) (list (add-binding variable-expr datum symbolization empty)) (map (lambda (assoc-list) (add-binding variable-expr datum symbolization assoc-list)) list-of-assoc-lists)))

(define (correct-matching-function key)
  (case key ((total-match) (eval total-match))
        ((left-match) (eval left-match))
        ((middle-match) (eval middle-match))
        ((left-then-middle-match) (eval left-literal-then-middle-match))
        (else (eval general-match))))

(define (rule-matching-function-key rule)
  (car rule))

(define (rule-matching-function rule)
  (correct-matching-function (rule-matching-function-key rule)))

(define (rule-if rule) (cadr rule))

(define (rule-then rule) (caddr rule))

; end of matching support functions

; rules (propositional)

(define gPropRules '(

                     ( left-match (((? _) (IT IS NOT THE CASE THAT)) ((? Sentence) (lambda (s) (proposition-p s))) ) ( chNeg ((? Sentence))))

                     ( left-then-middle-match (((? _) (BOTH)) ((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (AND)) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chAnd (? Sentence2))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (AND) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chAnd (? Sentence2))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (UNLESS) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chOr (? Sentence2))))

                     ( left-then-middle-match (((? _) (EITHER)) ((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (OR)) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chOr (? Sentence2))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (OR) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chOr (? Sentence2))))

                     ( left-then-middle-match (((? _) (NEITHER)) ((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (NOR)) ((? Sentence2) (lambda (s) (proposition-p s))) ) ( chNeg((? Sentence1)  chOr (? Sentence2))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (IF) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence2)  chImplic (? Sentence1))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (ONLY IF) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chImplic (? Sentence2))))

                     ( left-then-middle-match (((? _) (IF)) ((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (THEN)) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chImplic (? Sentence2))))

                     ( middle-match (((? Sentence1) (lambda (s) (proposition-p s))) ((? _) (IF AND ONLY IF) ) ((? Sentence2) (lambda (s) (proposition-p s))) ) (((? Sentence1)  chEquiv (? Sentence2))))


                     ))

; rules (predicate)

(define gRules '(

                 ( left-match (((? _) (IT IS NOT THE CASE THAT)) ((? Sentence) (lambda (s) (sentence-p s))) )
                              (chNeg ((? Sentence))))

                 ( left-then-middle-match (((? _) (BOTH)) ((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (AND)) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                          (((? Sentence1)  chAnd (? Sentence2))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (AND) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence1)  chAnd (? Sentence2))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (UNLESS) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence1) chOr(? Sentence2))))

                 ( left-then-middle-match (((? _) (EITHER)) ((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (OR)) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                          (((? Sentence1) chOr(? Sentence2))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (OR) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence1) chOr(? Sentence2))))

                 ( left-then-middle-match (((? _) (NEITHER)) ((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (NOR)) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                          ( chNeg((? Sentence1) chOr(? Sentence2))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (IF) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence2)  chImplic (? Sentence1))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (ONLY IF) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence1)  chImplic (? Sentence2))))

                 ( left-then-middle-match (((? _) (IF)) ((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (THEN)) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                          (((? Sentence1)  chImplic (? Sentence2))))

                 ( middle-match (((? Sentence1) (lambda (s) (sentence-p s))) ((? _) (IF AND ONLY IF) ) ((? Sentence2) (lambda (s) (sentence-p s))) )
                                (((? Sentence1)  chEquiv (? Sentence2))))

                 (middle-match( ((? Term) (lambda (s) (term-p s)))((? _) (IS NOT) )((? BP) (lambda (s) (bp-p s))))
                              ( chNeg ( (? Term) IS (? BP))))

                 (middle-match( ((? Term) (lambda (s) (term-p s)))((? _) (DOES NOT) )((? DP) (lambda (s) (dp-p s))))
                              ( chNeg ( (? Term) (? DP))))
	
                 (general-match( ((? Term) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS A) s )))((? NP) (lambda (s) (np-p s)))((? _) (lambda (s) (equal? '(THAT) s )))((? Subj) (lambda (s) (subject-p s)))((? RV) (lambda (s) (rv-p s))))
                               ( ((? Term) IS A (? NP)  chAnd (? Subj) (? RV) (? Term))))

                 (general-match( ((? Term) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS A) s )))((? Adj) (lambda (s) (adj-p s)))((? Noun) (lambda (s) (adjnoun-p s))))
                               ( ((? Term) IS (? Adj)  chAnd (? Term) IS A (? Noun))))

                 (general-match ( ((? Term) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS A) s )))((? NP) (lambda (s) (np-p s)))((? _) (lambda (s) (equal? '(THAT) s )))((? VP) (lambda (s) (vp-p s))))
                                ( ((? Term) IS A (? NP)  chAnd (? Term) (? VP))))

                 (left-match (((? _) (EVERYTHING)) ((? VP) (lambda (s) (vp-p s))))
                             (( chUniquantx) (x  (? VP))))

                 (left-match(((? _) (SOMETHING)) ((? VP) (lambda (s) (vp-p s))))
                            ( ( chExiquantx) (x  (? VP))))

                 (left-match (((? _) (NOTHING)) ((? VP) (lambda (s) (vp-p s))))
                             ( chNeg( chExiquantx) (x  (? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(EVERYTHING THAT) s )))((? VP1) (lambda (s) (vp-p s)))((? VP2) (lambda (s) (vp-p s))))
                                ( ( chUniquantx) (x  (? VP1)  chImplic x  (? VP2))))

                 (general-match ( ((? _) (lambda (s) (equal? '(SOMETHING THAT) s )))((? VP1) (lambda (s) (vp-p s)))((? VP2) (lambda (s) (vp-p s))))
                                ( ( chExiquantx) (x  (? VP1)  chAnd x  (? VP2))))

                 (general-match( ((? _) (lambda (s) (equal? '(NOTHING THAT) s )))((? VP1) (lambda (s) (vp-p s)))((? VP2) (lambda (s) (vp-p s))))
                               (  chNeg( chExiquantx) (x  (? VP1)  chAnd x  (? VP2))))

                 (general-match( ((? _) (lambda (s) (equal? '(EVERYTHING THAT) s )))((? Subj) (lambda (s) (subject-p s)))((? RV) (lambda (s) (rv-p s)))((? VP) (lambda (s) (vp-p s))))
                               ( ( chUniquantx) ((? Subj) (? RV) x  chImplic x  (? VP))))

                 (general-match( ((? _) (lambda (s) (equal? '(SOMETHING THAT) s )))((? Subj) (lambda (s) (subject-p s)))((? RV) (lambda (s) (rv-p s)))((? VP) (lambda (s) (vp-p s))))
                               ( ( chExiquantx) ((? Subj) (? RV) x  chAnd x  (? VP))))

                 (general-match( ((? _) (lambda (s) (equal? '(NOTHING THAT) s )))((? Subj) (lambda (s) (subject-p s)))((? RV) (lambda (s) (rv-p s)))((? VP) (lambda (s) (vp-p s))))
                               (  chNeg ( chExiquantx) ((? Subj) (? RV) x  chAnd x  (? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(EVERY ) s ))) ((? NP) (lambda (s) (np-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ( chUniquantx) (x IS A (? NP)  chImplic x  (? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(SOME ) s ))) ((? NP) (lambda (s) (np-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ( chExiquantx) (x IS A (? NP)  chAnd x  (? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(NO ) s ))) ((? NP) (lambda (s) (np-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                (  chNeg ( chExiquantx) (x IS A (? NP)  chAnd x  (? VP))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV1) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(EVERYTHING THAT ) s ))) ((? Subj) (lambda (s) (subject-p s))) ((? RV2) (lambda (s) (rv-p s))) )
                                ( ( chUniquanty) ((? Subj) (? RV2) y  chImplic (? Term) (? RV1) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV1) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(SOMETHING THAT ) s ))) ((? Subj) (lambda (s) (subject-p s))) ((? RV2) (lambda (s) (rv-p s))) )
                                ( ( chExiquanty) ((? Subj) (? RV2) y  chAnd (? Term) (? RV1) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV1) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(NOTHING THAT ) s ))) ((? Subj) (lambda (s) (subject-p s))) ((? RV2) (lambda (s) (rv-p s))) )
                                (  chNeg( chExiquanty)  ((? Subj) (? RV2) y  chAnd (? Term) (? RV1) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(EVERYTHING THAT ) s ))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ( chUniquanty) ( y (? VP)   chImplic (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(SOMETHING THAT ) s ))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ( chExiquanty) ( y (? VP)   chAnd (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(NOTHING THAT ) s ))) ((? VP) (lambda (s) (vp-p s))) )
                                (  chNeg( chExiquanty) ( y (? VP)   chAnd (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(EVERY ) s ))) ((? NP) (lambda (s) (np-p s))) )
                                ( ( chUniquanty) ( y IS A  (? NP)   chImplic (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(SOME ) s ))) ((? NP) (lambda (s) (np-p s))) )
                                ( ( chExiquanty) ( y IS A  (? NP)   chAnd (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(NO) s ))) ((? NP) (lambda (s) (np-p s))) )
                                (  chNeg( chExiquanty) ( y IS A  (? NP)   chAnd (? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(EVERYTHING) s ))) )
                                ( ( chUniquanty)((? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(SOMETHING) s ))) )
                                ( ( chExiquanty)((? Term) (? RV) y)))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(NOTHING) s ))) )
                                (  chNeg( chExiquanty)((? Term) (? RV) y)))

                 (general-match ( ((? _) (lambda (s) (equal? '(BOTH) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? Subj2) (lambda (s) (subject-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ((? Subj1) (? VP)  chAnd  (? Subj2) (? VP))))

                 (general-match (((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? Subj2) (lambda (s) (subject-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ((? Subj1)(? VP)  chAnd  (? Subj2)(? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(EITHER) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? Subj2) (lambda (s) (subject-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ((? Subj1) (? VP) chOr (? Subj2) (? VP))))

                 (general-match ( ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? Subj2) (lambda (s) (subject-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                ( ((? Subj1) (? VP) chOr (? Subj2) (? VP))))

                 (general-match ( ((? _) (lambda (s) (equal? '(NEITHER) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(NOR) s ))) ((? Subj2) (lambda (s) (subject-p s))) ((? VP) (lambda (s) (vp-p s))) )
                                (   chNeg ((? Subj1) (? VP) chOr (? Subj2) (? VP))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS BOTH) s ))) ((? BP1) (lambda (s) (bp-p s))) ((? _) (lambda (s) (equal? '(AND) s )))
                                                                     ((? BP2) (lambda (s) (bp-p s))) ) ( ((? Term) IS (? BP1)  chAnd  (? Term) IS (? BP2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS) s ))) ((? BP1) (lambda (s) (bp-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? BP2) (lambda (s) (bp-p s))) )
                                ( ((? Term) IS (? BP1)  chAnd  (? Term) IS (? BP2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS EITHER) s ))) ((? BP1) (lambda (s) (bp-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? BP2) (lambda (s) (bp-p s))) )
                                ( ((? Term) IS (? BP1) chOr (? Term) IS (? BP2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS) s ))) ((? BP1) (lambda (s) (bp-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? BP2) (lambda (s) (bp-p s))) )
                                ( ((? Term) IS (? BP1) chOr (? Term) IS (? BP2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS NEITHER) s ))) ((? BP1) (lambda (s) (bp-p s))) ((? _) (lambda (s) (equal? '(NOR) s ))) ((? BP2) (lambda (s) (bp-p s))) )
                                (  chNeg((? Term) IS (? BP1) chOr (? Term) IS (? BP2))))

                 (general-match ( ((? Subj) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(BOTH) s ))) ((? VP1) (lambda (s) (vp-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? VP2) (lambda (s) (vp-p s))) )
                                ( ((? Subj) (? VP1)  chAnd  (? Subj) (? VP2))))

                 (general-match ( ((? Subj) (lambda (s) (subject-p s))) ((? VP1) (lambda (s) (vp-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? VP2) (lambda (s) (vp-p s))) )
                                ( ((? Subj) (? VP1)  chAnd  (? Subj) (? VP2))))

                 (general-match ( ((? Subj) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(EITHER) s ))) ((? VP1) (lambda (s) (vp-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? VP2) (lambda (s) (vp-p s))) )
                                                                                                ( ((? Subj) (? VP1) chOr (? Subj) (? VP2))))

                 (general-match ( ((? Subj) (lambda (s) (subject-p s))) ((? VP1) (lambda (s) (vp-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? VP2) (lambda (s) (vp-p s))) )
                                                                                                (((? Subj) (? VP1) chOr (? Subj) (? VP2))))

                 (general-match ( ((? Subj) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(NEITHER) s ))) ((? VP1) (lambda (s) (vp-p s))) ((? _) (lambda (s) (equal? '(NOR) s ))) ((? VP2) (lambda (s) (vp-p s))) )
                                                                                                (  chNeg ((? Subj) (? VP1) chOr (? Subj) (? VP2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(BOTH) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(AND) s )))
                                                                                                                                     ((? Subj2) (lambda (s) (subject-p s))) ) (((? Term) (? RV) (? Subj1)  chAnd  (? Term) (? RV) (? Subj2))))


                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(AND) s ))) ((? Subj2) (lambda (s) (subject-p s))) )
                                                                                                (((? Term) (? RV) (? Subj1)  chAnd  (? Term) (? RV) (? Subj2))))


                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(EITHER) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(OR) s )))
                                                                                                                                     ((? Subj2) (lambda (s) (subject-p s))) ) (((? Term) (? RV) (? Subj1) chOr (? Term) (? RV) (? Subj2))))


                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(OR) s ))) ((? Subj2) (lambda (s) (subject-p s))) )
                                                                                                (  ((? Term) (? RV) (? Subj1) chOr (? Term) (? RV) (? Subj2))))

                 (general-match ( ((? Term) (lambda (s) (term-p s))) ((? RV) (lambda (s) (rv-p s))) ((? _) (lambda (s) (equal? '(NEITHER) s ))) ((? Subj1) (lambda (s) (subject-p s))) ((? _) (lambda (s) (equal? '(NOR) s ))) ((? Subj2) (lambda (s) (subject-p s))) )
                                                                                                (  chNeg ((? Term) (? RV) (? Subj1) chOr (? Term) (? RV) (? Subj2))))


                 (middle-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (does)) ((? DP) (lambda (s) (dp-p s))) )                                                                                               (  ( (? Term) (? DP) )))

))


(define (instantiate-variables pattern a-list) (cond ((null? pattern) empty) ((not (pair? (car pattern))) (cons (car pattern) (instantiate-variables (cdr pattern) a-list))) ((eq? '? (car (car pattern))) (append (extract-value (find-binding (car pattern) a-list)) (instantiate-variables (cdr pattern) a-list))) (#t (cons (instantiate-variables (car pattern) a-list) (instantiate-variables (cdr pattern) a-list)))))

(define (symbolize-variables pattern a-list) (cond ((not (pair? pattern)) pattern) ((eq? '? (car pattern)) (extract-symbolization (find-binding pattern a-list))) (#t (cons (symbolize-variables (car pattern) a-list) (symbolize-variables (cdr pattern) a-list)))))

(define (try-rule rule assertion) (let ((binding-list ((rule-matching-function rule) assertion (rule-if rule))))
                                    (if (eq? empty binding-list)
                                        empty
                                        (do ((binding-list binding-list (cdr binding-list)) (result-list empty)) ((null? binding-list) result-list) (let ((result (instantiate-variables (rule-then rule) (car binding-list))))
                                                                                                                                                      (if (not (null? result)) (set! result-list (cons result result-list))))))))

(define (code list-of-coding-functions list-to-be-coded) (if (or (null? list-of-coding-functions) (null? list-to-be-coded)) empty (cons ((car list-of-coding-functions) (car list-to-be-coded)) (code (cdr list-of-coding-functions) (cdr list-to-be-coded)))))

(define (try-symbolization symbol-rule assertion) (let ((binding-list ((rule-matching-function symbol-rule) assertion (rule-if symbol-rule))))
                                                    (if (null? binding-list)
                                                        empty
                                                        (do ((binding-list binding-list (cdr binding-list)) (result-list empty)) ((null? binding-list) result-list) (let ((result (symbolize-variables (rule-then symbol-rule) (car binding-list))))
                                                                                                                                                                      (if (not (null? result)) (set! result-list (cons result result-list))))))))

(define (try-all-the-prop-shuffle-rules assertion) (if (null? assertion) empty (accumulate (map (lambda (rule) (try-rule rule assertion)) gPropRules))))

(define (try-all-the-prop-symbol-rules assertion) (if (null? assertion) empty (accumulate (map (lambda (symbol-rule) (try-symbolization symbol-rule assertion)) gPropSymbolizations))))

(define (try-all-the-symbol-rules assertion) (if (null? assertion) empty (accumulate (map (lambda (symbol-rule) (try-symbolization symbol-rule assertion)) gSymbolizations))))

(define (try-all-the-shuffle-rules assertion) (if (null? assertion) empty (accumulate (map (lambda (rule) (try-rule rule assertion)) gRules))))

(define (try-all-the-prop-rules assertion) (if (null? assertion)
                                               empty
                                               (let ((result-list (accumulate (map (lambda (rule) (try-rule rule assertion)) gPropRules))))
                                                 (if (null? result-list) (accumulate (map (lambda (symbol-rule) (try-symbolization symbol-rule assertion)) gPropSymbolizations)) result-list))))

(define (try-all-the-rules assertion) (if (null? assertion)
                                          empty
                                          (let ((result-list (try-all-the-symbol-rules assertion)))
                                            (if (null? result-list) (try-all-the-shuffle-rules assertion) result-list))))

; matching functions

(define (general-match word-list variable-pair-list) (let ((result-list empty)
                                                           (target-length (length word-list))
                                                           (no-of-variables (length variable-pair-list)))
                                                       (cond ((and (null? word-list) (null? variable-pair-list)) 'success) ((or (null? word-list) (null? variable-pair-list)) empty) (#t(let ((test (eval(extract-test (car variable-pair-list)))) ) (letrec ((mydotimes (lambda (count limit)(if (< count limit) (begin (let ((symbolization (test (butlast  word-list (- target-length (+ count 1))))) ) (if symbolization (let ((tail-assoc-list (general-match (nthcdr (+ count 1)  word-list) (cdr variable-pair-list))) ) (if (not (null? tail-assoc-list)) (begin (if (eqv? tail-assoc-list 'success) (set! tail-assoc-list empty)) (set! result-list (append result-list (add-binding-to-each-member-of-list (extract-var (car variable-pair-list)) (butlast  word-list (- target-length (+ count 1))) symbolization tail-assoc-list) )))) ) ) (mydotimes (+ 1 count) limit))) ))))(mydotimes 0 (- target-length (- no-of-variables 1)))) result-list)))))

(define (total-match word-list variable-pair-list) (let* ((test (eval (extract-test (car variable-pair-list))))
                                                          (symbolization (test word-list)))
                                                     (if symbolization (list (add-binding (extract-var (car variable-pair-list)) word-list symbolization empty)) empty)))

(define (left-match word-list variable-pair-list) (let ((left-literal (extract-test (car variable-pair-list)))
                                                        (variable (extract-var (cadr variable-pair-list)))
                                                        (test (eval (extract-test (cadr variable-pair-list)))))
                                                    (left-match-aux word-list left-literal variable test)))

(define (left-match-aux target left-literal variable test) (if (null? left-literal)
                                                               (let ((symbolization (test target)))
                                                                 (if symbolization (list (add-binding variable target symbolization empty)) empty))
                                                               (if (equal? (car left-literal) (car target)) (left-match-aux (cdr target) (cdr left-literal) variable test) empty)))

(define (middle-match word-list variable-pair-list) (middle-match-aux word-list (car variable-pair-list) (cadr variable-pair-list) (caddr variable-pair-list)))

(define (middle-match-aux target left-variable-pair middle-variable-pair right-variable-pair) (let* ((left-variable (extract-var left-variable-pair))
                                                                                                     (test1 (eval (extract-test left-variable-pair)))
                                                                                                     (middle-literal (extract-test middle-variable-pair))
                                                                                                     (right-variable (extract-var right-variable-pair))
                                                                                                     (test2 (eval (extract-test right-variable-pair)))
                                                                                                     (length-target (length target))
                                                                                                     (length-middle (length middle-literal))
                                                                                                     (result-list empty))
                                                                                                (if (or (< length-target 3) (< length-target (+ 2 length-middle)))
                                                                                                    empty
                                                                                                    (letrec ((mydotimes (lambda (count limit)
                                                                                                                          (if (< count limit)
                                                                                                                              (let ((temp-left (butlast target (- length-target (+ count 1))))
                                                                                                                                    (temp-middle (butlast (nthcdr (+ count 1) target) (- length-target (+ count 1 length-middle))))
                                                                                                                                    (temp-right (nthcdr (+ count 1 length-middle) target)))
                                                                                                                                (if (equal? middle-literal temp-middle)
                                                                                                                                    (let ((left-symbolization (test1 temp-left)))
                                                                                                                                      (if left-symbolization
                                                                                                                                          (let ((right-symbolization (test2 temp-right)))
                                                                                                                                            (if (and left-symbolization right-symbolization) (set! result-list (cons (add-binding right-variable temp-right right-symbolization (add-binding left-variable temp-left left-symbolization empty)) result-list)))))))
                                                                                                                                (mydotimes (+ 1 count) limit)))))) (mydotimes 0 (- length-target (+ 1 length-middle))) result-list))))

(define (left-literal-then-middle-match word-list variable-pair-list) (let ((left-literal (extract-test (car variable-pair-list))))
                                                                        (left-literal-then-middle-match-aux word-list left-literal (cadr variable-pair-list) (caddr variable-pair-list) (cadddr variable-pair-list))))

(define (left-literal-then-middle-match-aux target left-literal left-variable-pair middle-variable-pair right-variable-pair) (if (null? left-literal) (middle-match-aux target left-variable-pair middle-variable-pair right-variable-pair) (if (equal? (car target) (car left-literal)) (left-literal-then-middle-match-aux (cdr target) (cdr left-literal) left-variable-pair middle-variable-pair right-variable-pair) empty)))

(define gPropSymbolizations '(
 (total-match (((? Prop)  (lambda (s) (proposition-p s))))((? Prop)))
 ))

(define gSymbolizations '(

  (middle-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (IS A)) ((? Noun) (lambda (s) (noun-p s))) ) (  ( (? Noun) (? Term) )))

  (middle-match ( ((? Term) (lambda (s) (term-p s))) ((? _) (IS )) ((? Adj) (lambda (s) (adj-p s))) ) (  ( (? Adj) (? Term) )))

  (general-match ( ((? Term) (lambda (s) (term-p s))) ((? Vi) (lambda (s) (vi-p s))) ) (  ( (? Vi) (? Term) )))

  (general-match ( ((? Term1) (lambda (s) (term-p s))) ((? Vt) (lambda (s) (vt-p s))) ((? Term2) (lambda (s) (term-p s))) ) (  ( (? Vt) (? Term1)(? Term2) )))

  (general-match ( ((? Term1) (lambda (s) (term-p s))) ((? _) (lambda (s) (equal? '(IS) s))) ((? Verbp) (lambda (s) (verbp-p s))) ((? Term2) (lambda (s) (term-p s))) ) (  ( (? Verbp) (? Term1)(? Term2) )))

  (general-match( ((? Term1) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS) s)))((? Adj2) (lambda (s) (binary-adj-p s)))((? Term2) (lambda (s) (term-p s))))(  ( (? Adj2) (? Term1)(? Term2) )))

  (general-match( ((? Term) (lambda (s) (term-p s)))((? VT) (lambda (s) (vt-p s)))((? _) (lambda (s) (or (equal? '(HIMSELF) s)(equal? '(HERSELF) s)(equal? '(itself) s)))))(  ( (? VT) (? Term)(? Term) )))

  (general-match( ((? Term1) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS) s)))((? Verbp) (lambda (s) (verbp-p s)))((? _) (lambda (s) (or (equal? '(HIMSELF) s)(equal? '(HERSELF) s)(equal? '(itself) s)))))(  ( (? Verbp) (? Term1)(? Term1) )))

  (general-match( ((? Term1) (lambda (s) (term-p s)))((? _) (lambda (s) (equal? '(IS) s)))((? Adj2) (lambda (s) (binary-adj-p s)))((? _) (lambda (s) (or (equal? '(HIMSELF) s)(equal? '(HERSELF) s)(equal? '(itself) s)))))(  ( (? Adj2) (? Term1)(? Term1) )))

  ))


;;;;;;;;;;;;;;; Parts of speech parsing

(define gEngKeyWords '(IT IS NOT THE CASE THAT BOTH AND EITHER OR NEITHER NOR IF THEN ONLY UNLESS ITSELF HIMSELF HERSELF A DOES))

(define gUQuantWords '(EVERYTHING SOMETHING NOTHING))

(define gRQuantWords '(EVERY SOME NO))

;;;fTokenizer =
(define (get-token-triple word-list)
   ; sorry
  (if (null? word-list)
      (list '() 'endlist '())
      (let ((firstWord (car word-list))
            (key (list (car word-list)))
            (restWords (cdr word-list)))
        (if (member firstWord gEngKeyWords)
            (list firstWord 'engword restWords)
            (if (member firstWord gUQuantWords)
                (list firstWord 'qU restWords)
                (if (member firstWord gRQuantWords)
                    (list firstWord 'qR restWords)
                    (if (assoc key *intransitive-verb*)
                        (list firstWord 'iverb restWords)
                        (if (assoc key *nouns*)
                            (list firstWord 'noun restWords)
                            (if (or (assoc key *names*) (assoc key gEngVariables))
                                (list firstWord 'name restWords)
                                (if (assoc key *adjectives*)
                                    (list firstWord 'adj restWords)
                                    (if (assoc key *transitive-verb*)
                                        (list firstWord 'tverb restWords)
                                        (if (assoc key *binary-adjectives*)
                                            (list firstWord 'binadj restWords)
                                            (if (not (null? restWords))
                                                (let* ((secondWord (car restWords))
                                                       (firstTwoWords (list firstWord secondWord)))
                                                  (if (assoc firstTwoWords *binary-adjectives*)
                                                      (list firstTwoWords 'binadj (cdr restWords))
                                                      (if (assoc firstTwoWords *passive-verbs*)
                                                          (list firstTwoWords 'pverb (cdr restWords))
                                                          (list firstWord 'unknownToken restWords))))
                                                (list firstWord 'unknownToken restWords))))))))))))))

;;;fAdjNoun

(define (adjnoun-parse word-list)
  (let ((top (adjnoun-top (get-token-triple word-list))))
    (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (adjnoun-top token-triple)
  (let ((next (consume-adjectives token-triple)))(if (equal? (cadr next) 'noun) (get-token-triple (caddr next)) #f)))

;;; fBPhrase

(define (b-phrase-parse word-list) (let ((top (bp-top (get-token-triple word-list))))
                                     (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (bp-top token-triple)
  (let ((tertiary (bp-tertiary token-triple)))
     (if tertiary
      (let ((next-token (car tertiary))
            (next-type (cadr tertiary))
            (next-wordlist (caddr tertiary))
            (larger-bp #f))
        (if (and (equal? next-type 'engword) (or (equal? next-token 'AND) (equal? next-token 'OR))) (set! larger-bp (bp-top (get-token-triple next-wordlist))))
        (if larger-bp larger-bp tertiary))
      #f))
)

(define (bp-tertiary token-triple)
  (case (cadr token-triple)
    ((engword) (bp-tertiary-engword token-triple))
    ((adj) (get-token-triple (caddr token-triple)))
    ((pverb) (pverb-binadj-subroutine token-triple))
    ((binadj) (pverb-binadj-subroutine token-triple))
    (else #f)))

(define (pverb-binadj-subroutine token-triple)
  (let ((next (get-token-triple (caddr token-triple))))
       (if (and (equal? (cadr next) 'engword) (or (equal? (car next) 'ITSELF) (equal? (car next) 'HIMSELF) (equal? (car next) 'HERSELF)))
           (get-token-triple (caddr next))
           (subj-top next))))

(define (bp-tertiary-engword token-triple)
  (case (car token-triple)
    ((BOTH) (bp-bothand-type token-triple 'AND))
    ((EITHER) (bp-bothand-type token-triple 'OR))
    ((NEITHER) (bp-bothand-type token-triple 'NOR))
    ((A) (np-top (get-token-triple (caddr token-triple))))
    (else #f)))

(define (bp-bothand-type token-triple key2)
  (let ((next (if (equal? key2 'NOR) (bp-top (get-token-triple (caddr token-triple))) (bp-tertiary (get-token-triple (caddr token-triple))))))
      (if next
         (let ((next-token (car next))
            (next-type (cadr next))
            (next-wordlist (caddr next)))
            (if (equal? next-type 'endlist) #f (if (and (equal? next-type 'engword) (equal? next-token key2)) (bp-top (get-token-triple next-wordlist)) #f)))
         #f)))

;;; fDPhrase

(define (d-phrase-parse word-list) (let ((top (dp-top (get-token-triple word-list))))
                                     (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (dp-top token-triple) (let ((tertiary (dp-tertiary token-triple)))
                                (if tertiary
                                    (let ((next-token (car tertiary))
                                          (next-type (cadr tertiary))
                                          (next-wordlist (caddr tertiary))
                                          (larger-dp #f))
                                      (if (and (equal? next-type 'engword) (or (equal? next-token 'AND) (equal? next-token 'OR)))
                                          (set! larger-dp (dp-top (get-token-triple next-wordlist))))
                                      (if larger-dp larger-dp tertiary))
                                    #f)))

(define (dp-tertiary token-triple)
  (case (cadr token-triple)
    ((engword) (dp-tertiary-engword token-triple))
    ((iverb) (get-token-triple (caddr token-triple)))
    ((tverb) (let ((next (get-token-triple (caddr token-triple))))
                                                                                                                                                                         (if (and (equal? (cadr next) 'engword) (or (equal? (car next) 'ITSELF) (equal? (car next) 'HIMSELF) (equal? (car next) 'HERSELF))) (get-token-triple (caddr next)) (subj-top next)))) (else #f)))

(define (dp-tertiary-engword token-triple)
  (case (car token-triple)
    ((BOTH) (dp-bothand-type token-triple 'AND))
    ((EITHER) (dp-bothand-type token-triple 'OR))
    ((NEITHER) (dp-bothand-type token-triple 'NOR)) (else #f)))

(define (dp-bothand-type token-triple key2)
  (let ((next (if (equal? key2 'NOR) (dp-top (get-token-triple (caddr token-triple))) (dp-tertiary (get-token-triple (caddr token-triple))))))
        (if next
            (let ((next-token (car next))
                 (next-type (cadr next))
                 (next-wordlist (caddr next)))
            (if (equal? next-type 'endlist)
                #f
                (if (and (equal? next-type 'engword) (equal? next-token key2))
                    (dp-top (get-token-triple next-wordlist))
                    #f)))
            #f)))


;;;  fNounPhrase

(define (np-parse word-list)
  (let ((top (np-top (get-token-triple word-list))))
    (if top
        (if (equal? (cadr top) 'endlist)
            #t
            #f)
        #f)))

(define (consume-adjectives token-triple)
  (if (equal? (cadr token-triple) 'adj)
      (consume-adjectives (get-token-triple (caddr token-triple)))
      token-triple))

(define (np-top token-triple)
  (let ((next (consume-adjectives token-triple))
        (next-but-one '(() 'endlist ())))
    (if (equal? (cadr next) 'noun)
        (begin (set! next-but-one (get-token-triple (caddr next)))
               (if (and (equal? (cadr next-but-one) 'engword)
                        (equal? (car next-but-one) 'THAT))
                   (rel-clause-top next-but-one)
                   next-but-one))
        #f)))

;;;fRelClause=

(define (rel-clause-top token-triple) (if (and (equal? (cadr token-triple) 'engword) (equal? (car token-triple) 'THAT))
                                          (let* ((next (get-token-triple (caddr token-triple)))
                                                 (alternative next)
                                                 (verb-phrase (vp-top next)))
                                            (if verb-phrase
                                                verb-phrase
                                                (let ((subject (subj-top alternative)))
                                                  (if subject (rv-top subject) #f))))
                                          #f));

;;; fRelVerb

(define (relational-verb-parse word-list) (let ((top (rv-top (get-token-triple word-list))))
                                            (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (rv-top token-triple)
  (case (cadr token-triple)
    ((tverb) (get-token-triple (caddr token-triple)))
    ((engword) (if (equal? (car token-triple) 'IS)
                  (let* ((next (get-token-triple (caddr token-triple)))
                         (next-type (cadr next)))
                      (if (or (equal? next-type 'pverb) (equal? next-type 'binadj))
                         (get-token-triple (caddr next))
                         #f))
                #f))
    (else #f)))

;;;fSentence

(define (sentence-parse word-list) (let ((top (sentence-top (get-token-triple word-list))))
                                     (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (sentence-top token-triple) (let ((tertiary (sentence-tertiary token-triple)))
                                      (if tertiary
                                          (let ((next-token (car tertiary))
                                                (next-type (cadr tertiary))
                                                (next-wordlist (caddr tertiary)))
                                            (if (or (equal? next-token 'AND) (equal? next-token 'OR) (equal? next-token 'IF) (equal? next-token 'UNLESS) (equal? next-token 'ONLY))
                                                (if (or (equal? next-token 'AND) (equal? next-token 'OR) (equal? next-token 'UNLESS))
                                                    (sentence-top (get-token-triple next-wordlist))
                                                    (let* ((second-triple (get-token-triple next-wordlist))
                                                           (second-token (car second-triple))
                                                           (second-type (cadr second-triple))
                                                           (second-wordlist (caddr second-triple))
                                                           (third-triple (get-token-triple second-wordlist))
                                                           (third-token (car third-triple))
                                                           (third-type (cadr third-triple))
                                                           (third-wordlist (caddr third-triple))
                                                           (fourth-triple (get-token-triple third-wordlist))
                                                           (fourth-token (car fourth-triple))
                                                           (fourth-type (cadr fourth-triple))
                                                           (fourth-wordlist (caddr fourth-triple)))
                                                      (if (and (equal? next-token 'IF) (not (equal? second-token 'AND)))
                                                          (sentence-top second-triple)
                                                          (if (and (equal? next-token 'ONLY) (equal? second-token 'IF))
                                                              (sentence-top third-triple)
                                                              (if (and (equal? next-token 'IF) (equal? second-token 'AND) (equal? third-token 'ONLY) (equal? fourth-token 'IF))
                                                                  (sentence-top (get-token-triple fourth-wordlist))
                                                                  #f)))))
                                                tertiary))
                                          #f)))

(define (sentence-tertiary token-triple) (let ((sentence-try (sentence-tertiary-try token-triple)))
                                           (if sentence-try sentence-try (sentence-secondary token-triple))))

(define (sentence-tertiary-try token-triple)
  (case (car token-triple)
    ((BOTH) (sentence-bothand-type token-triple 'AND))
    ((EITHER) (sentence-bothand-type token-triple 'OR))
    ((NEITHER) (sentence-bothand-type token-triple 'NOR))
    ((IF) (sentence-bothand-type token-triple 'THEN))
    ((IT) (it-etc token-triple))
    (else (sentence-secondary token-triple))))

(define (it-etc token-triple) (let* ((second-triple (get-token-triple (caddr token-triple)))
                                     (second-token (car second-triple))
                                     (second-type (cadr second-triple))
                                     (second-wordlist (caddr second-triple))
                                     (third-triple (get-token-triple second-wordlist))
                                     (third-token (car third-triple))
                                     (third-type (cadr third-triple))
                                     (third-wordlist (caddr third-triple))
                                     (fourth-triple (get-token-triple third-wordlist))
                                     (fourth-token (car fourth-triple))
                                     (fourth-type (cadr fourth-triple))
                                     (fourth-wordlist (caddr fourth-triple))
                                     (fifth-triple (get-token-triple fourth-wordlist))
                                     (fifth-token (car fifth-triple))
                                     (fifth-type (cadr fifth-triple))
                                     (fifth-wordlist (caddr fifth-triple))
                                     (sixth-triple (get-token-triple fifth-wordlist))
                                     (sixth-token (car sixth-triple))
                                     (sixth-type (cadr sixth-triple))
                                     (sixth-wordlist (caddr sixth-triple)))
                                (if (and (equal? (car token-triple) 'IT) (equal? second-token 'IS) (equal? third-token 'NOT) (equal? fourth-token 'THE) (equal? fifth-token 'CASE) (equal? sixth-token 'THAT))
                                    (sentence-top (get-token-triple sixth-wordlist))
                                    #f)))

(define (sentence-bothand-type token-triple key2) (let ((next (if (or (equal? key2 'NOR) (equal? key2 'THEN)) (sentence-top (get-token-triple (caddr token-triple))) (sentence-tertiary (get-token-triple (caddr token-triple))))))
                                                    (if next
                                                        (let ((next-token (car next))
                                                              (next-type (cadr next))
                                                              (next-wordlist (caddr next)))
                                                          (if (equal? next-type 'endlist) #f (if (and (equal? next-type 'engword) (equal? next-token key2)) (sentence-top (get-token-triple next-wordlist)) #f)))
                                                        #f)))

(define (sentence-secondary token-triple) (let ((subject (subj-top token-triple)))
                                            (if subject (vp-top subject) #f)))

;;; fSubject

(define (subject-parse word-list) (let ((top (subj-top (get-token-triple word-list))))
                                    (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (subj-top token-triple) (let ((tertiary (subj-tertiary token-triple)))
                                  (if tertiary
                                      (let ((next-token (car tertiary))
                                            (next-type (cadr tertiary))
                                            (next-wordlist (caddr tertiary)))
                                        (if (and (equal? next-type 'engword) (or (equal? next-token 'AND) (equal? next-token 'OR)))
                                            (subj-top (get-token-triple next-wordlist))
                                            tertiary))
                                      #f)))

(define (subj-tertiary token-triple)
  (case (cadr token-triple)
    ((engword) (subj-tertiary-engword token-triple))
    ((name) (get-token-triple (caddr token-triple)))
    ((qU) (qU token-triple))
    ((qR) (np-top (get-token-triple (caddr token-triple))))
    (else #f)))

(define (subj-tertiary-engword token-triple)
  (case (car token-triple)
    ((BOTH) (subj-bothand-type token-triple 'AND))
    ((EITHER) (subj-bothand-type token-triple 'OR))
    ((NEITHER) (subj-bothand-type token-triple 'NOR))
    (else #f)))

(define (subj-bothand-type token-triple key2) (let ((next (if (equal? key2 'NOR) (subj-top (get-token-triple (caddr token-triple))) (subj-tertiary (get-token-triple (caddr token-triple))))))
                                                (if next
                                                    (let ((next-token (car next))
                                                          (next-type (cadr next))
                                                          (next-wordlist (caddr next)))
                                                      (if (equal? next-type 'endlist)
                                                          #f
                                                          (if (and (equal? next-type 'engword) (equal? next-token key2))
                                                              (subj-top (get-token-triple next-wordlist))
                                                              #f)))
                                                    #f)))

(define (qU token-triple) (let* ((next (get-token-triple (caddr token-triple)))
                                 (next-token (car next))
                                 (next-type (cadr next)))
                            (if (and (equal? next-type 'engword) (equal? next-token 'THAT)) (rel-clause-top next) next)))

;;; fVerbPhrase

(define (verb-phrase-parse word-list) (let ((top (vp-top (get-token-triple word-list))))
                                        (if top (if (equal? (cadr top) 'endlist) #t #f) #f)))

(define (vp-top token-triple) (let ((tertiary (vp-tertiary token-triple)))
                                (if tertiary
                                    (let ((next-token (car tertiary))
                                          (next-type (cadr tertiary))
                                          (next-wordlist (caddr tertiary))
                                          (larger-vp #f))
                                      (if (and (equal? next-type 'engword) (or (equal? next-token 'AND) (equal? next-token 'OR)))
                                          (set! larger-vp (vp-top (get-token-triple next-wordlist)))
                                          ) ; this 'if' does not have an else ???
                                      (if larger-vp larger-vp tertiary))
                                    #f)))

(define (vp-tertiary token-triple)
  (case (cadr token-triple) ((engword) (vp-tertiary-engword token-triple))
                            ((iverb) (get-token-triple (caddr token-triple)))
                            ((tverb) (let ((next (get-token-triple (caddr token-triple))))
                                          (if (and (equal? (cadr next) 'engword) (equal? (car next) 'ITSELF))
                                              (get-token-triple (caddr next))
                                              (subj-top next))))
                            (else #f)))

(define (vp-tertiary-engword token-triple)
  (case (car token-triple) ((BOTH) (vp-bothand-type token-triple 'AND))
                           ((EITHER) (vp-bothand-type token-triple 'OR))
                           ((NEITHER) (vp-bothand-type token-triple 'NOR))
                           ((IS) (let ((next (get-token-triple (caddr token-triple))))
                                      (if (and (equal? (cadr next) 'engword) (equal? (car next) 'NOT))
                                          (set! next (get-token-triple (caddr next))))
                                      (bp-top next)))
                           ((DOES) (let ((next (get-token-triple (caddr token-triple))))
                                       (if (and (equal? (cadr next) 'engword) (equal? (car next) 'NOT))
                                           (set! next (get-token-triple (caddr next))))
                                           (dp-top next)))
                          (else #f)))

(define (vp-bothand-type token-triple key2) (let ((next (if (equal? key2 'NOR)
                                                            (vp-top (get-token-triple (caddr token-triple)))
                                                            (vp-tertiary (get-token-triple (caddr token-triple))))))
                                              (if next
                                                  (let ((next-token (car next))
                                                        (next-type (cadr next))
                                                        (next-wordlist (caddr next)))
                                                    (if (equal? next-type 'endlist) #f (if (and (equal? next-type 'engword) (equal? next-token key2)) (vp-top (get-token-triple next-wordlist)) #f)))
                                                  #f)))



; predicates for parts of speech

(define (adj-p word-list) (myassoc word-list *adjectives*))

(define (adjnoun-p word-list) (if (null? word-list) #f (adjnoun-parse word-list)))

(define (atomic-proposition-p word-list) (myassoc word-list *propositions*))

(define (binary-adj-p word-list) (myassoc word-list *binary-adjectives*))

(define (bp-p word-list) (if (null? word-list) #f (b-phrase-parse word-list)))

(define (dp-p word-list) (if (null? word-list) #f (d-phrase-parse word-list)))

(define (noun-p word-list) (myassoc word-list *nouns*))

(define (np-p word-list) (if (null? word-list) #f (np-parse word-list)))

(define (passv-p word-list) (myassoc word-list *passive-verbs*))

(define (proposition-p word-list) (cond ((myassoc word-list *propositions*)) ((let ((shuffle (try-all-the-prop-shuffle-rules word-list)))
                                                                                (not (null? shuffle)))) (#t #f)))

(define (rv-p word-list) (if (null? word-list) #f (relational-verb-parse word-list)))

(define (sentence-p word-list) (if (null? word-list) #f (sentence-parse word-list)))

(define (subject-p word-list) (if (null? word-list) #f (subject-parse word-list)))

(define (term-p word-list) (cond ((myassoc word-list *names*)) ((myassoc word-list gEngVariables)) (#t #f)))

(define (verbp-p word-list) (myassoc word-list *passive-verbs*))

(define (vi-p word-list) (myassoc word-list *intransitive-verb*))

(define (vp-p word-list) (if (null? word-list) #f (verb-phrase-parse word-list)))

(define (vt-p word-list) (myassoc word-list *transitive-verb*))

(define (remember-rule rule) (set! *rules* (cons rule *rules*)))
(define (remember-symbolization rule) (set! *symbolizations* (cons rule *symbolizations*)))

;;;;;;;;;;;;;;; End of Parts of speech parsing


; if the Scheme is being interpreted we might need our own versions
; of functions that are usually built in

(define (accumulate list-items)
  (if (null? list-items)
      empty
      (append (car list-items) (accumulate (cdr list-items)))))

(define (butlast alist n)
  (if (>= n (length alist))
      empty
      (cons (car alist) (butlast (cdr alist) n))))

(define (dotimes count limit body result)
  (if (= count limit)
      result
      (begin body (dotimes (+ count 1) limit body result))))

(define (singleton-last aList)
  (if (null? aList)
      empty
      (if (null? (cdr aList)) aList (singleton-last (cdr aList)))))

(define (myassoc key-list assoc-list)
  (if (null? assoc-list)
      #f
      (if (equal? key-list (caar assoc-list))
          (cadar assoc-list)
          (myassoc key-list (cdr assoc-list)))))

(define (rassoc value assoc-list)
  (if (null? assoc-list)
      #f
      (if (equal? value (cadar assoc-list))
          (caar assoc-list)
          (rassoc value (cdr assoc-list)))))

(define (nthcdr n thelist)
  (if (null? thelist) empty (if (= n 0) thelist (nthcdr (- n 1) (cdr thelist)))))





;;;;;;;;;;;;;; TESTING, input some of these into REPL

; There may seem to be many parentheses here. The host addresses that.


;;;;;;;;;;;;;; Propositional Rules (independent of Predicate Rules

; (try-all-the-prop-rules '(PHILOSOPHY IS HARD))
; ((H))

; (try-all-the-prop-rules '(IT IS NOT THE CASE THAT PHILOSOPHY IS HARD))
; ((chNeg (PHILOSOPHY IS HARD)))

;(try-all-the-prop-rules '(PHILOSOPHY IS HARD AND LOGIC IS HARD))
; (((PHILOSOPHY IS HARD chAnd LOGIC IS HARD)))

; (try-all-the-prop-rules '(IT IS NOT THE CASE THAT PHILOSOPHY IS HARD AND LOGIC IS HARD))
; ((chNeg (PHILOSOPHY IS HARD AND LOGIC IS HARD)) ((IT IS NOT THE CASE THAT PHILOSOPHY IS HARD chAnd LOGIC IS HARD)))   !!! Two results, ambiguous

; (try-all-the-prop-rules '(IT IS NOT THE CASE THAT PHILOSOPHY IS HARD UNLESS LOGIC IS HARD))
; ((chNeg (PHILOSOPHY IS HARD UNLESS LOGIC IS HARD)) ((IT IS NOT THE CASE THAT PHILOSOPHY IS HARD chOr LOGIC IS HARD)))   !!! Two results, ambiguous

;;;;;;;;;;;;;; Predicate Rules

; (try-all-the-rules '(ARTHUR IS ANGRY))
; (((A a)))

; (try-all-the-rules '(IT IS NOT THE CASE THAT ARTHUR IS ANGRY))
; ((chNeg (ARTHUR IS ANGRY)))

; (try-all-the-rules '(BERYL IS DRIVEN BY ARTHUR))
; (((D b a)))

; (try-all-the-rules '(IT IS NOT THE CASE THAT BERYL IS DRIVEN BY ARTHUR UNLESS ARTHUR IS ANGRY))
; ((chNeg (BERYL IS DRIVEN BY ARTHUR UNLESS ARTHUR IS ANGRY)) ((IT IS NOT THE CASE THAT BERYL IS DRIVEN BY ARTHUR chOr ARTHUR IS ANGRY))) !!ambiguous

; (try-all-the-rules '(NEITHER BERYL IS DRIVEN BY ARTHUR NOR ARTHUR IS ANGRY))
;((chNeg (BERYL IS DRIVEN BY ARTHUR chOr ARTHUR IS ANGRY)))

; (try-all-the-rules '(ARTHUR IS CHEERFUL))
; (((C a)))

; (term-p '(X))

; (term-p (CHEERFUL ARTHUR))

;(subject-parse '(CHEERFUL ARTHUR))

;(subject-p '(CHEERFUL ARTHUR))

; (try-all-the-rules '(CHEERFUL ARTHUR IS ANGRY))  !! no rule for this form

; (try-all-the-rules '(BERYL IS DRIVEN BY CHEERFUL ARTHUR)) !! no rule for this form
; ()
; (passv-p '(DRIVEN BY))
; D
; (verbp-p '(DRIVEN BY))
; D
; (term-p '(BERYL))
; b
; (term-p '(CHEERFUL ARTHUR))
; #f

; (try-all-the-rules '(EVERY PHILOSOPHER IS A NINCOMPOOP))
; (((chUniquantx) (x IS A PHILOSOPHER chImplic x IS A NINCOMPOOP)))

; (try-all-the-rules '(X studies))

; (assoc '(X) gEngVariables)
; ((X) x)

; (try-all-the-rules '(X STUDIES))
; (((S x)))

; (try-all-the-rules '(Y THINKS))
; (((T y)))

; (try-all-the-rules '(IT IS NOT THE CASE THAT Z IS BOLD))
; ((chNeg (Z IS BOLD)))

; (try-all-the-rules '(Z IS A CHEERFUL NINCOMPOOP))
; (((Z IS CHEERFUL chAnd Z IS A NINCOMPOOP)))






; (try-all-the-rules '(EVERYTHING THINKS))
;(((chUniquantx) (x THINKS)))

; (try-all-the-rules '(SOMETHING THINKS))
; (((chExiquantx) (x THINKS)))

; (try-all-the-rules '(NOTHING THINKS))
;((chNeg (chExiquantx) (x THINKS)))

; (try-all-the-rules '(EVERYTHING IS CHEERFUL))
; (((chUniquantx) (x IS CHEERFUL)))

; (try-all-the-rules '(SOMETHING IS CHEERFUL AND BOLD))
; (((chExiquantx) (x IS CHEERFUL AND BOLD)))



; (try-all-the-rules '(EVERY NINCOMPOOP IS A PHILOSOPHER))
; (((chUniquantx) (x IS A NINCOMPOOP chImplic x IS A PHILOSOPHER)))

; (try-all-the-rules '(SOME PHILOSOPHER IS A NINCOMPOOP))
; ((chExiquantx) (x IS A PHILOSOPHER chAnd x IS A NINCOMPOOP)))

; (try-all-the-rules '(SOME BOLD PHILOSOPHER IS A NINCOMPOOP))
; (((chExiquantx) (x IS A BOLD PHILOSOPHER chAnd x IS A NINCOMPOOP)))

; (try-all-the-rules '(NO NINCOMPOOP IS A PHILOSOPHER))
; ((chNeg (chExiquantx) (x IS A NINCOMPOOP chAnd x IS A PHILOSOPHER)))

; (try-all-the-rules '(EVERYTHING THAT THINKS STUDIES))
; (((chUniquantx) (x THINKS chImplic x STUDIES)))

; (((chUniquantx) (x THINKS chImplic x STUDIES)))


; (try-all-the-rules '(NOTHING THAT STUDIES THINKS))
; ((chNeg (chExiquantx) (x STUDIES chAnd x THINKS)))

; (try-all-the-rules '(EVERYTHING THAT BOTH STUDIES AND THINKS IS CHEERFUL))
; (((chUniquantx) (x BOTH STUDIES AND THINKS chImplic x IS CHEERFUL)))

; (try-all-the-rules '(NOTHING THAT THINKS IS EITHER ANGRY OR CHEERFUL))
; ((chNeg (chExiquantx) (x THINKS chAnd x IS EITHER ANGRY OR CHEERFUL)))


; https://softoption.us/node/622  Ex1

; (try-all-the-rules '(ARTHUR ANNOYS BERYL))
; (((A a b)))

; (try-all-the-rules '(BERYL ANNOYS ARTHUR))
; (((A b a)))

; (try-all-the-rules '(ARTHUR IS ENCOURAGED BY BERYL))
; (((E a b)))

; (try-all-the-rules '(ARTHUR IS NOT SMARTER THAN BERYL))
; ((chNeg (ARTHUR IS SMARTER THAN BERYL)))

; (try-all-the-rules '(ARTHUR ANNOYS HIMSELF))
; (((A a a)))

; (try-all-the-rules '(IF CHARLES BRINGS ARTHUR THEN CHARLES ANNOYS BERYL))
; (((CHARLES BRINGS ARTHUR chImplic CHARLES ANNOYS BERYL)))


; https://softoption.us/node/622  Ex2


; (try-all-the-rules '(EVERYTHING THAT ARTHUR IS SMARTER THAN THINKS))
; (((chUniquantx) (ARTHUR IS SMARTER THAN x chImplic x THINKS)))

; (try-all-the-rules '(SOMETHING THAT ARTHUR IS RUDER THAN IS A CHEERFUL NINCOMPOOP))
; (((chExiquantx) (ARTHUR IS RUDER THAN x chAnd x IS A CHEERFUL NINCOMPOOP)))

; (try-all-the-rules '(ARTHUR IS SMARTER THAN EVERYTHING THAT BERYL IS SMARTER THAN))
; (((chUniquanty) (BERYL IS SMARTER THAN y chImplic ARTHUR IS SMARTER THAN y)))

; (try-all-the-rules '(IF CHARLES BRINGS ARTHUR THEN CHARLES ANNOYS BERYL))
; (((CHARLES BRINGS ARTHUR chImplic CHARLES ANNOYS BERYL)))

; (try-all-the-rules '(CHARLES IS ENCOURAGED BY SOMETHING THAT BERYL IS ENCOURAGED BY))
; (((chExiquanty) (BERYL IS ENCOURAGED BY y chAnd CHARLES IS ENCOURAGED BY y)))

; (try-all-the-rules '(ARTHUR IS SMARTER THAN NOTHING THAT BERYL IS ENCOURAGED BY))
; ((chNeg (chExiquanty) (BERYL IS ENCOURAGED BY y chAnd ARTHUR IS SMARTER THAN y)))

; (try-all-the-rules '(ARTHUR IS ENCOURAGED BY EVERYTHING THAT STUDIES))
; (((chUniquanty) (y STUDIES chImplic ARTHUR IS ENCOURAGED BY y)))

; (try-all-the-rules '(ARTHUR ANNOYS SOMETHING THAT STUDIES))
; ((chExiquanty) (y STUDIES chAnd ARTHUR ANNOYS y)))


; https://softoption.us/node/622  Ex3


; (try-all-the-rules '(EVERYTHING IS SMARTER THAN EVERYTHING))
; (((chUniquantx) (x IS SMARTER THAN EVERYTHING)))

; (try-all-the-rules '(EVERYTHING IS SMARTER THAN SOMETHING))
; (((chUniquantx) (x IS SMARTER THAN SOMETHING)))

; (try-all-the-rules '(SOMETHING IS SMARTER THAN EVERYTHING))
; (((chExiquantx) (x IS SMARTER THAN EVERYTHING)))

; (try-all-the-rules '(EVERYTHING IS SMARTER THAN NOTHING))
; (((chUniquantx) (x IS SMARTER THAN NOTHING)))

; (try-all-the-rules '(NOTHING IS SMARTER THAN EVERYTHING))
; ((chNeg (chExiquantx) (x IS SMARTER THAN EVERYTHING)))

; (try-all-the-rules '(SOMETHING IS SMARTER THAN SOMETHING))
; (((chExiquantx) (x IS SMARTER THAN SOMETHING)))

; (try-all-the-rules '(SOMETHING IS SMARTER THAN NOTHING))
; (((chExiquantx) (x IS SMARTER THAN NOTHING)))

; (try-all-the-rules '(NOTHING IS SMARTER THAN SOMETHING))
; ((chNeg (chExiquantx) (x IS SMARTER THAN SOMETHING)))

; (try-all-the-rules '(NOTHING IS SMARTER THAN NOTHING))
; ((chNeg (chExiquantx) (x IS SMARTER THAN NOTHING)))


; https://softoption.us/node/622  Ex4


; (try-all-the-rules '(SOMETHING IS NOT A PHILOSOPHER))
; (((chExiquantx) (x IS NOT A PHILOSOPHER)))

; (try-all-the-rules '(SOME CHEERFUL PHILOSOPHER IS NOT A NINCOMPOOP))
; (((chExiquantx) (x IS A CHEERFUL PHILOSOPHER chAnd x IS NOT A NINCOMPOOP)))

; (try-all-the-rules '(SOMETHING IS SMARTER THAN EVERYTHING))
; (((chExiquantx) (x IS SMARTER THAN EVERYTHING)))

; (try-all-the-rules '(EVERYTHING IS SMARTER THAN SOMETHING THAT STUDIES))
; (((chUniquantx) (x IS SMARTER THAN SOMETHING THAT STUDIES)))


