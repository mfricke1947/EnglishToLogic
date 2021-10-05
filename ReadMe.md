

# Introduction

This translates English into Propositional or Predicate Logic, primarily in the context of teaching logic in a college course.

# Running the code

The code is written in Scheme, a LISP dialect. It was written to run on its own interpreter and so it uses only a subset of Scheme. There are different dialects of Scheme (for example, some have the empty list '() and others have the defined constant 'empty).

This code will run on Racket (DrRacket). Download from

https://racket-lang.org/

Use the 'Pretty Big' language option. This may help you with Racket https://www.youtube.com/watch?v=oQD01ybB1z4

# Some other notes

There are 3 ideas behind what is going on.
1) The well known AI program Eliza can produce
    a chatbot that produce one-step one-reply
    conversations like:
      you: I feel sick
      Eliza: Please tell me why you feel sick
      etc.
   What is going on here is that Eliza has rules
   containing patterns eg
     Input: I feel (?X) Reply: Please tell me why you feel (?X)
   and a pattern matcher (and a randomizer).
   Eliza matches the input, if it can, and
   returns a random choice among the matching outputs
2) Symbolizing logic to predicate calculus level can
   be done by a repeated one-step divide and conquer
   technique e.g.
      We reduce unemployment and we increase health costs
      We reduce unemployment & we increase health costs
      R & we increase health costs
      R & I
3) Each step in (2) can be done by a pattern matching rule
    e.g.  Input: (?P1) and (?P2) Reply: (?P1) & (?P2)
   However, this is going to be more complicated than
   Eliza because not any old pattern match will do. In the
   example case, the (?P)s have to be propositions i.e. there
   is a guard or predicate on the pattern which has to be
   satisfied.
   Then it just gets a lot more complicated. For example,
   the rule
      (general-match ( ((? Term) (lambda (s) (term-p s)))
                     ((? RV) (lambda (s) (rv-p s))) ((? _)
                     (lambda (s) (equal? '(NEITHER) s )))
                     ((? Subj1) (lambda (s) (subject-p s))) ((? _)
                     (lambda (s) (equal? '(NOR) s )))
                     ((? Subj2) (lambda (s) (subject-p s))) ) 
                     (  chNeg ((? Term) (? RV) (? Subj1) chOr (? Term) (? RV) (? Subj2))))
   uses patterns with guards for terms, subjects,relational verbs, literal  "neither" and literal "nor".
   And it will translate, e.g.
   Input: Betty is taller than neither Ann nor Chloe
   Output: ~ ((Betty is taller than Ann) v (Betty is taller than Chloe))

This Scheme code, or similar, is buried in a host (e.g. a web page). The host
can supply selected text and overwrite the selected text (say on button
press). The User just selects and overwrites, selects and overwrites, etc.
until the entire initial English sentence is symbolized.

The host also has a LISP interpreter, which runs the LISP. One consequence
of this is that the LISP code uses a limited subset of what would be
available in a fully-fledged LISP system. So the LISP code here
may be considered to be long-winded.

Attention needs to be paid to UPPER and LOWER case. Of course, the User
will write whatever the User wishes. The LISP will be case-sensitive
in its matching e.g. "not" won't match with "NOT". What happens in
the running systems is that the input is massaged a little (e.g. periods
are discarded (LISP does not like them)). The tokens in the input are
supplied to the LISP separated by single spaces. The text is all converted
to UPPER case. This does not effect the User's text on the page.
But the overwrite is UPPER case (which slightly highlights it).
[It is standard in Common-LISP for the reader to convert all input to
UPPER case internally, so it appears to be case insensitive.]

In the plain Scheme dialect of LISP running here, there is no massaging 
in the example tests. So there may be some oddity in making sure the
text is in the right form for the rules.

Racket can run different Schemes, even case insensitive ones.
But they mostly have other features that our code is
inconsistent with. In Racket, the code here has been run
using the 'Pretty Big' choice of language. That probably
would be the best choice for you.

The host (web page, say) looks after the symbols for the logical connectives
which can be different for the different systems. Here
you might see 'chNeg' (for the negation character).

You can look at the program running in a Web page
at https://softoption.us/node/610 . Scroll down
and watch the video.

Some of the grammar rules were inspired by a paper
mf heard at the 1990(?) Computers and Philosophy (CAP) Conference. 
I would cite that paper here but I cannot remember its title
or authors (Hannah?).

Sorry about some of the formatting. There aren't any good LISP
formatters, and the Scheme here was brought in inline from its
initial home as quoted strings in Pascal code.
