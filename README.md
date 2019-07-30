All the code is written in Racket language(DrRacket)

The purpose of this Project is twofold:
1. Understand regular expressions as a way to describe strings with certain
properties.
2. Implementing a matcher (more correctly a recognizer) for regular expressions. In the process, we had to program with trees.

Structure of the Project:

1. The file declarations.rkt contains the struct declarations to represent (i) regular expressions in tree form (b) (ii) the graph and (iii) The
edges (or transitions) of the graph.

2. The file utilities.rkt gives you several utility functions to make your
life easier. For instance, it gives you a function maketree to convert
regular expressions to tree form along with labels:
>(maketree "(a|b)*bba | cc*")

(Then (Or (Then (Then (Then (Star (Or (Literal "a" 1)
(Literal "b" 2) 3) 4)
3
(Literal "b" 5) 6)
(Literal "b" 7) 8)
(Literal "a" 9) 10)
(Then (Literal "c" 11)
(Star (Literal "c" 12) 13) 14) 15)
(Literal "#" 16) 17)

3. How is the graph represented? Well, here is the struct:

(struct Graph(greennode nodes trans rednodes symbols)
#:transparent)
