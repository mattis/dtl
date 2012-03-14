
# a Dependently Typed Language

In this FoPL'11 project  we are going to implement a dependently typed
programming language based on [Agda][1]. We want to learn how a
dependent type checker works. The plan is to begin with implementing
the a language with lambda abstractions, dependent pairs and unit as
the only built in data types. Then, depending on how much time is
left, we will proceed with adding algebraic data types and pattern
matching.

## Progress
To get a feel of how dependent type checking works we have started with implementing [System λP][2]

[1]: http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf "Ulf Norell. Towards a practical programming language based on dependent type theory. PhD Thesis. Chalmers University of Technology, 2007."
[2]: http://store.elsevier.com/Lectures-on-the-Curry-Howard-Isomorphism/Morten-Heine-S-rensen/isbn-9780444520777/ "Lectures on the Curry-Howard Isomorphism, Sørensen and Urzyczyn, 2006."