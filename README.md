# Crossword Solver

*Disclaimer*: Some of the code, e.g. all of **the checker code is the
intellectual property of the PP-Team.** Because I found other people have little
interest in Prolog **I did not put effort in translating the source code to
English.** Feel free to open a issue on this subject and I will help you.

![crossword](https://upload.wikimedia.org/wikipedia/commons/1/14/CrosswordUSA.svg)


Table of Contents
=================

* [Crossword Solver](#crossword-solver)
   * [1. Repository description](#1-repository-description)
   * [2. Usage](#2-usage)
   * [3. Acknowledgments](#3-acknowledgments)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)

## 1. Repository description

This project provides solutions for the third homework for the
[Programming Paradigms](https://ocw.cs.pub.ro/courses/pp) course taught at
[Politehnica University of Bucharest](https://upb.ro), during the second
semester of the university year 2020-2021.

Homework description can be read
[here](https://ocw.cs.pub.ro/courses/pp/21/teme/prolog-integrame).

Because this project was build in stages, you will find in each stage* sub
folder the degree of completion. In the final stage the code takes less then 3
seconds to complete under a stress test.

It was tested only using [SWI-Prolog](https://www.swi-prolog.org)

**The code written by me can be read in [integrame.pl](stage3/integrame.pl).**

For more information or a syllabus check out the course description
[page here](https://cs.pub.ro/index.php/education/courses/59-under/an2under/114-programming-paradigms)
. This course was taught by Mihnea-Cosmin Muraru.

## 2. Usage

Testing with the homework checker:

```bash
user@user-pc:~$ prolog integrame.pl 
?- test_mode(detailed).
true.

?- vmtest.
```

The main predicate `rezolvare(integ(H, W, L, Voc), Sol)` takes in a crossword,
`integ`, with height `H`, width `W`, a list, `L`, of cell types, and a
vocabulary `Voc`. The second argument could be given, or built.

See how the input is [build](stage3/input.pl) for more details.

## 3. Acknowledgments

I would like to thank Mihnea Muraru for all his support.
