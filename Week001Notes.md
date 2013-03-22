# _Land of Lisp_ - Week 001 Notes

# Chapter 01 - Getting started with Lisp (p.15)

* A quick overview of picking a lisp, (recommends CLisp)
* Mentions Clojure and Arc

**Quick note: if things go wierd in the repl, enter the command :q**

# Chapter 02 - Your first program (p. 21)

The first program is a number guessing game.  The game is meant to be played in the repl.

* You pick a number.
* The computer makes a guess
* You ask the computer to guess a smaller or bigger number

see [guess.lisp](clisp/guess.lisp) for the program.

## What I Learned

### defparameter
	(defparameter *big* 100)
Creates a global variable with a name and a value

### defun
	(defun hello-world (name)
		(format t "Hello, ~a" name))
Creates a function with a name, parameter list, and body

### let
	(let ((a 5)
		  (b 6))
		 (+ a b))
Defines a scoped variable for local use

### flet
	(flet ((add-ten (n) (+ n 10)))
		  (add-ten 5))
Like let but specifically for functions

### labels
	(labels ((a (n) (+ n 5))
		     (b (n) (+ (a n) 6)))
		    (b 10))
Like flet but allows recursion or mutual function calls

### ash
	(ash 11 -1)
Binary shift of a number.  Above, 11 -> 5.  for a positive 2nd argument, 11 -> 22

# Chapter 03 - Exploring the Syntax of Lisp Code (p. 31)
[S-Expression](http://en.wikipedia.org/wiki/S-expression)

Basically, a nested tree of atoms or list of atoms.
## What I learned

### Symbols
	'foo
A symbol is a self-referential "constant". Like a :foo in ruby except they are case-insensitive.

### Numbers
Lisp converts ints to floats automatically.  It also seems to promote to bigints.

### Strings
	"Hello"
Quoted character string, just like every other language.

### Lists
	'(some number of atoms)
When started with a single quote, the list is interpreted solely as data, IE the first atom is not interpreted as a function to be evaluated

#### Cons
	(cons 'a ()) => '(a)
	(cons 'a '(a)) => '(a a)
	(cons 'a 'b) => '(a . b)
constructs a list by pre-pending a value on to an existing list

#### Car
	(car ()) => nil
	(car '(a)) => 'a
	(car '(a b c)) => 'a
Returns the first part of a cons cell, the head of a list usually

#### Cdr
	(cdr ()) => nil
	(cdr '(a)) => nil
	(cdr '(a b)) => '(b)
	(cdr '(a . b)) => 'b
Returns the tail of a list, or the second part of a cons cell

#### cdaddr et al.

short hand for (crd (car (cdr (cdr some-list)))) used for navigating nested lists

#### list
	(list 'a 'b 'c) => '(a b c)
constructs a list from the arguments
