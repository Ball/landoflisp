# _Land of Lisp_ - Week 003 Notes

# Chapter 07 - Going Beyond Basic Lists (p.107)

## What I Learned

### Dotted Lists
    '(1 . (2 . (3 . nil)))

Dotted lists are what happens when consing something that isn't a list.

### Pairs
    (cons 2 3)
    > (2 . 3)
    
The underlying structure of lisp is the cons cell.  It has a car value and a cdr value.

### Circular Lists
	(setf *print-circle* t)
	(defparameter foo '(1 2 3))
	(setf (cdddr foo) foo)

You can set the cdr of a cons cell to a cell further up the list creating a circular structure.
### Association Lists
    (defparameter *drink-order* '((bill . double-espresso)
                                  (lisa . small-drip-coffee)))
                                                          
A list if key value pairs.
 
 ### assoc

	(assoc 'lisa *drink-order*)
 
 Returns the first key/value pair found using the provided key.
 
 ### push

	(push ’(lisa . large-mocha-with-whipped-cream) *drink-order*)
 
 Adds a pair at the head of the list.
 
 ### Trees
 
 	’((walls (mortar (cement)
 				     (water)
 				     (sand))
 		      (bricks))
 	   (windows (glass)
 	   		    (frame)
 	   		    (curtains))
 	   (roof (shingles)
 	   	     (chimney)))
    
Nested lists are used for trees.

### dot files
    digraph { a -> b; }
 
Graph declaration language that is used for visualization via GraphViz 

### substitute-if
    (substitute-if #\e #'digit-char-p "I'm a l33t hack3r!")

Character substitutions in a string when they match the predicate.

### complement
    (complement #'alphanumericp)

Inverts an existing predicate.

### with-open-file
    (with-open-file (*standard-output*
                                  fname
                                  :direction :output
                                  :if-exists :supersede)
              (funcall thunk))
 
 Opens, and possibly replaces, a file stream tied to a file name.
 
 ### thunk
 
 A zero arity function used to delay an operation until later.
 
 ### Keyword Parameters
 
 A keyword is a constant.  When used in argument lists, it is used for expressing optional arguments.

# Chapter 08 - This Ain't Your Daddy's Wumous (p.129)

## What I Learned
 
### load
	(load "graph-util")

Loads a lisp file withy the base name that matches the string supplied.

### loop
	(loop repeat 10
		collect 1)

Repeat 10 times and emit a 1 each time

	(loop for n from 1 to 10
		collect n)

Repeat 10 times and emit the value each time.

	(loop for n from 1 to 10
		collect (+ 100 n))

Repeat 10 times and emit 100 plus the value each time.

###  set-difference
	(set-difference '(1 2) '(3 2))

Returns a list if items in one list but not the other.

###  intersection
	(intersection '(1 2) '(3 2))

Returns a list of items in both lists.

###  remove duplicates
	(remove duplicates '(1 2 1 4 6 2))

###  let*
	(let* ((a 'init) (b a)) ...)

A let form like more like clojure's

#  Code Review [graph_util.lisp](clisp/graph_util.lisp)

For the most part, this is a fairly straight forward library.  It would be nice if there were facilities for better handling private vs exported functions.

#  Code Review [wumpus.lisp](clisp/wumpus.lisp)





### code
  sample

stuff about code

# Code review - [thing.lisp](clisp\thing.lisp)

# Talking points

* one
* two
