# _Land of Lisp_ - Week 002 Notes

# Chapter 04 - Making Decisions with Conditions (p.49)

Equality, true/false, logical operations, and conditional code structures.

## What I Learned

### false
     nil
     'nil
     ()
     '()
Nil and empty lists are considered false.  As is the #f atom.

**verify #f is false**

### if
     (if (= (+ 1 2) 3)
        'Yup
        'Nope)
Evaluates the first form if the conditional is true.  Evaluates the second form if the conditional is false.

### progn
     (progn (setf *small* 1)
               some-value))

Allows multiple forms to be evaluated when a single form is expected.

### cond
     (cond ((eq person 'henry) (do-henry-things))
                t '(why you eat my pudding stranger?)))

Each test has a series of forms to evaluate if that test is true.  It short circuits once the first test is passed.

### case
    (case person
             ((henry) '(curse you lisp alien - you ate my pudding)))
             (otherwise ’( why you eat my pudding stranger?)))

Simplifies cond where the tests are all equal.

### and

### or

### oddp
A predicate used to test if a number is odd.

### member
    (member 1 '(1 2 3))

Tests a value for membership in a list.

### eq
Used to compare symbols

### eql
Compares symbols, numbers, and characters

### equal
Compares most everything


# chapter 5 - Building a Text Game Engine (p.67)

There is a lot of getting used to lisp in this chapter.  It sets up the wizard game's basic functionality and data structures.

## What I Learned

### association list
    ((some-key some-value)
     (another-key a-different-value))

A list if lists that associates some value with a lookup key

### assoc
   (assoc 'garden *nodes*)
    > (garden garden-data)

Returns the key value pair from an associated list, or nil if the key wasn't found.

### quasiquoting
    `(There is a ,(caddr edge) going ,(cadr edge) from here)

When quoting with a back-tick instead of a single quote, a comma can cause the evaluation of select forms.

### mapcar
    (mapcar #'sqrt '(1 2 3 4 5))

Creates a new list by applying the function to the elements of the source list.

### append
   (append '(Mary had) '(a) '(little lamb))

Joins several lists in to a single list.

### apply
     (apply #'append '((Mary had) '(a) '(little lamb)))

Used to pass a list to a function as its argument list, not as a single argument.

### labels
    (labels ((describe-obj (obj) `(you see a ,obj on the floor.)))
        (apply #'append (mapcar #'describe-obj (objects-at loc objs objs-loc))))

Encapsulates functions locally so they cannot be used by external functions.

### keyword parameters
    (find 'y '((5 x) (3 y) (7 z)) :key #'cadr)

A colon prefixed name followed by a value used in some function calls.

### graphs

Graphs in the game are represented as nodes.  The edges between the nodes are kept in a separate data structure than the nodes themselves.

# Chapter 6 - Interacting with the world: reading and printing in Lisp (p. 85)

Basic IO features of lisp

## What I Learned

### print
    (print "foo")

Displays a string to the terminal.  Automatically appends a new-line character.

### print1
    (print1 "hello")

Like print, but without the new-line.

### read
    (let ((name (read)))
          ...)

Gets the value of a string from the terminal.

### printc
    (printc "thing")

Prints characters to the terminal.  While print will allow them to be _read_ back in, printc will just print the data.

### read-line
    (read-line)

Eats terminal input up to the newline and turns it into a string.

# Chapter 6.5 - Lambda: a function so important it deserves its own Chapter (p. 103)

Anonymous functions

## What I Learned

### lambda
    (lambda (x) (+ 2 x))

Creates and returns an 'anonymous' function

# Code review - [wizard.lisp](lisp/wizard.lisp)

Possible duplication of data structures: \*objects\* - no infomration not in \*object-locations\*

What _IS_ the win of s-expressions over strings?  It seems that the text tweak function is just a crappy templating engine.  It's possibly used to show quasi-qoting, but I'm not really excited about it.

There are three ways of dealing with _helper functions_.
  * walk uses <code>label</code>
  * describe-paths uses a peer defined <code>describe-path</code>
  * game-read uses <code>flet</code>

# Talking points

  REPL vs Event loop
  Scoping functions as encapsulation
  S-Expressions vs Strings/templates
