# _Land of Lisp_ - Week 004 Notes

# Chapter 09 - Advanced Datatypes and Generic Programming (p.151)

Arrays, Structs, Hashes, and an intro to multimethods

## What I Learned

### make-array
    (make-array 3)

constructs an array with three cells

### aref
    (aref x 1)

Array indexed lookup of a value

### Array assignment 
    (setf (aref x 1) 'foo)

Sets an array member to the specified value

### make-hash-table
    (make-hash-table)

Constructs a hash table

### get-hash
    (get-hash 'key h)

Hash key based lookup

### Hash assignment
    (setf (get-hash 'key h) '25)

Sets a value equal to a key in a hash

### Multiple return values
    (defparameter foo (values 3 7))
    (multiple-value-bind (a b) (foo)
        (* a b))

Some functions can return multiple values.

### Structs
    (defstruct person name age waist-size favorite-color)
    (defparameter *bob*
      (make-person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
    (person-age *bob*)
    (setf (person-age *bob*) 36)

Structured data with parameter names

### Sequences

* **find-if**
* **count**
* **position**
* **some**
* **every**
* **reduce**
* **sum**
* **subseq**
* **sort**

### dotimes
    (dotimes (i 3)
        (fresh-line)
        (princ i)
        (princ ". Hatchoo!"))

### Multimethods

    ; define a struct
    (defstruct monster (health (randval 10)))
    ; how to show a monster
    (defmethod monster-show (m)
        (princ "A fierce ")(princ (type-of m)))

    ; a subclass of monster
    (defstruct (orc (:include monster)) (club-level (randval 8)))
    (defmethod monster-show ((m orc))
        (princ "A wiched orc with a leve ")
        (princ (orc-club-level m))
        (princ " club"))

## Code Review
