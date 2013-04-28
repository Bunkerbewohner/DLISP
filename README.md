DLISP
=====

Lisp-like interpreted programming language implemented in Delphi. 
Uses Automatic-Reference-Counting for Memory Management (no cyclic reference detection yet).
The syntax and function names are inspired by Clojure and LISP. 
Expressions as function arguments are currently evaluated eagerly, not lazily. 
Macros and built-in functions however can control when exactly expressions are evaluated.

Built-in Functions (current state)
----------------------------------

* (def &lt;symbol&gt; &lt;exp&gt;) - define a new variable in the current scope
* (type &lt;exp&gt;) - returns the internal data type of the expression result
* (print &lt;exp1&gt; &lt;exp2&gt; ...) - prints each expression in a new line to stdout
* (list &lt;exp1&gt; &lt;exp2&gt; ...) - creates a list of values
* (quote &lt;exp&gt;) - returns the expression unevaluated
* (fn (list &lt;symbol&gt; ... & &lt;symbol&gt;) &lt;exp1&gt; ... <expn>) - creates a new function
* (__cfg &lt;symbol&gt; &lt;exp&gt;) - function for reading and setting internal configuration
