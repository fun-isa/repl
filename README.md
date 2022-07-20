# repl
A simple compiler from lambda to Structured Combinators


## Build Instructions
Run: 
   cabal build
   
The executable file should be generated at ./dist/iwu

## Usage Instructions

After loading the iwu executable file, try a few examples in Wu:
   wu> succ x = + 1 x
   
The REPL should reply with a compiled expression:
   succ = +1
   
Try another example:
   wu> flip x f g = x g f
   
The REPL should reply with
   flip = C3T2[0,2,1]
   
C3T2[0,2,1] is a structured combinator, a lifted higher-order function defined in terms of graph transformations. In this case, the function flip compiles to a combinator of arity 3, type 2.

Now, a more complex function:
   wu> foldr f z ls = ls (z) (\h t -> f h (foldr f z t))
   
   foldr = C4T14[3,2,0,1,2](foldr_r)
   foldr_r = C5T40[1,3,0,1,2,4](foldr)
   
In this case, the compiler has lifted the internal lambda abstraction (\h t -> f h (foldr f z t)) to an auxiliary function foldr_r. Nested lambdas are useful on programs at a high-level language, but complicate the compilation process (especially when using a variable abstraction algorithm). Lambda-lifitng solves this problem, separating nested lambdas into standalone functions. 

Type :env to check the list of functions defined in this session.
    wu> :env
    [succ = +1 , flip = C3T2[0,2,1], foldr = C4T14[3,2,0,1,2](foldr_r), foldr_r = C5T40[1,3,0,1,2,4](foldr) ]  


To quit, type :q


## Check the wiki

A tutorial presentation of the compiler, describing the process of converting lambda terms to structured combinators is being prepared and will soon be available at the project [wiki](http://wiki.fun-arch.org).




   

