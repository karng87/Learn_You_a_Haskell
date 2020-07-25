## Reader  [: link](http://learnyouahaskell.com/for-a-few-monads-more#reader)
![Gun](http://s3.amazonaws.com/lyah/revolver.png)

In the chapter about applicatives, we saw that the function type,  
**(->) r** is an instance of Functor.

Mapping a function f over a function g   
  will make a function that takes the same thing as g,   
  applies g to it and then  
  applies f to that result. 

So basically,  
we're making a new function that's like g,   
only before returning its result, f gets applied to that result as well.  
For instance:  

    ghci> let f = (*5)  
    ghci> let g = (+3)  
    ghci> (fmap f g) 8  
     55

We've also seen that functions are applicative functors.  
They allow us to operate on the eventual results of functions  
as if we already had their results.  
Here's an example:

    ghci> let f = (+) <$> (*2) <*> (+10)  
    ghci> f 3  
    19 


