## For a Few Monads More
![gang](http://s3.amazonaws.com/lyah/clint.png)
We've seen 
  how monads can be used to take values with contexts 
    and apply them to functions 
    and 
  how using **>>=** or **do** notation allows us to focus on the values themselves 
    while the context gets handled for us.

We've met the Maybe monad 
        and 
      seen how it adds a context of possible failure to values. 

We've learned about the list monad 
              and 
      saw how it lets us easily introduce **non-determinism** into our programs. 

We've also learned how to work in the IO monad, 
      even before we knew what a monad was!

In this chapter, 
we're going to learn about a few other monads. 

We'll see how they can make our programs clearer 
    by letting us treat all sorts of values as monadic ones. 

Exploring a few monads more 
    will also solidify our intuition for monads.

The monads that we'll be exploring 
    are all part of the mtl package. 

A Haskell package is a collection of modules. 

The mtl package comes with the Haskell Platform, 
    so you probably already have it. 

To check if you do, type **ghc-pkg list (stack exec ghc-pkg list)** in the command-line. 
This will show which Haskell packages you have installed 
    and one of them should be mtl, followed by a version number.

---
### Writer? I hardly know her!

We've loaded our gun with the Maybe monad, the list monad and the IO monad. 

Now let's put the **Writer monad** in the chamber 
                and 
          see what happens when we fire it!

Whereas Maybe is for values with an added context of failure 
                        and 
        the list is for non-deterministic values, 
        the Writer monad 
            is 
            for values that have another value attached 
                       that acts as a sort of log value. 

**Writer** allows us to do computations 
    while making sure that all the log values are combined into one log value 
                      that then gets attached to the result.

For instance, 
we might want to equip our values with strings that explain what's going on, 
  probably for debugging purposes. 

Consider a function that takes a number of bandits(무법자) in a gang 
                                and 
                         tells us if that's a big gang or not. 

That's a very simple function:
    isBigGang :: Int -> Bool
    isBigGang x = x > 9

Now, what if instead of just giving us a True or False value, 
we want it to also return a log string that says what it did? Well, 
we just make that string and return it along side our Bool:
    isBigGang :: Int -> (Bool, String)
    isBigGang x = (x > 9, "Compared gang size to 9.")

So now instead of just returning a Bool, 
we return **a tuple** where the first component of the tuple is the actual value 
  and the second component is the string that accompanies(수반하는) that value. 
There's some added context to our value now. 
Let's give this a go:
    ghci> isBigGang 3
    (False, "Compared gang size to 9.")
    ghci> isBigGang 30
    (True, "Compared gang size to 9.")

So far so good. isBigGang takes a normal value and returns a value with a context. 

As we've just seen, 
feeding it a normal value is not a problem. 

Now what if we already have a value that has a log string attached to it, 
such as (3, "Smallish gang."), 

and we want to feed it to isBigGang? 

It seems like once again, 
we're faced with this question: 

if we have a function that takes a normal value and returns a value with a context, 
how do we take a value with a context 
and 
feed it to the function?


When we were exploring the Maybe monad, 
we made a function applyMaybe, 
which took a Maybe a value and 
           a function of type a -> Maybe b 
  and fed that Maybe a value into the function, 
    even though the function takes a normal a instead of a Maybe a. 

It did this by minding the context that comes with Maybe a values, 
which is that they are values with possible failure. 

But inside the **a -> Maybe b** function, 
we were able to treat that value as just a normal value, 
because applyMaybe (which later became >>=) took care of 
          checking if it was a Nothing or a Just value.

In the same vein, 
let's make a function 
  that takes a value with an attached log, that is, an (a,String) value 
                        and 
             a function of type a -> (b,String) 
                        and 
        feeds that value into the function. 

We'll call it applyLog. 

But 
because an (a,String) value doesn't carry with it a context of possible failure, 
  but rather a context of an additional log value, 

  applyLog is going to make sure that the log of the original value isn't lost, 
but is joined together with the log of the value that results from the function. 

Here's the implementation of applyLog:

    applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
    applyLog (x, log) f = let (y, newLog) = f x
                          in (y, log ++ newLog)

When we have a value with a context 
              and
     we want to feed it to a function, 
we usually try to separate the actual value from the context 
                    and then 
           try to apply the function to the value 
                    and then 
           see that the context is taken care of. 

In the Maybe monad, 
we checked if the value was a Just x and if it was, 
we took that x and applied the function to it. 

In this case, 
it's very easy to find the actual value, 
because we're dealing with a pair where one component is the value and the other a log. 

So first we just take the value, which is x and we apply the function f to it. 
We get a pair of (y,newLog), where y is the new result and newLog the new log. 

But if we returned that as the result, 
the old log value wouldn't be included in the result, 
so we return a pair of (y,log ++ newLog). 
We use ++ to append the new log to the old one.

Here's applyLog in action:

  ghci> (3, "Smalish gang") `applyLog` isBigGang
    (False,"Smalish gang.Compared gang size to 9")
  ghci> (30, "A freaking platoon") `applyLog` isBigGang
    (True, "A freaking platoon.Compared gang size to 9")

The results are similar to before, 
only now the number of people in the gang had its accompanying log 
          and 
    it got included in the result log. 
  Here are a few more examples of using applyLog:

  ghci> ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
    (5, "Got outlaw name.Applied length.")

  ghci> ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))
    (7, "Got outlaw name.Applied length.")

See how inside the lambda, 
x is just a normal string and not a tuple and 
how applyLog takes care of appending the logs.

---
### Monoid to the rescue

Right now, 
applyLog takes values of type (a,String), 
but is there a reason that the log has to be a String? 

It uses ++ to append the logs, 
so wouldn't this work on any kind of list, not just a list of characters? 
Sure it would. We can go ahead and change its type to this:

    applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])

Now, the log is a list. 
The type of values contained in the list 
  has to be the same for the original list 
    as well as for the list that the function returns, 

  otherwise we wouldn't be able to use ++ to stick them together.

Would this work for bytestrings? 
There's no reason it shouldn't. 
However, the type we have now only works for lists. 
It seems like we'd have to make a separate applyLog for bytestrings. 

But wait! 
Both lists and bytestrings are monoids. 

As such, 
they are both instances of the Monoid type class, 
which means that they implement the mappend function. 

And for both lists and bytestrings, 
mappend is for appending. Watch:

    ghci> [1,2,3] `mappend` [4,5,6]
      [1,23,4,5,6]
    ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]
      Chunk "chi" (Chunk "huaua" Empty)

Cool! Now our applyLog can work for any monoid. 
We have to change the type to reflect this, 
as well as the implementation, 
because we have to change ++ to mappend:

    applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
    applyLog (x, log) f = let (y, newLog) = f x
                          in (y, log `mappend` newLog)


Because the accompanying value can now be any monoid value, 
we no longer have to think of the tuple as a value and a log, 
but now we can think of it as a value with an accompanying monoid value. 

For instance, 
we can have a tuple that has an item name and an item price as the monoid value. 
We just use the Sum newtype to make sure 
  that the prices get added as we operate with the items. 

Here's a function that adds drink to some cowboy food:

    import Data.Monoid  
      
    type Food = String  
    type Price = Sum Int  
      
    addDrink :: Food -> (Food,Price)  
    addDrink "beans" = ("milk", Sum 25)  
    addDrink "jerky" = ("whiskey", Sum 99)  
    addDrink _ = ("beer", Sum 30)

We use strings to represent foods and 
       an Int in a Sum newtype wrapper to keep track of how many cents something costs. 

Just a reminder, doing mappend with Sum results in the wrapped values getting added together:

    ghci> Sum 3 `mappend` Sum 9  
     Sum {getSum = 12} 

The addDrink function is pretty simple. 
If we're eating beans, it returns "milk" along with Sum 25, 
  so 25 cents wrapped in Sum. 
If we're eating jerky we drink whiskey and if we're eating anything else we drink beer. 

Just normally applying this function to a food 
  wouldn't be terribly interesting right now, 
but using applyLog to feed a food that comes with a price itself into this function 
  is interesting:

    ghci> ("beans", Sum 10) `applyLog` addDrink  
      ("milk",Sum {getSum = 35})  
    ghci> ("jerky", Sum 25) `applyLog` addDrink  
      ("whiskey",Sum {getSum = 124})  
    ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
      ("beer",Sum {getSum = 35})  

Milk costs 25 cents, 
but if we eat it with beans that cost 10 cents, 
we'll end up paying 35 cents. 
Now it's clear how the attached value doesn't always have to be a log, 
it can be any monoid value and how two such values are combined into one depends on the monoid. 
When we were doing logs, they got appended, but now, the numbers are being added up.

Because the value that addDrink returns is a tuple of type (Food,Price), 
we can feed that result to addDrink again, 
so that it tells us what we should drink along with our drink and 
how much that will cost us. 
Let's give it a shot:

    ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
      ("beer",Sum {getSum = 65})  

Adding a drink to some dog meat results in a beer and an additional 30 cents, so ("beer", Sum 35). 
And if we use applyLog to feed that to addDrink, 
we get another beer and the result is ("beer", Sum 65).

---
### The Writer type

Now that we've seen that a value with an attached monoid acts like a monadic value, 
let's examine the Monad instance for types of such values. 

The Control.Monad.Writer module 
    exports 
  the Writer w a type along with its Monad instance 
    and 
  some useful functions for dealing with values of this type.

First, 
let's examine the type itself. 

To attach a monoid to a value, 
we just need to put them together in a tuple. 

The Writer w a type is just a newtype wrapper for this. 
Its definition is very simple:

    newtype Writer w a = Writer { reunWriter :: (a, w) }


