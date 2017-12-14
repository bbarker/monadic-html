# Optimizing Interpreter
The goal is to implement an optimizing interpreter that automatically inserts `.shared` 
when it doesn't break semantics (without changing anything to the public 
API). 

## Overview and Examples

The idea starts from the observation that certain streams (or chunks 
of streams), can be systematically cached and shared under certain purity
assumptions. For instance, given `val out = source.map(f1).map(f2)`, 
and assuming purity of f1 and f2, we know that the following two programs 
are equivalent (and it should be possible to prove this equivalence):

```scala
val out = source.map(f1).map(f2)
out.impure.foreach(println)
out.impure.foreach(println)
```


```scala
val out = source.map(f1).map(f2)
val tmp = Var(null)
out.impure.foreach(tmp.:=)
tmp.impure.foreach(println)
tmp.impure.foreach(println)
// Additional care must be taken to generate proper Cancelables...
```


But this is obviously not true in the general case, for instance in the presence of foldp. 

Developing a bit on this idea, it's possible to categorize the current API in three categories
— *always shareable*, *tail shareable* and *never shareable*:

* always shareable = map, zip, sampleOn, imitate, dropRepeats
* tail shareable = keepIf, merge
* never shareable = foldp, flatMap    

We will run through some examples, taking the simplest case of always shareable first. 
In all of the following marble diagrams, "•" represents the registration point.

* `map`

    ```scala
    val numbers: Rx[Int]
    val even: Rx[Int] = numbers.map(_ * 2)
    // numbers =>    0       0   3   4        5   6  ...
    // even1   =>        •   0   6   8        10  12 ...  
    // even2   =>                         •   10  12 ...      
    ```
    
In this case we can see that `even2` can directly be implemented in terms of `even1`,
so having `even2 = even1.sharing` under the hood is fine.


Similarly for the remaining *always shareable* operations:

* `zip`

```scala
    val r1: Rx[Int]
    val r2: Rx[Int]
    val zipped: Rx[Int] = r1.zip(r2)
    // r1      =>  0      8                        9     ...
    // r2      =>  1            4      5     6           ...
    // zipped1 =>  •(0,1) (8,1) (8,4)  (8,5) (8,6) (9,6) ...
    // zipped2 =>               •(8,4) (8,5) (8,6) (9,6) ...
```

* `sampleOn`

     ```scala
     val r1: Rx[Char]
     val r2: Rx[Int]
     val sp: Rx[Int] = r2.sampleOn(r1)
     // r1  =>   u    u   u   u ...
     // r2  => 1    2 3     4   ...
     // sp1 =>   •1   3   3   4 ...
     // sp2 =>            •3  4 ...   
     ```

* `imitate`
    
    ```scala
    val sndProxy = Var(1)
    val fst = source.merge(sndProxy.map(1.+))
    val snd = fst.keepIf(isOdd)(-1)
    val imitating = sndProxy.imitate(snd)
    // source     => 1       6     7
    // fst        =>  1 2     6     7   8
    // snd        =>   1             7
    // imitating1 =>   •1            7
    // imitating2 =>         •       7  
    ```

* `dropRepeats`

    ```scala
    val numbers: Rx[Int]
    val noDups: Rx[Int] = numbers.dropRepeats
    // numbers  => 0  0 3 3 5 5 5 4 ...
    // noDups1  => •0   3   5     4 ...
    // noDups2  =>            •5  4 ...
    ```


Tail shareable means that a stream can be shared after it emitted its second element. At first glance it might not be obvious why `dropIf` is only tail shareable, here is a small diagram to illustrate:

* `dropIf`/`keepIf`

    ```scala
    val numbers: Rx[Int]
    val odd: Rx[Int] = numbers.dropIf(_ % 2 == 0)(-1)
    // numbers =>    0       0   3   4        5   6 ...
    // odd1    =>       •-1      3            5     ...
    // odd2    =>                       •-1   5     ...    
    ```
    
We see that `odd1` emits -1, 3, 5 while `even2` emits -1, 5, so not everything can be 
shared between the two streams. In other words, we cannot simply implement `odd2` by 
"hooking" into `odd1` upon registration, since this would instead give the emission 
sequence 3, 5. But we could do that after the 5 is emitted! 

A similar argument can be used to show that merge is not always shareable but only 
tail-shareable.
    
//TODO:    
* `merge`
    
    ```scala
    val r1: Rx[Int]
    val r2: Rx[Int]
    val merged: Rx[Int] = r1.merge(r2)
    // r1      => 0    8     3 ...
    // r2      => 1      4 3   ...
    // merged1 => •0 1 8 4 3 3 ...
    // merged2 =>   •1 8 4 3 3 ...
    ```


* `foldp`

    ```scala
    val numbers: Rx[Int]
    val folded: Rx[Int] = numbers.foldp(0)(_ + _)
    // numbers  => 1  2 1  1 3 ...
    // folded1  => •1 3 4  5 8 ...
    // folded2  =>      •1 2 5 ...
    ```


(TODO: I think flatMap can be made more precise by saying that it "inherits" the 
shareability of whatever is returned by its function. 
For instance, flatMapping on something that always returns a Rx made of map and zip 
results in something always shareable.)

* `flatMap` basic case (always shareable)

    ```scala
    val numbers: Rx[Int]
    val evens: Rx[Int]
    val odds: Rx[Int]
    val flip: Rx[Int] = numbers.flatMap(x => if x % 2 == 0 evens else odds)
    // numbers   => 1  2 1  4 3 ...
    // evens     => 0  2 4  6 8 ...
    // odds      => 1  3 5  7 9 ...
    // flip1     => •1 2 5  6 9 ...
    // flip2     =>      •5 6 9 ...
    ```

//TODO: other `flatMap` cases

It's then interesting to think how this shareability properties compose. 
I have the intuition than if a Rx is only composed of always shareable parts it 
is itself always shareable, while containing any never shareable component makes 
the entire Rx non shareable. It gets funny when mixing always shareable and 
tail shareable components, it seams that the result is not just tail shareable, 
but something stronger where each of the intermediate streams have emitted at
least one element. As an example, consider 
`val out = rx1.dropIf(_ % 2 == 0)(-1).merge(rx2.dropIf(_ % 2 == 0)(-1))`. 
It is possible to have out emit several values without reaching shareability, 
or to have each "leaf" Var to emit several values without reaching shareability 
(the diagrams get a bit messy to draw, but I have have them on a white board .


## Algorithmic Considerations

**Question**: since we already have asserted that:
> When combined with a `fold` operator, `flatMap` is problematic: 
it either leaks memory or breaks referential transparency.

should we detect this and either warn or throw an error?