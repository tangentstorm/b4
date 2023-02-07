Once Upon a Time, a Heap was a Tree
===================================

When I learned to program with Turbo Pascal back in the early 1990's, I learned there were three places to store data:

 - **the static data are**, where all the unit-global and program-global variables lived,
 - **the stack**, where the procedure-local and function-local variables lived, and
 - **the heap**, where dynamically allocated variables lived

The stack started at the top end of working memory. The stack grew downward a little bit each time you entered a routine, and would shrink upward by that same amount when you left the routine.

The heap started at the bottom end of working memory. It would grow each time you allocated some memory with `New`, and would shrink a little each time you called `Dispose`.

Of course, if you disposed an object that was in the middle of the heap, there would be a hole in it, and the system would try to fill that hole in the next time you called `New`. Aside from the fragmentation caused by those holes, the heap was something that was very linear.

In fact, you could treat the heap as a kind of stack by calling two special procedures: `Mark` and `Release`. When you called `Mark`, the program would store the current end of the heap. Then you could allocate some temporary data structures, work with them, and dispose of them all at once by calling `Release`. The data would still be there in RAM, of course, but it could be used again later.

Imagine my surprise many years later, then, when I stumbled on to [wikipedia's description of a heap](http://en.wikipedia.org/wiki/Heap_%28data_structure%29):

> In computer science, a **heap** is a specialized tree-based data structure that satisfies the *heap property*: If *A* is a parent node of *B* then *key(A)* is ordered with respect to *key(B)* with the same ordering applying across the heap.

From what I understand, a particular old LISP implementation used a heap structure to manage its dynamic memory, and the name just sort of stuck.

Well, no big deal, right? This happens in human language all the time. ( There's even a name for it in linguistics, though the term escapes me. ) So I just noted it as an interesting fact, and forgot about it.

Recently, though, I've been thinking quite a bit about the heap in [retroforth](http://retroforth.org/) (an very interesting open source language I discovered a few months ago), and happened to notice a discussion about "Cheney's algorithm" on the #forth IRC channel.

Retro (like almost all forth dialects) separates the stack into two pieces. There's a data stack, which holds arguments, and a return stack, which holds the return address. Since the two are separate, you can think of forth as a language where every function has the same data type: each word takes a stack, and returns a stack, and so there's no need to specify the argument by name at all.

However, the heap grows the same way it did back in turbo pascal... And while it's easy enough to implement `Mark` and `Release`, it's a little trickier to implement `New` and `Dispose`. This is because:

  - in forth, you can create new words (functions) on the fly. These words are created on the heap, and each word is compiled and written to ram as you type it (at least in retroforth).
  - since you don't know how much ram a new definition will need, you can't just assume it will fit in a hole in the heap (though you could certainly move it after the fact).
  - however, you don't really use variables all that much in forth
  - therefore, you don't create too many pointers (instead, you just put addresses on the data stack)
  - therefore, most of the things you'd want to dispose are anonymous
  - therefore, it would be nice to have garbage collection instead of trying to manage the ram or just letting the heap grow.

Anyway, I jumped into the discussion on #forth, because I'd recently been thinking that the way the [Cheney on the MTA](http://www.pipeline.com/~hbaker1/CheneyMTA.html) algorithm used the stack would be a pretty good way for retro to manage its heap.

This lead to a bit of Abbot-and-Costello-like confusion, because while this paper was what I thought of as the "Cheney Algorithm", it was actually an adaptation of Cheney's actual work. The guy I was talking to hadn't read the MTA paper, and I hadn't read the original.

*Cheney on the MTA* is a paper by Henery Baker that describes a garbage collection plan for a scheme implementation. Scheme is required to be tail-recursive, which means that the stack doesn't grow when the last thing a function does is call itself. C doesn't work this way, but the author of the MTA paper found a way to trick C into working this way, using a function called `alloca` that allocates dynamic memory on the stack rather than the heap, and uses [continuation-passing-style](http://en.wikipedia.org/wiki/Continuation-passing_style) to avoid ever returning from a function at all.

So: in Baker's algorithm, the C stack just grows and grows until it overflows:

> Since none of our C functions ever returns, the only live frame on this "stack" is the top one. However, within many of the dead frames will be found live closures and live user data objects. Eventually, the C "stack" will overflow the space assigned to it, and we must perform garbage collection. Garbage collection (GC) by copying is a relatively straight-forward process. There are a number of static roots, as well as the latest continuation closure, which is passed to the GC as an argument. (Forming an explicit continuation closure for the GC avoids the necessity of scanning C stack frames.) The live objects and live closures are all copied (and thereby condensed) into another area, so that execution can be restarted with a "stack" frame at the beginning of the C "stack" allocation area.

The name of the paper is a reference to a song, *Charlie on the M.T.A.* recorded by the Kingston Trio in 1959. As a footnote to the paper explains:

> Charlie couldn't get off the Metropolitan Transit Authority (now the Massachusetts Bay Transit Authority "MBTA") subway (tube) train, because he lacked the money to pay the fare. 

From the song:

> Oh, will he ever return?
> No, he'll never return,
> and his fate is still unlearned.
> He will ride forever,
> 'neath the streets of Boston,
> he's a man who'll never return. 

In Baker's scheme, it's the instruction pointer that never returns. But why, exactly, was it *Cheney* on the MTA? Who was this Cheney guy, and what did he have to do with anything?

I'd never really followed up on this question, until the Abbot-and-Costello routine in #forth.

It turns out [Cheney's Algorithms](http://en.wikipedia.org/wiki/Cheney%27s_algorithm) is a general garbage collection algorithm. Per wikipedia:

> Garbage collection is performed by copying live objects from one semispace (the from-space) to the other (the to-space), which then becomes the new heap. The entire old heap is then discarded in one piece.

All quite interesting, I thought, but something bugged me about it, and I wasn't exactly sure what.

Eventually, I realized the problem was this line:

> it's sometimes called a "two-finger" collector --- it only needs "two fingers" pointing into the to-space to keep track of its state. The data between the two fingers represents work remaining for it to do.

How could the system copy all those objects, when they could be all tangled up? After all, the heap could be a completely tangled up mess, with objects pointing to each other all over the place. 

Had I read the article more carefully, I'd have known the answer. (Wikipedia explains it very clearly.) Instead, I found myself wondering about it later and I made what turned out to be a completely wrong guess. I was wrong about how Cheney might have approached the problem, but my wrong guess helped me to understand something else that had been bugging me for a while.

My thinking went something like this:

Suppose Cheney had worked his magic by creating a tree structure?

- All the data would be arranged in a tree, sort of like an XML document.
- Each node in the tree would be something like a `cons` cell in lisp: 
  - each cell has two parts
  - one part holds data
  - the other cell holds the address of another cell
  
Anyway... Suppose you wanted to reference another part of the tree? Well instead of a pointer, you could use something like a hyperlink. That is: you could create a token that identified the node (like a primary key in a database, or the `id` attribute in XML). Then you could use some kind of lookup system to find the correct node in the tree.

Come to think of it... Isn't that pretty much how lisp actually works? You can play tricks with the layout of a `cons` cell if you really want to, but at least 90% of the time, they work like trees, and if you want to keep a reference to another tree, you give it a name and use the name instead.

If everything was accessed by reference this way, then you could move the actual data around in memory however you liked. The tree could act as an index, containing pointers to a much bigger block of ram that stored variable length items like strings and whatnot.

So for example, you could create a number of strings: "one", "two", "three", "four", and they might be stored in ram like so:

   **3** 'o' 'n' 'e' **3** 't' 'w' 'o' **5** 't' 'h' 'r' 'e' 'e' **4** 'f' 'o' 'u' 'r'
   
Where the numbers indicate the length of the string. Then you could number these as 1, 2, 3, 4, and store pointers (or relative pointers) to them in a sorted index.

Now suppose later it turns out that you're not actually using the string "three" anymore, and you want to reclaim that memory. You could keep a slot for reference counting in each record of the index, set all the counts to zero, and then walk the stack to figure out what values were actually referenced.

Then you could move the actual data around, updating the index to point to the new location.

Anyway, now I'm picturing this data structure, and I remembered that some time ago, I had encountered a certain kind of tree-data structure that was stored in an array of fixed-sized records like this, and was good at keeping items sorted.

What was that tree structure called? Oh yeah. A *heap*. I could use a heap to model the heap! What a concept!


----

Notes
=====

- There's an online book about text editors -- [The Craft of Text Editing, or: Emacs for the Modern World](http://www.finseth.com/craft/) -- that talks about having a floating gap of space inside the text buffer, and moving the gap around as the cursor moves. The image of this gap image kept occurring to me as I thought about defragmenting the memory using the index. I think it may make sense to use something like that to minimize the amount of data that you have to move when defragmenting, compared to packing it all to one side. This would of course break `Mark` and `Release` but they are obsolete anyway.


- In the original draft, I wrote:

> It has always baffled me in lisp why CAR ("contents of the address register") holds data whereas the CDR ("contents of the data register") holds the link to the rest of the list. But this is a mystery for another day.

[ttmrichter](http://www.txt.io/ttmrichter) pointed out that LISP was originally implemented on an [IBM 704](http://en.wikipedia.org/wiki/IBM_704), which used a 36-bit machine word, broken into two 15-bit parts and two 3 bit parts. The 15-bit parts stored the *address* and *decrement* that the instruction applied to (the decrement being a negative offset). These two words are the actual origin of `CAR` and `CDR` in LISP, and the reason I was baffled was that I was just plain wrong. :)
