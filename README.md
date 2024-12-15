# Advent of Code 2024 in Ada

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 6 years of pain and suffering aren't enough. :grin:
An elf historian has gone missing,
and instead of letting him learn his lesson,
other historians impress you into saving him.


* [Ranking of problems by difficulty](#ranking-of-problems-by-difficulty)
* Problems in order of appearance
  * 📋 [Day 1](#-day-1-historian-hysteria): Historian Hysteria
  * ☢️ [Day 2](#️-day-2-red-nosed-reports): Red-Nosed Reports
  * ✖️ [Day 3](#️-day-3-mull-it-over): Mull It Over
  * 🎄 [Day 4](#-day-4-ceres-search): Ceres Search
  * 🖨️ [Day 5](#️-day-5-print-queue): Print Queue
  * 👮 [Day 6](#-day-6-guard-gallivant): Guard Gallivant
  * 🌁 [Day 7](#-day-7-bridge-repair): Bridge Repair
  * 📡 [Day 8](#-day-8-resonant-collinearity): Resonant Collinearity
  * 💾 [Day 9](#-day-9-disk-fragmenter): Disk Fragmenter
  * 🥾 [Day 10](#-day-10-hoof-it): Hoof It
  * 🎱 [Day 11](#-day-11-plutonian-pebbles): Plutonian Pebbles
  * 🌽 [Day 12](#-day-12-garden-groups): Garden Groups

## Ranking of problems by difficulty
This is inherently subjective, and I may even misremember how difficult I found a problem, so if you disagree, at least check out the justification I give in the relevant day's Experience section.

### 🤩 Quick 'n easy puzzles

C'mon, you can do these. (Ironically, I got some of them wrong on the first try, while getting some harder ones right on the first try...)

Note that "easy" is a relative term here:
I might not have found these easy when I first started doing Advent of Code puzzles back in 2019.
In particular, I'm much more comfortable with data structures and Ada's standard library,
which is sometimes the main dividing line between "quick 'n easy" and "needing a little more thought".

* 📋 [Day 1](#-day-1-historian-hysteria): Historian Hysteria
* ☢️ [Day 2](#-day-2-red-nosed-reports): Red-Nosed Reports
* ✖️ [Day 3](#-day-3-mull-it-over): Mull It Over
* 🎄 [Day 4](#-day-4-ceres-search): Ceres Search
* 🖨️ [Day 5](#-day-5-print-queue): Print Queue
* 🌁 [Day 7](#-day-7-bridge-repair): Bridge Repair
* 📡 [Day 8](#-day-8-resonant-collinearity): Resonant Collinearity
* 💾 [Day 9](#-day-9-disk-fragmenter): Disk Fragmenter


### 🤔 Puzzles needing a little more thought

An example of "a little more thought" is when
a brute force approach leads to a combinatorial explosion
that exhausts memory, patience, or the heat death of the universe.
Each puzzle listed here also gives a reason for its being so listed.

* 👮 [Day 6](#-day-6-guard-gallivant): Guard Gallivant

  Part 2: How do you detect an infinite loop?
* 🥾 [Day 10](#-day-10-hoof-it): Hoof It
  - Part 1: Some sort of search algorithm: BFS, DFS, ...
  - Part 2: How do you count the distinct paths?

    (From what I read, a brute force approach to Part 2
    is not the combinatorial explosion I expected.
    I'm keeping it here because I put more thought into it,
    the solution is mildly non-trivial, and either way
    BFS or DFS really ought to land you here.)
* 🎱 [Day 11](#-day-11-plutonian-pebbles): Plutonian Pebbles

  Part 2: A brute force approach **will not work.**

### 😨 Problems requiring a lot of thought, or trickier ideas

* 🌽 [Day 12](#-day-12-garden-groups): Garden Groups

  Part 1 already requires a bit of care, since there is not just 1
  but multiple BFS (at least, that's how I did it).
  Without the wrinkle in Part 2, this would be in the lower category,
  but that wrinkle was non-trivial.

### 😱 Great puzzles that are jes' plain ornery

### 😭 What was the puzzle master thinking?!?

## Problems in order of appearance

### 📋 Day 1: Historian Hysteria

Other historians have drawn up lists of where the missing one may be.
Unsurprisingly, Santa's elves can't get their lists straight.

In part 1, you have to compute the distance between two lists,
after sorting them.

In part 2, you compute a similarity score.

#### Unusual tools

None in particular.

#### Experience

Aside from getting it wrong on the first try through some carelessness I've since forgotten,
it's the usual, soft start.

### ☢️ Day 2: Red-Nosed Reports

You walk on over to the Red Nosed Reindeer's nuclear fusion/fission plant, which appeared in the 2015 Advent of Code.
The engineers need help interpreting some unusual data.

In part 1, you have to determine which reports indicate safe behavior.

In part 2, they realize that some bad behavior is tolerable,
so you re-figure it with that new information.

#### Unusual tools

None in particular.

#### Experience

Aside from getting it wrong on the first try, it's another relatively easy problem.
If memory serves, this is one of the problems where I forgot that
Ada's sensible indexing of arrays means you need this:

    while Position <= Line'Last loop

instead of this:

    while Position < Line'Length loop

### ✖️ Day 3: Mull It Over

Now you're at the North Pole Toboggan Repair Shop,
which appeared in the 2020 Advent of Code.
The historians go to look in the warehouse
(what did they drag you along for anyway?)
so you're left to help the shopkeeper with his computer issues.
The computer's memory is corrupted, but you can still make out
what it wanted to do.

In part 1, you add up the results of all the multiplications.

In part 2, you re-figure it after realizing that
the program sometimes tells `don't()` and `do()` multiply.

While I appreciate the pun, I was hoping for mulled wine to appear.

#### Unusual tools

The same procedure solves both parts.
An optional boolean parameter determines which part it solves.

#### Experience

Aside from running into `Integer_IO.Get`'s issue with colons, it was pretty straightforward.

### 🎄 Day 4: Ceres Search

The historians teleport themselves (and you) to the Ceres monitoring station,
which appeared in the 2019 Advent of Code.
Once again, the historians abandon you while they go looking,
so you end up helping yet another elf with technical issues: a word search.

In part 1, you help her find all locations of `XMAS`.

In part 2, after she ~~glares at you~~ "looks quizzically at you"
because you misunderstood the assignment,
you help her find all locations of two `MAS`'s arranged in cross.
For example:

    M S
     A
    M S

#### Unusual tools

None in particular.

#### Experience

I have this vague memory that this is the first one I got right on the first try.
I also have a vague memory that I had some trouble with it, and
the use of `Doing_Example` suggests I was sufficiently insecure about my solution
that I wanted to check it.

### 🖨️ Day 5: Print Queue

Now you teleport (? the puzzle doesn't say) back to the North Pole
to search sub-basement 17,
which contains a printer that appeared in the 2017 Advent of Code.
Abandoned yet again by the historians,
you are once again taken aside by an elf to fix the aforementioned printer,
which is having trouble printing the updated sleigh launch safety manuals.
He has a list of updates with pages to print,
but the pages are out of order.

In part 1, you determine which updates are in the correct order.

In part 2, you correctly order the other manuals.

#### Unusual tools

Nothing in particular,
though I can remember back when `Ada.Containers.Vectors.Generic_Sorting`
seemed devilishly hard to implement.

#### Experience

As I recall, I was ecstatic at finally getting both puzzles right on the first submission.

### 👮 Day 6: Guard Gallivant

The historians now time travel to 1518, dragging you along with them.
You remember that you visited 1518 in the 2018 Advent of Code.
Just as you did then (albeit on a differently-linked day),
you must avoid a time travel paradox which would occur
if a patrolling guard discovers you.

In part 1, you count how many distinct positions the guard visits
before leaving the room.

In part 2, the historians decide that's too many, and decide
to place an obstacle which turns his route into an infinite loop.
You determine the number of locations where they could place said obstacle.

#### Unusual tools

Whipped out the `Common` crate which I create for the 2023 Advent of Code,
and even modified it slightly, so that
`Two_Dimensional_Map_IO` remembers the start position when reading the input!

#### Experience

I enjoyed this puzzle very much, especially since I remembered pretty quickly
how to detect an infinite loop.
That said, my implementation of the approach is not the best:
I add "annotation locations" (`Location_And_Direction_Record`) to a set,
instead of using an array of booleans, which would have been much more efficient.

### 🌁 Day 7: Bridge Repair

The historians teleport (and perhaps time travel) with you
to a rope bridge that appeared in the 2022 Advent of Code, but
it's in disrepair.
Some engineers have the calibration equations needed to fix it,
but young elephants that live nearby stole the equations' operators.

In part 1, you figure out which equations are satisfied by placing
addition and multiplication operations between the numbers.

In part 2, you figure out which additional equations are satisfied
by placing addition, multiplication, **and concatenation** operations
between the numbers.

#### Unusual tools

This is the first time I have whipped out

    type Values is range 0 .. 2 ** 64 - 1;

...instead of using something like `Ada.Numerics.Big_Integers`,
and the result was well worth it.

#### Experience

I am rather pleased that my mildly optimized solution worked
(pruning equations where the product of all the operands is too small
or the sum is too large).
I am not so pleased that I mis-reasoned about the concatenation operator,
which frustrated me mightily until I worked through the example,
in which case it was quickly obvious what the problem was.
Thus, the problem stays in the [Quick 'n easy](#-quick-n-easy-puzzles),
even though it took me much more time than those puzzles usually take.

### 📡 Day 8: Resonant Collinearity

You're now on the roof of "a top-secret Easter Bunny installation",
which is apparently related to the 2016 Advent of Code,
and not in a good way, since it seems unsurprising that
the Easter Bunny has some scheme to make people a fraction more likely
to buy "Easter Bunny brand Imitation Mediocre Chocolate as a Christmas gift"
by broadcasting to antennas' antinodes.

In part 1, you count the unique locations with antinodes.

In part 2, you update the model considering "resonant harmonics"
(thankfully he explains the consequence) and do the same thing again.

#### Unusual tools

* I used a range of characters,
  though I didn't narrow it down as much as I could have via predicates.
* I also used the `Common` crate's `Mathematics` package, to compute gcd's,
  but that turned out to be unnecessary.

#### Experience

Fun and easy: once the code ran without crashing,
I was getting the right answer on the first try.
However, the code crashed more than it should have
because I didn't adequately think through
the number of antinodes necessary for part 2.
What was my degree in, again?

### 💾 Day 9: Disk Fragmenter

Now you're under the sea again,
with some amphipods you first met in the 2021 Advent of Code.
The historians abandon you to search, but start off crashing into walls.
While they figure out how to make it work,
you see a young amphipod trying to fix his computer.
It's a problem with fragmented disk space.

In part 1, you compute the filesystem checksum resulting from
defragmenting the drive by fragmenting the files
(no, I am not making this up).

In part 2, you compute the filesystem checksum resulting from
defragmenting the drive without fragmenting the files.

#### Unusual tools

* Used this construct again:

      type Checksum_Value is range 0 .. 2 ** 64 - 1;

* I used cursors a lot, not that I wanted to.

#### Experience

I struggled getting this one to compile and run.
Issues included:
* cursors and their usage;
* formulating the right data structures;
* implementing the algorithm carefully;
* converting integers to the `Checksum_Value` type at the right moment;

...and probably some others. The main one was implementing the algorithm carefully.

On the bright side, _once I had it compiled and running_,
the correct answer just popped out!

### 🥾 Day 10: Hoof it

You're back at the Lava Production Facility from Advent of Code 2023!
Once again the Historians abandon you to search for their colleague,
while a reindeer asks you to help make sense of a topographic map.
In part 1, you determine the number of summits reachable from a trailhead.
In part 2, you determine the number of unique trails from the trailheads
to the summits.

#### Unusual tools

For Part 2 I pulled out a Manhattan distance and
performed a _reverse_ breadth-first search from the summits to the trailheads.
That makes it relatively easy to trace the unique trails.

At this point I can't claim that using my `Common` crate is unusual.

#### Experience

Part 1 was fun and easy.
For Part 2 I spent far too long chasing false trails, trying to find
a relatively easy combinatorial formula I could use from the information I had saved.
Eventually I concluded that I wasn't saving enough information to use those formulas,
and while ruminating over how to work around it, I realized that
a reverse search from the summits to the trailheads could work.

For each trail, I perform a breadth-first search for the corresponding trailhead:
* start from each summit;
* move in each legal direction (N, S, E, W **plus** decreasing distance);
* prune positions only when their distance to the trailhead exceeds their current level,
  as any gradual descent would at best take you to a different trailhead.

Worked great! 😁

### 🎱 Day 11: Plutonian Pebbles

The Historians teleport you to Pluto, so that they can explore
the infinite corridors carved out by its ancient civilization.
"You notice a strange set of physics-defying stones."
Every time you blink, the numbers on the stones change,
and sometimes the **number** of stones changes:
* a stone numbered 0 changes to a stone numbered 1;
* a stone with an even number of digits splits into two stones,
  where the first stone has the left digits
  and the second stone has the right digits;
* any other stone, whose number is _x_,
 changes to a stone numbered with the value 2024 * _x_.

In part 1, you figure how many stones there are after you blink 25 times.
In part 2, you figure how many stones there are after you blink 75 times.

#### Unusual tools

Nothing in particular.

#### Experience

I enjoyed this problem!
It took me a bit to figure part 2; at first I thought
it should be a memoization problem, where you tracked when each ball appeared,
then worked out the number of stones in some recursive fashion.
It soon became clear that this was a fruitless rabbit hole.

A quick test verified my suspicion that brute force would quickly exhaust memory.
(In the worst case the list doubles in size on a blink.
While the worst case never happens, its growth is still near-exponential.)

I then realized that one merely needs to track the distinct numbers
and of how many there are of each.
That is, instead of tracking a list like

    4, 0, 4, 8, 20, 24, 4, 0, 4, 8, 8, 0, 9, 6

track a list like

    0 (3), 4 (4), 6 (1), 8 (3), 9 (1), 20 (1), 24 (1)

Makes things short 'n quick.

### 🌽 Day 12: Garden Groups

You're back at a garden you first visited in the 2023 Advent of Code.
While the historians go searching, a local elf asks you to help them figure
the cost of fencing their gardens.
In part 1, the cost is the sum of the products
of the area and perimeter of each garden region.
In part 2, the elves qualify for a discount, so the cost is the sum of the products
of the area and number of sides of each region.

#### Unusual tools

* I had to look at multiple examples.
  **Thank you thank you** Mr. Puzzle Master
  for providing that last example!
* Instead of one BFS to solve a part of a puzzle,
  I used multiple BFS.

#### Experience

Fun, but not easy, especially when I got the wrong answer on part 2,
because I hadn't read that last example.
Once again I write, **thank you thank you** Mr. Puzzle Master!
Earning that gold star felt _extremely_ rewarding this time.

The approach I used was to scan by row and column,
BFS at each unconsidered point to construct the region,
and identify fencing by comparing neighboring plots.
That's enough to finish Part 1
(and already enough to bump it up to the next highest list).
For Part 2, I identified sides for each region
by picking one of its fence pieces at random,
then following in the fences direction
to remove other pieces on that side.
That successfully identifies one unique piece of fence per side.

The additional wrinkle in part 2,
illustrated by the additional example,
is resolved by identifying which side of the fence
corresponds to the plot we're studying.
It then suffices to check
that the corresponding plot as you walk along the fence
contains the same plant.