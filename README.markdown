# KITTeL/KoAT

KITTeL is an automatic termination prover for integer transition
systems and imperative programs written in a fragment of
[Simple](http://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/interproc/manual_syntax.html).
For programs written in other languages (e.g., C), consider using the
frontend [llvm2KITTeL](https://github.com/s-falke/llvm2kittel).

KoAT is an automatic complexity analyzer taking the same kinds of inputs.

## Authors

Stephan Falke, Marc Brockschmidt

## Papers

Stephan Falke, Deepak Kapur, Carsten Sinz:
[Termination Analysis of C Programs Using Compiler Intermediate Languages](http://dx.doi.org/10.4230/LIPIcs.RTA.2011.41).
RTA 2011: 41-50

Stephan Falke, Deepak Kapur, Carsten Sinz:
[Termination Analysis of Imperative Programs Using Bitvector Arithmetic](http://dx.doi.org/10.1007/978-3-642-27705-4_21).
VSTTE 2012: 261-277

Marc Brockschmidt, Fabian Emmes, Stephan Falke, Carsten Fuhs, J&uuml;rgen Giesl:
[Alternating Runtime and Size Complexity Analysis of Integer Programs](http://dx.doi.org/10.1007/978-3-642-54862-8_10).
TACAS 2014: 140-155

# dep
dep is a tool for visualizing dependencies between rules and arguments of an integer transition system.
dep shows how arguments influence eachother in an ITS by drawing an edge between two function arguments
whenever:
        * An argument is passed directly from one function to another
        * A variable is constrained in some way by another variable by the guard
        * The argument to a function is an equation involving other arguments

Additionally, arguments which are passed without change have a blue edge between
them, arguments which may change during the rewrite are drawn in green. This hopefully allows
the user to see which function positions are involved in the termination / complexity results
returned by KoAT.

## Building
   make dep

## Running
   ```
   ./dep.native <filename>.koat
   ```

   produces
   ```
    <filename>.dot
    <filename>-sliced.dot
    ```

where the "-sliced" modifier represents a version of the program that has had
argument slicing applied before the visualization process.  Many of the ITSs that
KoAT is capable of solving are too large to easily visualize and understand.  Argument slicing
reduces the size of the resulting graph in many cases, and provides an easier to understand
graph.

# drawRules
drawRules simply draws the connectivity of rewrite rules provided in an input
file in the format which KoAT accepts.  If there is a rule of the form f -> g,
then there is an edge between f and g in the graph.  No special attention is
paid to whether or not that particular transition is feasible.

## Building
   make drawRules

## Running
   ```./drawRules.native <filename>.koat``` produces ```<filename>.dot```.
   From here, apply dot on the output file to produce a renderable version of the graph
   in the format of your choice.

# compare

compare is part of the testing suite for KoAT.  Given two files that are the
result of running KoAT on an ITS, we can determine what the relative
complexities of the two systems are.  The primary purpose of this is in regression testing;
between two versions of KoAT, we should expect results to improve across all instances.

Complexity result A is considered to be better than B if:

* A is concrete, and B is Maybe
* A represents a polynomial with lower degree than B
* A and B represent the same order of polynomial, but A is smaller than B according to Poly.compare

compare exits with 0 if A is at least as good as B, and 1 otherwise.

## Building
   make compare

## Running
   ```./compare.native <a> <b>```
