# source("aperm.R") #* Seems to be some array stuff we want to support?
source("append.R") #* Appending lists? I think we should
source("apply.R") #* Appears to use some annoying reflection properties that are sketch
source("array.R") #* Definitely want to support such an important function 
# source("assign.R") #* Has an assign() function, and some list2env stuff -- guess needed somehow
# source("backsolve.R") #* These are internal calls to linear equality solvers
# source("Bessel.R") #* I guess it doesn' thurt to have custom bessel functions
# source("bitwise.R") #* Internal calls to bitwise operators
# source("builtins.R") #* Just a delegation call to builtin functions
# source("cat.R") #* We probably want to rewrite cat shit for strings
# source("character.R") #* Some of the stuff are nice, but others not so much.
# source("chol.R") #* Choleski decomposition
# source("colSums.R") #* Matrix column information?
# source("conditions.R") #* Try catch / error blocks?
# source("connections.R") #* Jesus fuck. System call time?
# source("constants.R") #* Finally something simple. literally just names for constants
# source("delay.R") #* I guess implementing delayedAssign() through native calls isn't too bad
# source("det.R") #* Determinant calculations
# source("diag.R") #* Diagonal shit
# source("diff.R") #* Appears to be iterated (consecutive) differences in a vector. Could be handy
# source("duplicated.R") #* Appears to be some duplication stuff that we can maybe handle.
# source("eapply.R") #* Applies with respect to some environment. Can totally handle this!
# source("eigen.R") #* Looks like some eigen value stuff
# source("environment.R") #* I guess, what's the harm in providing these?
# source("findInt.R") #* Interval finding mechanism, can probably be done natively.
# source("funprog.R") #* Functional programming stuff. I love it :)
# source("get.R") #* Apparently tries to find objects in the environment?
# source("identical.R") #* Object identicality. Implementation goes to native code.
# source("ifelse.R") #* Tutorials actually use this shit and so should we.
# source("is.R") #* Functions like is.na and is.vector
# source("jitter.R") #* Adds random noise and shit it seems
# source("kappa.R") #* Looks important and also has norms
# source("kronecker.R") #* Kronecker delta matrix multiplication
# source("LAPACK.R") #* This does SVD it seems.
# source("lapply.R") #* I guess if we have apply, we may as well have these guys as well.
# source("mapply.R") #* Sure
# source("match.fun.R") #* Okay
# source("match.R") #* Fine, fuck, okay >:(
# source("matrix.R") #* A rather important file
# source("max.col.R") #* Maximum of columns
source("mean.R") #* Find the mean of things
# source("New-Internal.R") #* Appears to have some things like trig functions and shit
# source("outer.R") #* cross product
# source("pairlist.R") #* zippidy pairlist shit
# source("pmax.R") #* Doesn't seem to hut to include?
# source("pretty.R") #* Does'nt seem too bad
# source("print.R") #* Could just null-out these print functions to shut them up
# source("qr.R") #* Matrix stuff
# source("quit.R") #* I would also like to quit R
# source("range.R") #* Matrix range it seems like
# source("rank.R") #* Matrix stuff
# source("raw.R") #* Can totes do this, bro
# source("replace.R") #* I guess?
# source("replicate.R") #* Sure
# source("rep.R") #* Replication function
# source("rev.R") #* Having a reverse function definitely helps
# source("rm.R") #* I guess this is doable
# source("RNG.R") #* RNG functions
# source("rowsum.R") #* Sure
# source("sample.R") #* Sampling stuff
# source("sapply.R") #* More apply function shit
# source("scale.R") #* Scaling shit, I guess
# source("seq.R") #* Sequence generation
# source("sets.R") #* Set stuff would be helpful
# source("solve.R") #* Some matrix solving code
# source("sort.R") #* Definitely need sorting stuff
# source("summary.R") #* Summarizes stuff, but we want to black out things.
# source("svd.R") #* Of course
# source("sweep.R") #* I guess people may use htis
# source("TAOCP.R") #* Knuth RNG
# source("tapply.R") #* Might as well 
# source("toString.R") #* I guess some parts of this are actually important
# source("unlist.R") #* List flattening
# source("unname.R") #* Remove name attributes
# source("utilities.R") #* So simple????
# source("vector.R") #* Why, of course
# source("xor.R") #* Just an XOR function
# source("zapsmall.R") #* Rounding stuff?


#####
# Definition of custom functions (usually stubs)

library <- function (...) {}

warning <- function (...) {}


