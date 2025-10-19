- when performing a `union` in the egraph, we can perform a very nice optimization, which is closely related to loops that originate from unions (e.g the loop created when optimizing `x * 0 == 0`, which generates the infinite expression of `x * (x * (x * (... * 0)))`).

  such loops can only be created when unioning one eclass, let's call it eclass A, with another eclass, let's call it eclass B, if A is a descendent of B, and B is not a descendent of A (if both of them are descendents of one another, then the loop already existed in the original expression, so there's nothing we can do about it).

  in that case, where originally there was no loop, but after unioning, we will get a loop, we can do something smart.
  once we union them both, we can say that we are left with some redundant information, which we can get rid of and not worry about losing any possible optimizations.

  originally, before the union, eclass B contained some path (choosing the right enodes down the tree) that leads to it using eclass A.
  after the union, this path is completely redundant, since it introduces an unnecessary loop, and is guaranteed to be more complex than just plainly using eclass A.

  so, we want to get rid of this path. it makes our lives harder for no reason.

  how do we get rid of it? well, we traverse the descendents of eclass B, and find any enode which directly uses eclass A as a child.
  for each of those enodes, we remove them from the egraph. this will also require us to remove every enode which uses the removed enodes, recursively, which is nice since it may kill a bunch of enodes in our graph and make it smaller.

  this does require figuring out how removing nodes should be properly done in the egraph, which traditionally is append only.

- when implementing rewrites, avoid forcing the implementation to clone the context every single time. this is currently necessary since the trait functions receive a &Ctx as argumnet, but should return an owned Ctx, which requires a lot of cloning.

  maybe we can somehow make it so that they can avoid the cloning, for example by returning `None` indicating that the original ctx should be used or something like that. need to see if it works with rust's annoying borrow checker.

- implement a constant-folding re-write. this will help us see if the rewrite traits that i implemented are actually generic enough.

- modify the rewrite trait such that the context is always `Box`ed or `Rc`ed so that we can convert it to a trait object, which will allow us to get rid of the annoying `Rewrites` trait which limits flexibility.

  this will then allow us to add a `bi_directional` field to the template re-write, and have the `build` function return either one or two rules without losing my shit.

  NOTE: maybe we can instead have a `build_bi_directional` function?

  how expensive is the `Box`ing and `Rc`ing? it will certainly make this trait much easier to work with, but is it worth it?

- add an "internal var allocator" type, since this is a repeating pattern, and we can re-use the code.
