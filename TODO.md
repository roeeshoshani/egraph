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
