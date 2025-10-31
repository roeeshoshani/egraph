- add an "internal var allocator" type, since this is a repeating pattern, and we can re-use the code.

- consider making the egraph take the rule set in its constructor, and then apply rules to nodes directly when the nodes are added, instead of in retrospect. this may provide a huge performance improvement. need to think about this.
