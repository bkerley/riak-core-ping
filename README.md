pingapp: A Riak Core Learning Experiece
======================================

Caveat
------

This app isn't useful in its execution. I wrote this in
preparation for Casey Rosenthal's Riak Core workshop
at Erlang DC on Feb. 9 2013. The part that's worth using
is the commit log, seeing how I put the thing together and
added parts.

Have fun, and let me know if you find this useful or don't find
it useful.

[Bryce Kerley](mailto:bryce@basho.com)

Application Structure
---------------------

This is a blank riak core application. To get started, you'll want to edit the
following files:

* `src/riak_pingapp_vnode.erl`
  * Implementation of the riak_core_vnode behaviour
* `src/pingapp.erl`
  * Public API for interacting with your vnode
