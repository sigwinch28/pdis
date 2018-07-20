pdis
=====

The Erlang'18 artifact of a lightweight communication discrepancy tool.

Note: due to difficulties in implementation, this version of the tool
does not use the `message_info` map described in the manuscript. Instead,
the tool naively checks each send against _all_ receives present in the
module.

This doesn't affect the implementation of the analysis, but does
make the tool impractical for use.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/pdis
