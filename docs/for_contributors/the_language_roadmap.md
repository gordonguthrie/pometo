# The Language Roadmap

`Pometo` is a reimplementation of a venerable programming language paradigm `apl` (which stands for ***Another Programming Language*** imagine the brass neck of that for a name?).

The aim is to make a janus-faced, pure-functional, auxiliary (or lambda) language:

* looking towards `Erlang`, `Elixir` and `LFE` it is a native `BEAM` language that can handle native data types
* looking towards `apl` it is a ***normal*** `apl` implementation

We therefore have some ***prejudices***:

* conforming to the current version of Dyalog APL (18.0) requires no special decision making
* deviating from or extending the language from that base requires an explicit decision to do so
* `Pometo` is not a concurrent language - it runs inside a single `BEAM` process - concurrency comes from being embedded inside an application writen in `Erlang`, `Elixir` or `LFE`. This means that `Pometo` will ***never*** have what are traditionally thought of as core `BEAM` features:
   * no OTP/supervision trees
   * no message passing
   * no side effects - it is a pure functional language

There are some great Dyalog resources:

* [a manual](https://www.dyalog.com/uploads/documents/MasteringDyalogAPL.pdf)
* [a live `REPL`](https://tryapl.org/)
* [a live help site](http://help.dyalog.com/18.0/index.htm)