![Pometo Logo](images/pometo_logo.png)

# What is Pometo?

`Pometo` is the the little `APL` for the `BEAM`, an auxiliary language to complement and use in your `Erlang`, `Elixir` and `LFE` applications.

THIS PROJECT IS SUPER-EARLY, NOT SO MUCH ALPHA AS BEFORE THE DAWN OF WRITING, SOME INCHOATE SYMBOLS SCRAWLED WITH A HALF-BURNT STICK ON A CAVE WALL.

# Why is it called Pometo?

`Pometo` is the Esperanto for ***little apple*** and `Pometo` is ***a little APL*** and Esperanto words are both easily pronouncable and usually available as domains.

# Why Pometo?

There are some problems that are elegantly solved with an `APL` syntax and `Pometo` is designed to write short, concise programmes that can be reasoned about to sole those problems.

Writing supervision trees and gen servers are not problems in that category.

`Pometo` is an auxiliary language - used to write library functions which are consumed in applications not written in `Pometo`.

[Read more about the rational and thinking](https://medium.com/@gordonguthrie/the-beam-needs-an-apl-y-language-6c5c998ba6d).

# Design Considerations

The normal developer of `Erlang`, `Elixir` and `LFE` applications should now know they are calling `Pometo` without reading the source code.

`Pometo` libraries will be listed on `hex`, the will use `rebar3` to build. The data structures in Pometo will be `lists` and `maps`, the datatypes `integers`, `floats`, `binaries` and `atoms`.

# Contributing To Pometo

`APL`'s traditionally have a REPL and the Pometo one is a standalone web application called `rappel`.

`rappel` is the ***runner*** in which development takes place.

If you wish to help develop `Pometo` you should install `rappel`.

The `rappel` github repository has instructions for how to get up and running with `rappel` and `Pometo` and start contributing code.

[You can find Rappel on github](http://github.com/gordonguthrie/rappel)

# Contents

{% for item in site.data.contents.toc %}
    <h3>{{ item.title }}</h3>
      <ul>
        {% for entry in item.subfolderitems %}
          <li><a href="{{ entry.url }}">{{ entry.page }}</a></li>
        {% endfor %}
      </ul>
  {% endfor %}
