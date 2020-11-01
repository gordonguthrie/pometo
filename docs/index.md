# What is Pometo?

`Pometo` is the the little `APL` for the `BEAM`, an auxiliary language to complement and use in your `Erlang`, `Elixir` and `LFE` applications.

# Status Of The Project

THIS PROJECT IS SUPER-EARLY, NOT SO MUCH ALPHA AS BEFORE THE DAWN OF WRITING, SOME INCHOATE SYMBOLS SCRAWLED WITH A HALF-BURNT STICK ON A CAVE WALL.

```warning
           _       _
     /\   | |     | |
    /  \  | |_ __ | |__   __ _
   / /\ \ | | '_ \| '_ \ / _` |
  / ____ \| | |_) | | | | (_| |
 /_/__  \_\_| .__/|_| |_|\__,_|_____                    ____        _      __
  / / |     | |               |  __ \                  / __ \      | |     \ \
 | || |     |_|_ _ __   __ _  | |  | | _____   _____  | |  | |_ __ | |_   _ | |
 | || |    / _` | '_ \ / _` | | |  | |/ _ \ \ / / __| | |  | | '_ \| | | | || |
 | || |___| (_| | | | | (_| | | |__| |  __/\ V /\__ \ | |__| | | | | | |_| || |
 | ||______\__,_|_| |_|\__, | |_____/ \___| \_/ |___/  \____/|_| |_|_|\__, || |
  \_\                   __/ |                                          __/ /_/
                       |___/                                          |___/
```

`Pometo` is a long way from being production usable - but if you want to help us get there, pile in.

# Why is it called Pometo?

`Pometo` is the Esperanto for ***little apple*** and `Pometo` is ***a little APL*** and Esperanto words are both easily pronounceable and usually available as domains.

# Why Pometo?

There are some problems that are elegantly solved with an `APL` syntax and `Pometo` is designed to write short, concise programmes that can be reasoned about to sole those problems.

Writing supervision trees and gen servers are not problems in that category.

`Pometo` is an auxiliary language - used to write library functions which are consumed in applications not written in `Pometo`.

[Read more about the rationale and thinking](https://medium.com/@gordonguthrie/the-beam-needs-an-apl-y-language-6c5c998ba6d).

# Design Considerations

The normal developer of `Erlang`, `Elixir` and `LFE` applications should not know they are calling `Pometo` without reading the source code.

`Pometo` libraries will be listed on `hex`, the will use `rebar3` to build. The data structures in Pometo will be `lists` and `maps`, the data-types `integers`, `floats`, `binaries` and `atoms`.

# Contributing To Pometo

`APL`'s traditionally have a REPL and the Pometo one is a standalone web application called `rappel`.

`rappel` is the ***runner*** in which development takes place.

If you wish to help develop `Pometo` you should install `rappel`.

The `rappel` github repository has instructions for how to get up and running with `rappel` and `Pometo` and start contributing code.

[You can find Rappel on github](http://github.com/gordonguthrie/rappel)

`Pometo` uses a `rebar3` plugin called `pometo_docs_to_tests` to turn the documentation into tests.

[You can find the Pometo Docs To Tests plugin on github](http://github.com/gordonguthrie/pometo_docs_to_tests)

There are pages in the Implementation Reference to help you get up and running as contributors to Pometo.

We are implementing a funded Open Source Maintainers programme in conjunction with [Code Your Future](https://codeyourfuture.io). CodeYourFuture is a refugee-founded and lead programme that trains disadvantaged people in software development.

The OSM programme is a post-graduation, pre-formal job transition that gives CYF graduates real world experience of software development in the wild. Because OSM contributors have no previous experience of Erlang, or Elixir or an APL this programme has a set of getting-started tasks defined.

(People who are not on the OSM programme are very welcome to ride along with the OSM introduction, but please don't just grab tasks off the OSM list without saying hello to the core team first.)

Please read our [Code of Conduct](./for_contributors/pometo_contributors_code_of_conduct.md).

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md) 

# Contents

<div>
{% for item in site.data.contents.toc %}
    <h3>{{ item.title }}</h3>
      <ul>
        {% for entry in item.subfolderitems %}
          <li><a href="{{ entry.url }}">{{ entry.page }}</a></li>
        {% endfor %}
      </ul>
  {% endfor %}
</div>