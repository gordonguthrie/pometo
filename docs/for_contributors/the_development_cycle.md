# The Development Cycle

## Basic Dev Cycle Of APL Features

The core development cycle is:

* write a documentation page description the operation that you wish to add and the results it should show
  * create your document not in the final directory but in `docs/_work_in_progress`
	* documentation pages are turned into tests - the title of test is the filename - so your docs page should not have the same name as any other page
  * feel free to raise a PR on your new docs page and get a review before starting work on implementing it
* generate the new test suite with the command `BUILDWIP=true rebar3 pometo_docs_to_tests`
    * the `pometo_docs_to_tests` `rebar3` plugin is fairly stable but does occasionally change
    * to fetch and run a new version delete the directory `_build/default/plugins/pometo_docs_to_test` and your next call of any `rebar3` command will fetch and install the latest version
    * the normal usage (without the Environment variable `BUILDWIP`) will build the documents with the `_work_in_progress` directory.
    * you can commit documents pages and have them merged when they are in the `_work_in_progress` directory - think of it as ***booking out*** that feature to work one
* run the tests with `rebar3 eunit`
    * you can run a single test suite with a command like `rebar3 eunit --module=complex_numbers_tests`
* when your code is working and the WIP test suite is passing raise a pull request
    * it should include a `git mv` to move the docs page to its destination
    * it should include changes to `docs/_data/contents.yml` to publish your new documents to the [main docs website](http://gordonguthrie.github.io/pometo)
    * you should run Jekyll and check that the site builds correctly (***Note***: the logo won't appear when running locally).
        * to run Jekyll locally `cd` to `/docs` and run the batch file `run_jekyll.sh` - the docs site will build and can be seen on `http://localhost:5000`
    * if you have implemented a new symbol you need to make that symbol available in `rappel` the `pometo` REPL. (This involves deleting a line of embedded CSS please see the section [Enabling New Symbols In Rappel](#enabling-new-symbols-in-rappel)

The `pometo_docs_to_ct_tests` plugin works in the same way as `pometo_docs_to_tests` and takes the same `BUILDWIP` environment variable - it generates tests in `tests/generated_common_tests` and when the tests are run the results are stored under `_build/test/log/index.html`.

***GOTCHA***: some of the tests (see the parser notes page for instance) have significant trailing white space - if you get mysterious ***my test is failing but the output looks the same*** this is probably it.

Please read [Our Testing](./our_testing.md).

Typically the results look like:

```apl
┌───┐ ┌───┐ ┌───┐        
│1 2│ │3 4│ │1 2│   4   5
└───┘ └───┘ └───┘        
```
The 4 and 5 have 'hidden' padding left, right top and bottom.

If you editor clears trailing white space this test will fail.

Code failures in tests are fairly hard to debug because the interpreter and compiler both capture and sanitize runtime errors.

The easiest way to proceed is to copy the input string of the test into the `runner` module and then invoke it with the `run.sh` bash command in `%ROOT`

This runs the same code as a plain Erlang function and you get the full, raw crash report.

There is a pain here - to see debugging output when running test suites you need to use the `?debugFmt` macro (it has the same arguments as `io:format/2` (there is no equivalent to `io:format/1`). It is defined in the eunit include file and added to source code with `-include_lib("eunit/include/eunit.hrl").`

These two output methods don't work with each other `io:format`s don't show when running tests and `?debugFmt`s don't show when running code :-(

This can tend to lead to dirty code with extra `io:format`s and `?debugFmt`s littering the code. Please check your diffs before submitting a PR.

To make this easier there is a `debug` function in `pometo_stdlib` which takes complex nested `$ast¯` and other records and prints them nicely. It also has a single macro which can be used to switch printing from `io:format` to `?debugFmt`.

## Basic Dev Cycle For Non-Conformant With Dyalog Features

It is the same basic dev cycle with one pre-step.

Please add a section at the end of the design docs page in the docs directory explaining:

* what you want to do
* why not conforming to Dyalog APL 18.0 is a good idea
* why you want to extend the language or add new features to make it work better with `Erlang`, `Elixir` and `LFE`.
