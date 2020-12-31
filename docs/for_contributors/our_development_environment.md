# Our Development Environment

To develop `pometo` (and `rappel` and the `pometo_docs_to_tests` and `pometo_docs_to_ct_tests` `rebar3` plugins) we use docker as an isolation unit. This means that you don't need to worry about:

* getting Erlang installed
* which version of Erlang and Elixir you have installed
* if your dev environment is compatible with the `rappel` application as well
* what tools you need to have installed

Docker is mechansism that lets you start your own version of an operating system (in this case Linux) that will run inside your host machine wether it runs OS X, Windows or another Linux flavour.

You need to have `docker` installed on your machine.

[https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/)

You need to clone this repo, the `Pometo` repo.

`git clone git@github.com:gordonguthrie/pometo.git`

Once the source code is available we bring up the `pometo` docker container

```
cd $GITROOT/pometo
docker-compose build
docker-compose up
```

This will leave docker running in that terminal. Switch to another terminal:

```
cd $GITROOT/pometo/scripts
sh start_pometo.sh
```

This will log you into the docker instance.

`pometo` app is in the directory `/pometo` inside docker. Your local file system is mounted - so if you edit files on your desktop the changes show up in the docker.

You are ready to go.

If you want to use the REPL `Rappel` locally you should [install](https://github.com/gordonguthrie/rappel/blob/master/README.md) that too.

If you have never used Docker before had a look at the file `docker/pometo.dockerfile`. This is the file that builds the operating system image. It starts with an OS with Elixir pre-installed and then installs a range of software using Debian's `apt-get` (the Elixir docker base OS is a Debian variant). We use the same base image for `Pometo` as for `Rappel` and as that is written in Elixir (and Elixir depends on Erlang) this is the simplest way of keeping the two projects in sync.

If the docker file defines what the encapuslated operating system will have installed, then the file `docker-compose.yml` defines how we bring up the operating system, what ports are opened what file systems are mounted.

You can see that we mount local directories into the docker file. This means that, if your main machine is a Mac, you can edit a file in OS X and the changes appear ***inside*** the docker container running Linux.
