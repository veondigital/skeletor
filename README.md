skeletor
=====

An Erlang Rebar Template to bootstrap HTTP and XMPP based Applications.
It consists in an Application Core, an HTTP Handler, an XMPP handler, a prometheus metric collector and also a Redis connectivity module.

It's also a plugin for dockerize the release. Check [docker](doc/docker.md) doc for further information.

![Skeletor](https://github.com/veondigital/skeletor/blob/development/skeletor.jpg)

Build
-----

```
./rebar3 compile
```

Use
---

Create a base directory for your projects having `rebar3` script and a `rebar.config` file only with this configuration:

```
{plugins, [
    {skeletor, {git, "https://github.com/veondigital/skeletor.git", {branch, "master"}}}
]}.
```

And then:

```
./rebar3 new skel name=veon \
                  desc="A base service" \
                  author_name="Manuel Rubio" \
                  author_email="manuel.rubio@veon.com" \
                  tag="midomain.com/erlang/veon"
```

This generates a new directory using the project name used on rebar3 command. Containing a ready to se base for your project.

For further information read the [how-to here!](doc/how_to.md)

Enjoy!
