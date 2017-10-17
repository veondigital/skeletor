skeletor
=====

An Erlanhg Rebar Template to bootstrap HTTP and XMPP based Applications. 
It consists in an Application Core, an HTTP Handler, an XMPP handler, a prometheus metric collector and also a Redis connectivity module.

![Skeletor](https://gitlab.knoopje.com/communications/skeletor/raw/development/skeletor.jpg)

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
    {skeletor, {git, "https://gitlab.knoopje.com/communications/skeletor.git", {branch, "master"}}}
]}.
```

And then:

```
./rebar3 new skel name=veon \
                  desc="A base service" \
                  author_name="Manuel Rubio" \
                  author_email="manuel.rubio@veon.com"
```

This generates a new directory using the project name used on rebar3 command. Containing a ready to se base for your project.

Enjoy!
