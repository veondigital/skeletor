skeletor
=====

This project is the base to create other Erlang projects. This file is a small guide to give you an idea of what is provided by the skeleton and what should be implemented.

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
    {skeletor, {git, "git@git.knoopje.com:communications/skeletor.git", {branch, "master"}}}
]}.
```

And then:

```
./rebar3 template skel name=veon \
                       desc="A base service" \
                       author_name="Manuel Rubio" \
                       author_email="manuel.rubio@veon.com"
```

This generates a new directory called as the name you put as a variable with all of the base content you need to start to create your project.

Enjoy!
