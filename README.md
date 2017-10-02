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

Add the plugin to your rebar config:

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

In the generated `README.md` file you can find more information.

Enjoy!
