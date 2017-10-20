How to...
=========

This is a small guide to how to create a project using [Skeletor](https://gitlab.knoopje.com/communications/skeletor). It's pretty easy... let's go!

The One Project...
------------------

I usually creates in my system a *Projects* directory but you can create it with the name you want. The point is you have that directory and you put inside the following files:

- **rebar3**, the script to run the base project.
- **rebar.config**, the basic configuration to use *Skeletor*.

The content of `rebar.config` is as follow:

```
{plugins, [
    {skeletor, {git, "https://gitlab.knoopje.com/communications/skeletor.git", {branch, "master"}}}
]}.
```

The `rebar3` script could be downloaded from [here](https://s3.amazonaws.com/rebar3/rebar3). Ensure the script has running permissions.

Now, in that directory you can create a project. Figure out you want to create `buttonws` project:

```
./rebar3 new skel name=buttonws \
                  desc="My Button WS" \
                  author_name="Manuel Rubio" \
                  author_email="manuel.rubio@veon.com"
```

Of course you can customize that information to give your own project name, your own description, your name and your email.

The Created Project...
----------------------

A new directory called `buttonws` will be created in the same directory you are. So, for example, if you created the previous two files inside of *Projects* now you should to see `Projects/buttonws`.

The new directory `buttonws` has a `.gitignore` file among all of the other files. It's intended you perform a `git init` inside of the directory and push everything to the new repository you have to create for that new project.

Note that you DON'T need to put inside of the new directory the base (where are the two first files *rebar3* and *rebar.config*), only it's needed what is inside of the new created directory.

In a nutshell... following the `buttonws` example:

```
cd buttonws
git init
git remote add origin git@...
git push -u origin master
```

That's all, folks!
