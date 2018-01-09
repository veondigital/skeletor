Docker
======

The dockerization of the project consist to create a docker image where the project is inside. The properties this image has:

- It is compiled and created in a building image so, the compilation tools and the extra development libraries are not in the final image. The final image is smaller.

- The logs are throw to the console only, no files are used to take advantage of the logs handling of docker.

- It's intended to publish the images to a Docker Registry but the plugin for rebar let us to even export the images to be sent as a file.

Creating the building image
---------------------------

The first step to create the artifact is the compilation. To achieve that we need to have the correct environment where the Erlang have to be compiled with all of the tools we'll need.

Those tools are not needed anymore in the release so, they are only available in this image.

This image is created using the `Dockerfile.build` file. The mission of this file is:

- Create an image with the specific Erlang/OTP version (specified through the `ERLANG_VSN` environment variable).

- Run the image to build the project.

The project is built inside of the image but the code used and the result is mount from outside so, when the container finalize its action, the artifact is available outside.

You can perform this action using the command:

```
ERLANG_VSN=19.3 ./rebar3 as prod docker build
```

Note that you can avoid to use `ERLANG_VSN` environment variable if you want to release using the Erlang/OTP 19.3 version.

This action is performed in a separate way just in case you want to use it for compilation only. To obtain the artifact.

Creating the release image
--------------------------

To obtain a docker image with the artifact inside and ready to be executed you can use the command:

```
./rebar3 as prod docker release
```

This command uses a minimal docker image and installs only the software needed to run the artifact.

Note that the artifact is containing Erlang so, its' not needed to install it in the docker image.

The image is configured with the specific entrypoint to run the system in foreground (without user interaction) and even the logs are redirected to the standard outputs (stdout and stderr).

You can check `script/start-prod.sh` to check the environment variables you can use to run the release.

Pushing the release
-------------------

The generated image is needed to be pushed to the private or public docker registry. The command to perform this action is:

```
./rebar3 as prod docker push
```

Note that this command is performing a local compilation to retrieve the version number. The actions this command does are:

- Tag the image to be pushed with the correct name (adding the server name in addition to the name as URL) and the version (retrieved from git).

- Push the docker image to the docker registry.

Be careful and keep in mind to use correctly the `git tag` to ensure the version is correctly formatted and used.

Note that you have to configure the name of the image to be pushed in the `rebar.config` file. There you'll find in the profile `prod` this entry:

```erlang
{docker, [
    {tag, "domain.com/erlang/name"}
]},
```

And then, the `domain.com/erlang/name` will be used to tag the image and perform the push to the private or public docker registry.

In a nutshell...
----------------

The skeleton configures a prod profile with the skeletor plugin to get the docker commands available so, you only needs to run something like this:

```
./rebar3 as prod do docker build, docker release, docker push
```

The commands let you to build the project (the release), put the release in the release image (docker-release command) and push the image to the Docker Registry (docker-push).

The configuration for all of those commands could be find in the `rebar.config` file.

Ensure to add `tag` configuration in the production part before to run `docker push`. This way the upload of the image could be done correctly.
