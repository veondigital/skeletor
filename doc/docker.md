Docker
======

The dockerization of the project consist to create a docker image where the project is inside. The properties this image has:

- It is compiled and created in a building image so, the compilation tools and the extra development libraries are not in the final image. The final image is smaller.

- The logs are throw to the console only, no files are used to take advantage of the logs handling of docker.

- It's intended to publish the images to a Docker Registry but the plugin for rebar let us to even export the images to be sent as a file.

Creating a release
------------------

The skeleton configures a prod profile with the skeletor plugin to get the docker commands available so, you only needs to run something like this:

```
./rebar3 as prod do docker build, docker release, docker push
```

The commands let you to build the project (the release), put the release in the release image (docker-release command) and push the image to the Docker Registry (docker-push).

The configuration for all of those commands could be find in the `rebar.config` file.

Ensure to add `tag` configuration in the production part before to run `docker push`. This way the upload of the image could be done correctly.
