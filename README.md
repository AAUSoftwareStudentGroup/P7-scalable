# Dating

## Running everything in docker
Assumming you have **Docker** installed, run the following while being in the `dating` folder.

`docker-compose up`

It will set everything up and run [GHCID](https://github.com/ndmitchell/ghcid), which automatically rebuilds and executes the Haskell project. You can access the *API* on http://localhost:8080/

## Running Elm locally
While being in the *frontend* folder, you should run the following command (assuming you have **Elm** installed.)

`elm reactor`

It will write out the url for viewing your elm files. Usually http://localhost:8000/.


## Useful Docker Things

### Commands

`docker-compose up` Builds, (re)creates, starts, and attaches to containers for a service.
`docker-compose down` Stops containers and removes containers, networks, volumes, and images created by `up`.

`docker-compose start` Starts existing containers for a service.
`docker-compose stop` Stops running containers without removing them. They can be started again with `start`.

`docker-compose ps` Lists containers.
`docker-compose images` Lists images.

### Lingo

A *Container* is a running *Image*.