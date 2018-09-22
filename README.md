# Dating

## Running the server locally
You can use the program [GHCID](https://github.com/ndmitchell/ghcid) to run and reload the server automatically on filechanges.
To install *GHCID* run this command in the terminal, while being in the *dating* folder. (Assuming you have **Stack** installed.)

`stack install ghcid`

Then you can run the following command:

`ghcid --command="stack ghci --main-is dating:exe:dating" --test="Main.main"`

The *test* argument is too specifically named. It can be used for tests, but it really just executes a function. Here we use it to execude our *main* function.


## Running Elm locally
While being in the *frontend* folder, you should run the following command (assuming you have **Elm** installed.)

`elm reactor`

It will write out the url for viewing your elm files. Usually http://localhost:8000.