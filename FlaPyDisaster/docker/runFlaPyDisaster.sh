mkdir -p hurricaneEvents
docker run -p 5555:5555 -u $(id -u):10$(id -g) -v $(pwd)/hurricaneEvents:/FlaPySrc/FlaPyDisaster/FlaPyDisaster/users/events/hurricane -it flapydisaster