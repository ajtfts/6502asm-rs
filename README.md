# Docker Dev Env for Rust 

# Running tests

This command builds a docker image with the code of this repository and runs the repository's tests

```sh
./build_docker.sh my_app
docker run -t my_app ./run_tests.sh
```

# Running a specific test

This example runs a single test with the name "test_add"

```sh
./build_docker.sh my_app
docker run -t my_app ./run_tests.sh test_add
```
