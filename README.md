# Ymir-Docker

This repository is used to compile a release of the Ymir compiler and runtime
Each version of gcc have a sub directory :
- builder_image/9/amd64
- builder_image/8/

**TODO** for the moment only the version 9 is working with amd64 arch. 

The version of ymir that is build is the last available on the repository [GNU-Ymir/gymir](https://github.com/GNU-Ymir/gymir).

## Compile a version :

First of all, you will need [docker](https://docs.docker.com/get-docker/)
Then launch the following commands : 
```bash
cd builder_image/{version}/{arch}
./config.sh
./build.sh
```

**WARNING** you may have permission problem if docker is not correctly configured->  [Configure Docker](https://docs.docker.com/engine/install/linux-postinstall/)

This could take a while (several hours for config.sh)

If everything worked as expected, the two files : gyc-{version}\_{sub_version}\_{arch}.deb and libgymidgard\_{version}\_{arch}.deb should be available in the directory :

- builder_image/{version}/{arch}/bin/

**INFO** The compilation is based on docker images, that are cached.
If you don't remove the images, the cached images will be used and the
compilation will be a lot faster than the first time, and the
config.sh script does not need to be executed each time

Then to update and compile a new release :  
```bash
cd builder_image/{version}/{arch}
./build.sh
```

## Run the tests

Once the compilation is done, the test can be launched

```bash
cd builder_image/{version}/{arch}
./run_tests.sh
```

**WARNING** you may have permission problem if docker is not correctly configured->  [Configure Docker](https://docs.docker.com/engine/install/linux-postinstall/)

## Create a docker container with a gyc installed in it

In each release directory, there is a Dockerfile that create a docker image usable to run gyc without installing anything
To create this image : 
```bash
cd builder_image/{version}/{arch}/
./create_docker.sh
```

Then you should have in your docker inventory the image : gnuymir/{version}-{arch}:latest
You can check the image : 
```bash
docker run -t -v $(pwd):/tmp -w /tmp gnuymir/{version}-{arch} --version 
```




