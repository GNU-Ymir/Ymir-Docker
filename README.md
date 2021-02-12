# Ymir-Docker

This repository is used to compile a release of the Ymir compiler and runtime
Each version of gcc have a sub directory :
- builder_image/9/
- builder_image/8/

**TODO** for the moment only the version 9 is working

The version of ymir that is build is the last available on the repository [GNU-Ymir/gymir](https://github.com/GNU-Ymir/gymir).

## Compile a version :

First of all, you will need [docker](https://docs.docker.com/get-docker/)

```bash
cd builder_image/{version}/
./config_{version}.sh
```

**WARNING** you may have permission problem if docker is not correctly configured->  [Configure Docker](https://docs.docker.com/engine/install/linux-postinstall/)

This could take a while

If everything worked as expected, the two files : gyc-{version}_{sub_version}_amd64.deb and libgymidgard_{version}.deb should be available in the directory :
- builder_image/{version}/bin/

**INFO** The compilation is based on docker images, that are cached.
If you don't remove the images, the cached images will be used and the
compilation will be a lot faster than the first time

## Run the tests

Once the compilation is done, the test can be launched

```bash
cd builder_image/{version}/
./run_tests.sh
```

**WARNING** you may have permission problem if docker is not correctly configured->  [Configure Docker](https://docs.docker.com/engine/install/linux-postinstall/)