# config-9.sh

This script create 4 docker image :
- base_9_3: download of the source and configure the image to be ready to build, this image can be cached
- build_9_3: build the base_9_3 image, this image can be cached
- update_9_3: update the build_9_3 image (that is cached) with the last update of ymir, and build it
- bin_9_3: use the update_9_3 image and create the deb file for gyc-9

Then, it retreive the deb file from the bin image, and remove update_9_3 and bin images
**WARNING** it kills all docker container

the test container can be used to verify the created binary