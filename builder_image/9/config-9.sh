rm gyc-9_9.3.0_amd64.deb
docker build -t base_9_3 base/
docker build -t build_9_3 make/
docker build --no-cache -t updated_9_3 update/
docker build --no-cache -t bin_9_3 fpm/
id=$(docker create bin_9_3)
docker cp $id:/home/ymir/bin/gyc-9_9.3.0_amd64.deb .
docker image prune
sudo docker container rm $(sudo docker ps -a -q)
sudo docker image rm bin_9_3
sudo docker image rm updated_9_3
