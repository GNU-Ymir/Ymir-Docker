mkdir bin
docker build --no-cache -t updated_9_3 update/
docker build --no-cache -t bin_9_3 fpm/
id=$(docker create bin_9_3)
docker cp $id:/home/ymir/bin/gyc-9_9.3.0_amd64.deb ./bin/
cp bin/gyc-9_9.3.0_amd64.deb runtime/
docker build -t runtime_9_3 runtime/

rm runtime/gyc-9_9.3.0_amd64.deb
id=$(docker create runtime_9_3)
docker cp $id:/home/ymir/bin/libgmidgard_9.3.0_amd64.deb ./bin/

docker image prune
docker container rm $(sudo docker ps -a -q)
