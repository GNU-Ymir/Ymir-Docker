docker build -t base_9_3 base/
docker build -t build_9_3 make/

docker image prune
docker container rm $(sudo docker ps -a -q)
