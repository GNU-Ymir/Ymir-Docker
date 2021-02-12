cp bin/* test/
docker build --no-cache -t test_9_3 test/
docker container rm $(sudo docker ps -a -q)
rm test/*.deb
