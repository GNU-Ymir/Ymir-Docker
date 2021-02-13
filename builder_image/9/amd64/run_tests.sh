cp bin/* test/
docker build --no-cache -t test_9_3 test/
docker container rm $(docker ps -a -q)
rm test/*.deb
