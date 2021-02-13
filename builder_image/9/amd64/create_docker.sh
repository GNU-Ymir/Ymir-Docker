cp bin/* docker/
docker build -t gnuymir/9.3.0-amd64 docker/
rm docker/*.deb
