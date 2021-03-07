echo rmdir gitlab/9.3.0/ | ftp ftp.cluster026.hosting.ovh.net
echo mkdir gitlab/9.3.0/ | ftp ftp.cluster026.hosting.ovh.net

curl -T bin/gyc-9_9.3.0_amd64.deb ftp://ftp.cluster026.hosting.ovh.net/gitlab/9.3.0/ --user emilecn:$(pass ovh)
curl -T bin/libgmidgard_9.3.0_amd64.deb ftp://ftp.cluster026.hosting.ovh.net/gitlab/9.3.0/ --user emilecn:$(pass ovh)
