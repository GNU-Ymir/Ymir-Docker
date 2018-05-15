fpm -s dir -t deb -n gyc-7 -v 7.3.0 -C /tmp/dir \
  -p gyc_7-VERSION_ARCH.deb \
  -d "g++-7 >= 7" \
  -d "gcc-7-base >= 7" \
  -d "libc6 >= 2.14" \
  -d "libgmp10 >= 2:5.0.1~" \
  -d "libgmidgard-7-dev >= 7" \
  -d "libisl19 >= 0.15" \
  -d "libmpc3" \
  -d "libmpfr6 >= 3.1.3" \
  -d "zlib1g >= 1:1.1.4" \
  usr/bin/gyc-7 usr/bin/x86_64-linux-gnu-gyc-7 usr/lib/gcc/x86_64-linux-gnu/7.3.0/ymir1


fpm -s dir -t deb -n libgmidgard-7-dev -C /tmp/midgard \
    -p libgmidgard_VERSION_ARCH.deb \
    -d "libc6 >= 2.17" \
    -d "gcc-7-base >= 7" \
    -d "libgcc1" \
    -d "zlib1g-dev" \
    -d "zlib1g >= 1:1.2.0" \
    usr/lib


