FROM ubuntu:20.04
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get -y update
RUN apt-get install -y --no-install-recommends sudo pkg-config git build-essential software-properties-common aspcud unzip curl wget
RUN apt-get install -y --no-install-recommends gcc-9 g++-9 flex autoconf automake libtool cmake emacs
RUN apt-get install -y --no-install-recommends gcc-multilib g++-multilib libgc-dev libgmp-dev
RUN rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo

WORKDIR /home/ymir/

USER ymir
RUN mkdir gcc
WORKDIR /home/ymir/gcc
RUN mkdir gcc-src
RUN mkdir gcc-build
RUN mkdir gcc-install
RUN git clone --depth=1 git://gcc.gnu.org/git/gcc.git gcc-src
WORKDIR /home/ymir/gcc/gcc-src
RUN git fetch --tags --depth=1
RUN git checkout releases/gcc-9.3.0

WORKDIR /home/ymir/gcc/gcc-src/gcc
RUN git clone --depth=1 https://github.com/GNU-Ymir/gymir.git ymir

WORKDIR /home/ymir/gcc/gcc-src
RUN ./contrib/download_prerequisites

WORKDIR /home/ymir/gcc/gcc-build
RUN ../gcc-src/configure --enable-languages=c,ymir --with-gcc-major-version-only --program-suffix=-9 --prefix=/usr --program-prefix=x86_64-linux-gnu- --libexecdir=/usr/lib --libdir=/usr/lib --with-sysroot=/ --with-arch-directory=amd64 --enable-multiarch --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu

CMD ["/bin/bash"]
