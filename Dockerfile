FROM ubuntu:18.04

RUN apt-get -y update && \
     apt-get -y install sudo pkg-config git build-essential \
     software-properties-common aspcud unzip curl wget && \     
     apt-get install -y gcc g++ flex autoconf automake libtool cmake emacs && \
     apt-get install -y gcc-multilib g++-multilib

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo

WORKDIR /home/ymir
RUN git clone git://github.com/ivmai/libatomic_ops.git
RUN git clone git://github.com/ivmai/bdwgc.git
RUN ln -s  /home/ymir/libatomic_ops /home/ymir/bdwgc/libatomic_ops
WORKDIR /home/ymir/bdwgc
RUN autoreconf -vif
RUN automake --add-missing
RUN ./configure --enable-cplusplus
RUN make
RUN make install

USER ymir

WORKDIR /home/ymir
RUN mkdir gcc	 
WORKDIR /home/ymir/gcc
RUN mkdir gcc-src
RUN mkdir gcc-build
RUN mkdir gcc-install
RUN git clone --depth=1 git://gcc.gnu.org/git/gcc.git gcc-src
WORKDIR /home/ymir/gcc/gcc-src/gcc
RUN git branch gcc-7_3_0-release
RUN git clone --depth=1 https://github.com/GNU-Ymir/gymir.git ymir

RUN export PATH=$PATH:/home/ymir/gcc/gcc-install/bin:.
RUN echo "export PATH=$PATH:/home/ymir/gcc/gcc-install/bin:." >> /home/ymir/.bashrc

RUN LIBRARY_PATH=/usr/lib32:$LIBRARY_PATH
RUN export LIBRARY_PATH

WORKDIR /home/ymir/gcc/gcc-src
RUN ./contrib/download_prerequisites

WORKDIR /home/ymir/gcc/gcc-build
RUN ../gcc-src/configure --enable-languages=c,ymir --program-suffix=-7.3 --prefix=/usr --program-prefix=x86_64-linux-gnu- --libexecdir=/usr/lib --libdir=/usr/lib --with-sysroot=/ --with-arch-directory=amd64 --enable-multiarch --with-arch-32=i686 --with-abi=m64 --with-multilib-list=m32,m64,mx32 --enable-multilib --enable-checking=release --build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu
RUN make -j3
RUN make install DESTDIR=/tmp/gyc
WORKDIR  /home/ymir

WORKDIR /home/ymir
RUN git clone --depth=1 https://github.com/GNU-Ymir/yruntime.git runtime
WORKDIR /home/ymir/runtime/runtime
RUN mkdir .build
WORKDIR /home/ymir/runtime/runtime/.build
RUN cmake ..
RUN make
RUN make install DESTDIR=/tmp/midgard

WORKDIR /home/ymir/runtime/midgard
RUN mkdir .build
WORKDIR /home/ymir/runtime/midgard/.build
RUN cmake ..
RUN make
RUN make install DESTDIR=/tmp/midgard


WORKDIR /tmp/
RUN git clone --depth=1 https://github.com/GNU-Ymir/Ymir-Docker.git ymirDocker
WORKDIR /tmp/ymirDocker/builder_image
RUN chmod +x fmp_creator.sh
RUN ./fmp_creator.sh

# overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]
