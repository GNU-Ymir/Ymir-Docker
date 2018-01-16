FROM ubuntu:17.04

RUN apt-get -y update && \
     apt-get -y install sudo pkg-config git build-essential \
     software-properties-common aspcud unzip curl wget && \     
     apt-get install -y gcc g++ flex autoconf automake libtool cmake
          

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

WORKDIR /home/ymir
RUN git clone --depth=1 https://github.com/GNU-Ymir/yruntime.git runtime
WORKDIR /home/ymir/runtime/runtime
RUN mkdir .build
WORKDIR /home/ymir/runtime/runtime/.build
RUN cmake ..
RUN make
RUN make install

WORKDIR /home/ymir/runtime/midgard
RUN mkdir .build
WORKDIR /home/ymir/runtime/midgard/.build
RUN cmake ..
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
RUN git clone --depth=1 https://github.com/GNU-Ymir/gymir.git ymir

RUN export PATH=$PATH:/home/ymir/gcc/gcc-install/bin:.
RUN echo "export PATH=$PATH:/home/ymir/gcc/gcc-install/bin:." >> /home/ymir/.bashrc

WORKDIR /home/ymir/gcc/gcc-src
RUN ./contrib/download_prerequisites

WORKDIR /home/ymir/gcc/gcc-build
RUN ../gcc-src/configure --prefix=$(pwd)/../gcc-install --enable-languages=c,ymir --disable-bootstrap --disable-multilib
RUN make
RUN make install



WORKDIR  /home/ymir

# overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]
