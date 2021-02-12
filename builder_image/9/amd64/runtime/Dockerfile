FROM ubuntu:20.04
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update -y

RUN apt-get install -y --no-install-recommends sudo pkg-config git build-essential software-properties-common aspcud unzip curl wget
RUN apt-get install -y --no-install-recommends gcc-9 g++-9 flex autoconf automake libtool cmake emacs
RUN apt-get install -y --no-install-recommends libgc-dev libbfd-dev

RUN sudo apt-get install -y ruby ruby-dev rubygems build-essential
RUN gem install --no-document fpm

RUN apt-get install apt-transport-https ca-certificates -y
RUN update-ca-certificates
RUN rm -rf /var/lib/apt/lists/*

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo
WORKDIR /home/ymir/

ADD gyc-9_9.3.0_amd64.deb /home/ymir/gyc.deb
RUN dpkg -i gyc.deb

RUN git clone https://github.com/GNU-Ymir/yruntime.git runtime
WORKDIR /home/ymir/runtime/runtime
RUN mkdir .build
WORKDIR /home/ymir/runtime/runtime/.build
RUN cmake ..
RUN make -j4
RUN make install DESTDIR=/tmp/midgard

WORKDIR /home/ymir/runtime/midgard
RUN mkdir .build
WORKDIR /home/ymir/runtime/midgard/.build
RUN cmake ..
RUN make -j4
RUN make install DESTDIR=/tmp/midgard

WORKDIR /home/ymir/runtime/midgard
RUN mkdir -p /tmp/midgard/usr/lib/gcc/x86_64-linux-gnu/9/include/ymir/
RUN cp -r core /tmp/midgard/usr/lib/gcc/x86_64-linux-gnu/9/include/ymir/
RUN cp -r etc /tmp/midgard/usr/lib/gcc/x86_64-linux-gnu/9/include/ymir/
RUN cp -r std /tmp/midgard/usr/lib/gcc/x86_64-linux-gnu/9/include/ymir/

RUN mkdir /home/ymir/bin
WORKDIR /home/ymir/bin
RUN fpm -s dir -t deb -n libgmidgard-9 -v 9.3.0 -C /tmp/midgard/ \
	-p libgmidgard_VERSION_ARCH.deb \
	-d "libc6 >= 2.31" \
	-d "gcc-9-base >= 9" \
	-d "libgcc1" \
	-d "zlib1g-dev" \
	-d "zlib1g >= 1:1.2.0" \
	usr/lib


CMD ["/bin/bash"]



