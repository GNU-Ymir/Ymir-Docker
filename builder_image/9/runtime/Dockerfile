FROM ubuntu:20.04
ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get install -y --no-install-recommends gcc-9 g++-9 flex autoconf automake libtool cmake emacs git
RUN apt-get install -y --no-install-recommends libgc-dev libbfd-dev

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo
WORKDIR /home/ymir/

ADD ./bin/gyc-9_9.3.0_amd64.deb /home/ymir/gyc.deb
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

CMD ["/bin/bash"]


