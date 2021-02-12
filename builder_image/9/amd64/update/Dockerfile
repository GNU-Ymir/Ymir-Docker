FROM build_9_3

USER ymir
WORKDIR /home/ymir/gcc/gcc-src/gcc/ymir
RUN git pull origin master

WORKDIR /home/ymir/gcc/gcc-build/
RUN make -j4
RUN make install DESTDIR=/tmp/gyc

CMD ["/bin/bash"]
