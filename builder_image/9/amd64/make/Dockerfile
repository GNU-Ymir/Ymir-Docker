FROM base_9_3

WORKDIR /home/ymir/gcc/gcc-build/
RUN make
RUN make install DESTDIR=/tmp/gyc

CMD ["/bin/bash"]
