FROM updated_9_3

USER root
RUN sudo apt-get install -y --no-install-recommends ruby ruby-dev rubygems build-essential
RUN gem install --no-document fpm
RUN rm -rf /var/lib/apt/lists/*

USER ymir
WORKDIR /home/ymir
RUN mkdir bin
WORKDIR bin
RUN ln -s x86_64-linux-gnu-gyc-9.3 /tmp/gyc/usr/bin/gyc-9
RUN ln -s gyc-9 /tmp/gyc/usr/bin/gyc
RUN cp -r /tmp/gyc/usr/lib/gcc/x86_64-linux-gnu/9.3.0/ /tmp/gyc/usr/lib/gcc/x86_64-linux-gnu/9/
RUN ln -s 9 /tmp/gyc/usr/lib/gcc/x86_64-linux-gnu/9.3.0

RUN fpm -s dir -t deb -n gyc-9 -v 9.3.0 -C /tmp/gyc \
	-p gyc-9_VERSION_ARCH.deb \
	-d "g++-9 >= 9" \
	-d "gcc-9-base >= 9" \
	-d "libc6 >= 2.31" \
	-d "libgmp10 >= 2:6.2.0~" \
	-d "libmpc3" \
	-d "libmpfr6 >= 4.0.2" \
	-d "zlib1g >= 1:1.2.0" \
	usr/bin/gyc-9 usr/bin/gyc usr/bin/x86_64-linux-gnu-gyc-9.3 usr/lib/gcc/x86_64-linux-gnu/9/ymir1 usr/lib/gcc/x86_64-linux-gnu/9.3.0

CMD ["/bin/bash"]
