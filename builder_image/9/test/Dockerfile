FROM ubuntu:20.04

ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get -y update
RUN apt-get install -y sudo pkg-config git build-essential software-properties-common aspcud unzip curl wget
RUN apt-get install -y gcc-9 g++-9 flex autoconf automake libtool cmake emacs
RUN apt-get install -y gcc-multilib g++-multilib libgc-dev libgmp-dev python3 python3-pip
RUN pip3 install pyaml

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo

USER ymir
ADD ./bin/gyc-9_9.3.0_amd64.deb /home/ymir/gyc.deb
ADD ./bin/libgymidgard_9_amd64.deb /home/ymir/libmidgard.deb

RUN dpkg -i gyc.deb
RUN dpkg -i libmidgard.deb

RUN git clone https://github.com/GNU-Ymir/Ymir-Test.git test
WORKDIR /home/ymir/test

RUN python3 run-test.txt playbook.yml

CMD ["/bin/bash"]
