FROM ubuntu:20.04

ENV TZ=Europe/Paris
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get -y update
RUN apt-get install -y sudo pkg-config git build-essential software-properties-common aspcud unzip curl wget
RUN apt-get install -y gcc-9 g++-9 flex autoconf automake libtool cmake emacs
RUN apt-get install -y gcc-multilib g++-multilib libgc-dev libgmp-dev

RUN useradd -ms /bin/bash ymir && echo "ymir:ymir" | chpasswd && adduser ymir sudo

USER ymir

WORKDIR /home/ymir/

