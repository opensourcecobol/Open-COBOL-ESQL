#
# Copyright 2019, Tokyo System House Co., Ltd. <opencobol@tsh-world.co.jp>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#
# opensoruce COBOL v1.5.2J
# Open COBOL ESQL  v1.2.0
#

FROM centos:centos7
LABEL maintainer="n-isaka@tsh-world.co.jp"

# install yum package
RUN yum install -y gcc gcc-devel gcc-c++ make bison flex gmp-devel ncurses-devel postgresql-devel postgresql-server autoconf psql

# install opensource COBOL
ADD https://github.com/opensourcecobol/opensource-cobol/archive/v1.5.2J.tar.gz opensource-cobol-1.5.2J.tar.gz
RUN tar zxvf opensource-cobol-1.5.2J.tar.gz &&\
    cd /opensource-cobol-1.5.2J/vbisam &&\
    ./configure --prefix=/usr/ &&\
    make install &&\
    cd /opensource-cobol-1.5.2J &&\
    ./configure --prefix=/usr/ --with-vbisam &&\
    make install &&\
    cd / &&\
    rm -rf opensource-cobol-1.5.2J.tar.gz

# install OCESQL
  ### The ADD instruction copies new files, directories or remote file URLs from <src> and adds them to the filesystem of the image at the path <dest>. 
  ### If you want to run another version or your own test, rewrite it as needed.
ADD https://github.com/opensourcecobol/Open-COBOL-ESQL/archive/refs/heads/develop.tar.gz Open-COBOL-ESQL-develop.tar.gz
RUN tar zxvf Open-COBOL-ESQL-develop.tar.gz &&\
    cd /Open-COBOL-ESQL-develop &&\
    ./configure --prefix=/usr/ &&\
    make install &&\
    cd / &&\
    rm -rf Open-COBOL-ESQL-1.2.tar.gz

ENTRYPOINT ["/bin/bash"]

RUN ls
