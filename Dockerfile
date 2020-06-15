FROM debian:jessie
RUN apt-get update && apt-get install -yq build-essential autoconf libnetcdf-dev libxml2-dev libproj-dev subversion valgrind dos2unix nano


COPY .svn /app/.svn
COPY atlantisCode /app/atlantis
COPY currentVersion /app/model

RUN cd /app/atlantis && aclocal && autoheader && autoconf && automake -a && ./configure && make && make install

WORKDIR /app/model
