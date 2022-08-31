FROM debian:jessie
RUN apt-get update && apt-get install -yq build-essential autoconf libnetcdf-dev libxml2-dev libproj-dev subversion valgrind dos2unix nano

COPY atlantisCode/v6536/atlantis /app/atlantis
COPY atlantisCode/v6536/.svn /app/.svns

RUN cd /app/atlantis && aclocal && autoheader && autoconf && automake -a && ./configure && make && make install

WORKDIR /app/model
CMD ./RunAtlantis.sh