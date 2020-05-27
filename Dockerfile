FROM debian:jessie
RUN apt-get update && apt-get install -yq build-essential autoconf libnetcdf-dev libxml2-dev libproj-dev subversion valgrind nano


COPY code_6490 /app/src
COPY currentVersion /app/model
RUN cd /app/src && aclocal && autoheader && autoconf && automake -a && ./configure && make && make install

WORKDIR /app/model
#CMD ./run_command.sh
