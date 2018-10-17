FROM alpine:3.8
RUN wget "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz"
RUN tar xzf binaries-for-linux.tar.gz
RUN mv elm /usr/local/bin/

RUN apk update && apk add inotify-tools && apk add bash
COPY ./onchange /usr/local/bin/
RUN chmod +x /usr/local/bin/onchange

RUN mkdir -p /opt/app
WORKDIR /opt/app/
# ENTRYPOINT "bash -c"
 CMD ["elm", "--help"]
