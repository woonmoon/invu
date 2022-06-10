# syntax=docker/dockerfile:1
FROM clojure
RUN mkdir /app
WORKDIR /app
ADD . /app/
COPY . /app/
# ADD . /app/
CMD ["lein", "run"]