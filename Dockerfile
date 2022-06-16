# syntax=docker/dockerfile:1
FROM clojure
RUN mkdir /app
WORKDIR /app
ADD . /app/
COPY . /app/
CMD ["lein", "run", "configs/all-survive/all-survive0.edn", "0"]