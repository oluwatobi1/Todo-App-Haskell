# ─── Stage 1: Build Stage (using official Haskell image) ───────────────
FROM haskell:9.6-slim-bullseye AS builder

RUN apt-get update \
  && apt-get install -y wget ca-certificates lsb-release \
  && echo "deb http://apt.postgresql.org/pub/repos/apt \
      $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
  && apt-get update \
  && apt-get install -y pkg-config libpq-dev


RUN cabal update && cabal install hpack

WORKDIR /app
COPY . .
RUN hpack

# Build dependencies first
RUN cabal build --only-dependencies


RUN cabal install -O2 \
--installdir=/app/bin \
--install-method=copy \
--overwrite-policy=always


# Strip debug symbols to shrink the binary
RUN strip /app/bin/todo-list-app-exe

# ─── Stage 2: Runtime Stage (minimal image) ─────────────────────────────
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y \
      libpq5 \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /app/bin/todo-list-app-exe /app/server

EXPOSE 8000
CMD ["/app/server"]
