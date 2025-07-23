# --- Build Stage ---
    FROM haskell:9.2 as builder

    WORKDIR /app
    
    # Install hpack for YAML to Cabal conversion
    RUN cabal update && cabal install hpack
    
    # Copy source and convert package.yaml to .cabal
    COPY . .
    RUN hpack
    
    # Build dependencies first
    RUN cabal update && cabal build --only-dependencies
    
    # Build the application
    RUN cabal build
    
    # --- Runtime Stage ---
    FROM debian:bullseye-slim
    
    # Install needed system libraries
    RUN apt-get update && apt-get install -y libpq-dev ca-certificates && rm -rf /var/lib/apt/lists/*
    
    WORKDIR /app
    
    # Copy binary from build stage
    COPY --from=builder /app/dist-newstyle/build/*/*/todo-list-app-*/x/todo-list-app-exe/build/todo-list-app-exe/todo-list-app-exe /app/server
    
    EXPOSE 8000
    
    CMD ["/app/server"]
    