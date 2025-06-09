# Stage 1: Build
FROM rust:1.86-bullseye as builder

WORKDIR /pythia

# Copy full source and build
COPY . .
RUN cargo clean && cargo build --release

# Stage 2: Runtime
FROM debian:bullseye-slim

WORKDIR /pythia

COPY --from=builder /pythia/target/release/pythia .

COPY --from=builder /pythia/data ./data

CMD ["./pythia"]