# Stage 1: Build
FROM rust:1.86 as builder

WORKDIR /pythia

# Pre-cache dependencies
COPY Cargo.toml Cargo.lock ./
RUN mkdir src && echo "fn main() {}" > src/main.rs
RUN cargo build --release
RUN rm -rf src

# Copy full source and build
COPY . .
RUN cargo build --release

# Stage 2: Runtime
FROM debian:bullseye-slim

WORKDIR /pythia
COPY --from=builder /pythia/target/release/pythia .

CMD ["./pythia"]