# Stage 1: Build
FROM rust:1.86-bullseye AS builder

WORKDIR /pythia

RUN curl -sSL -o /usr/local/bin/tailwindcss \
    https://github.com/tailwindlabs/tailwindcss/releases/latest/download/tailwindcss-linux-x64 && \
    chmod +x /usr/local/bin/tailwindcss

# Copy full source and build
COPY askama.toml Cargo.lock Cargo.toml ./
COPY ./src ./src
COPY ./templates ./templates
RUN tailwindcss -i src/input.css -o static/style.css --minify
RUN cargo clean && cargo build --release

# Stage 2: Runtime
FROM debian:bullseye-slim

WORKDIR /pythia

COPY --from=builder /pythia/static ./static
COPY --from=builder /pythia/target/release/pythia .

CMD ["./pythia"]