# Pythia

[![Build](https://github.com/jonjau/pythia/actions/workflows/rust.yml/badge.svg)](https://github.com/jonjau/pythia/actions/workflows/rust.yml)
[![License:MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<div align="center">

  <img src="doc/pythia-icon.png" alt="logo" width="200" height="auto" />
  <h1>Pythia</h1>
  
  <p>
    A novel 'state change explorer' tool built with Rust and [Scryer Prolog](https://www.scryer.pl/).
  </p>
</div>

Run unit tests with database changes tracked in Pythia to help answer questions like:
- How many unique database records of a certain table does Test T1 change?
- Which tests cover the mutation of field F from 'ordered', 'processing', then 'completed'?
- Is state S is reachable from state S' in the context of Test T1 or Test T2?
- How many database modifications would it take, to take a database record from state S to state S'?
- If we were to run the functionality covered by Test T1 followed by that of Test T2 on the same database record, what would be the resulting state?

Run keeping data local with `docker` or `podman`, or try out the public demo at [pythia.jonjauhari.com](https://pythia.jonjauhari.com) (I don't always keep it running).

## Features

- Observe how individual records evolve across test runs
- Web-based UI for exploring changes interactively
- Anonymous sessions
- Continuous deployment workflow to AWS Fargate with Terraform

## Usage

Example pictures and text

## How to run

### Prerequisites

- Terraform
- AWS CLI
- Docker / Podman

## License

Pythia is currently licensed under the terms of both the MIT license and the Apache License (Version 2.0). See [`LICENSE-MIT`](/LICENSE-MIT) and [`LICENSE-APACHE`](/LICENSE-APACHE) for more details.

