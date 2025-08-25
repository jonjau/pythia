# Pythia

[![Build](https://github.com/jonjau/pythia/actions/workflows/rust.yml/badge.svg)](https://github.com/jonjau/pythia/actions/workflows/rust.yml)
[![License:MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Pythia is a novel 'state change explorer' tool built with Rust and [Scryer Prolog](https://www.scryer.pl/).

Run unit tests with database changes tracked in Pythia to help answer questions like:
- How many unique database records of a certain table does Test T1 change?
- Which tests cover the mutation of field F from 'ordered', 'dispatched', then 'delivered'?
- Is state S is reachable from state S' in the context of Test T1 or Test T2?
- If field F changes, do any other fields tend to change with it?
- How many database modifications would it take, to take a database record from state S to state S'?
- If we were to run the functionality covered by Test T1 followed by that of Test T2 on the same database record, what would be the resulting state?

Run completely locally with `docker` or `podman`, or try out the public demo at [pythia.jonjauhari.com](https://pythia.jonjauhari.com) (I don't always keep it running).

## Features

- Observe how individual records evolve across test runs
- Web-based UI for exploring changes interactively with HTMX and TailwindCSS
- Anonymous sessions with cookies, 
- Continuous deployment workflow to AWS (Fargate) with Terraform

## Usage

1. Start a session:

<img src="doc/sessions.png" width="500" />

2. Add record types (either via web UI or the REST API):

<img src="doc/record-types.png" width="500" />

3. Add facts for the records (either via the web UI or the REST API):

<img src="doc/facts.png" width="500" />

4. Calculate state change paths!

<img src="doc/state-change-1.png" width="500" />
<img src="doc/state-change-2.png" width="500" />

There's a few things we can loosely infer:
- `Test_FailOrder` tests 2 unique 'order' records in total.
- Both tests cover the mutation of `Status` from 'ordered' to 'dispatched'.
- Starting from the `Status` of 'ordered' we can reach the `Status` of 'cancelled' or 'delivered'.
- when the `Status` changes to 'dispatched', the `DispatchDate` is set to some date, likewise for 'delivered' and `DeliveryDate`.
- It takes 2 steps to get from `Status` of 'ordered' to 'delivered'.
- We could probably cancel 'Ord2' before it goes to the 'delivered' `Status`. 

## How to run

### Run locally with Docker/Podman

```bash
docker compose up
```

Pythia will be listening on port 3000

### Run locally without Docker/Podman

explain environment variables

### Run on AWS

Ensure you have authenticated with the AWS CLI

- Terraform
- AWS CLI

## License

Pythia is currently licensed under the terms of both the MIT license and the Apache License (Version 2.0). See [`LICENSE-MIT`](/LICENSE-MIT) and [`LICENSE-APACHE`](/LICENSE-APACHE) for more details.

