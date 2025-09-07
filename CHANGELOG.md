# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2025-09-07

### Added

- Web interface style overhaul
- Add 'how to use' page
- Enable adding/deleting record types via web interface
- Enable anonymous cookie-based user sessions
- Add 'copy to clipboard' features for facts table for convenience
- Metadata fields now constrained to just 'Context' and 'SeqNum'
- Facts added by API are now persisted in a prerequisite locally-running DynamoDB instance

## [0.1.0-dev.3] - 2025-07-11

### Added

- New web interface styling
- Facts added by API are now persisted in the corresponding Prolog files

## [0.1.0-dev.2] - 2025-06-08

### Fixed

- Prevent app crashing when there is an empty string in Fact value

## [0.1.0-dev] - 2025-05-20

### Added

- Basic state change path calculation
