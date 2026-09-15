# Installation

This guide will walk you through the installation process for Henceforth.

## Prerequisites

To install Henceforth, you need Rust 1.85.0 or later and Cargo. The easiest way to get both is via [`rustup`](https://rustup.rs).

## Installing Henceforth

Currently, the quickest way to install Henceforth is to build the binary directly from source.
Run the following commands:
```
$ git clone https://github.com/riogu/henceforth.git
$ cd henceforth
$ cargo install --path .
```
This will install the `henceforth` binary directly to your path.

To verify the installation, run:
```
$ henceforth --version
henceforth 1.0.0
```

In the future, the package will be available on `crates.io` and other package managers.

## Updating Henceforth

To update, just run `git pull` on the repository and run `cargo install --force --path .`.

## Uninstalling Henceforth

To uninstall Henceforth, just run `cargo uninstall henceforth`.

## Editor/tooling integration

As of now, LSP and syntax highlighting is unavailable.
