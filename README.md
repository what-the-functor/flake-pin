# flake-pin

A CLI tool for automatically pinning/updating Nix flake inputs to their latest GitHub commits.

> 🚧 **Status: In Development** 
>
> This tool is currently under development and not yet functional.

## What it does

`flake-pin` takes a flake input name and updates it to the latest commit from GitHub:

- Reads your `flake.lock` to find the current state of the input
- Fetches the latest commit hash from the GitHub API
- Updates your `flake.nix` to pin the input to the latest commit (first 7 characters)
- Adds or updates a comment with the commit date (UTC) in ISO-8601 format

Although packages can be pinned individually by using the flake lock API, explicit pinning in the flake file:
- pins are prominently visible near the top of the flake file, and
- enables use of `flake update` with granular control of what is pinned.

## Features

- 🛟 Type-safe parsing and modification of Nix files
- 🚀 Uses GitHub API for reliable commit information
- 📝 Automatic date comments

## Example

# Before (in flake.nix):
```nix
emacs-overlay.url = "github:nix-community/emacs-overlay";
```

# Update the "emacs-overlay" input to latest commit
```sh
flake-pin --input emacs-overlay /path/to/your/flake
```

# After:
```nix
emacs-overlay.url = "github:nix-community/emacs-overlay/8dc6642"; # 2025-06-18
```

## Installation

### With Nix (recommended)

Add to your flake inputs, or run directly.

# Run directly
```bash
nix run github:what-the-functor/flake-pin -- --input emacs-overlay /path/to/flake
```

### With Cabal

```bash
git clone https://github.com/what-the-functor/flake-pin
pushd flake-pin
cabal install
```

## Usage

```bash
flake-pin --input <input-name> <flake-path>
```

- `input-name`: The name of the flake input to update (e.g., "nixpkgs", "emacs-overlay")
- `flake-path`: Path to the directory containing the `flake.nix` file

## Development

### With Nix

```sh
git clone https://github.com/what-the-functor/flake-pin
pushd flake-pin
nix develop
cabal build
cabal test
```

### Without Nix

Requirements:
- GHC 9 (tested with 9.8.4)
- Cabal
- Fourmolu

```bash
cabal build
cabal test
```

## Contributing

This project uses TDD with property-based testing. Pull requests should include tests for new functionality.
Format with Fourmolu

## License

MIT
