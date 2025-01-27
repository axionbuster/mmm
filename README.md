# mmm

Haddock link: [pg]

[![CI](https://github.com/axionbuster/mmm/actions/workflows/ci.yml/badge.svg)](https://github.com/axionbuster/mmm/actions/workflows/ci.yml)
[![Haddock](https://img.shields.io/badge/Haddock-Documentation-blue)](https://axionbuster.github.io/mmm/)

## Very short summary

Ultimate goal:

- Leverage STM and linear typing to __fundamentally shut out item / block duplication bugs.__
- Use STM observer pattern so that __users can directly observe changes in the environment__ rather than relying on a predefined set of events.
- Have a standalone working server as well as let library be used as a framework. Support both __subtractive and additive modes of server-building__.
- Fundamentally concurrent; have global clock for game logic, but __do not rely on a central event loop.__
- Make it __very easy to program a server using a proper effect system.__

1. Being ported from an old, working project with vast code improvements.
2. Networking code nearing first finished draft.
3. Has "working" collision detection and resolution system.
4. Immediate goal: let players join, get tab list working, hit each other.
5. Then: authenticate with Microsoft.
6. Will post updates soon.

X/Twitter: @axionbuster

## How to build (common)

On all operating systems, you need the development version of OpenSSL installed. There may be more dependencies.

Use the system-specific instructions below to install all needed dependencies.

Once all the necessary dependencies have been installed, run:

```
stack build
```

## How to build (Windows)

Since this project uses Stack, and Stack comes with its own MSYS2 environment, you should install it like this:

```sh
# first, update the package index
# (do this many times until it stops updating)
stack exec -- pacman -Syu

# now, install OpenSSL
stack exec -- pacman -S mingw-w64-x86_64-openssl
```

## How to build (Ubuntu or Fedora)

On Ubuntu or Fedora, OpenSSL is found in `libssl-dev` and `openssl-devel`, respectively. Download these packages, first.

On Ubuntu, you need to install `libgmp-dev`. On Fedora, install `gmp-devel`.

You also need `zlib`: `zlib1g-dev` on Ubuntu, `zlib-devel` on Fedora.

You also need a working C compiler and `pkg-config`. `pkg-config` is found in the Ubuntu package `pkgconf` and the Fedora package `pkgconf-pkg-config`.

## Immediate help

Well project is kind of less than well-documented and there's no working entry point but here's something I might need help with:

- Review the C OpenSSL adaptor for problems.
- Review the AI-generated packet formats for errors / omissions.

## What I'm doing now

- Parse "paletted container" format for chunks.
- Add a packet state registry (packet type <-> (who's talking * numeric packet code * protocol state))
  that is decoupled from the actual packet types so that I can reuse the same packet types if there's not been significant changes between versions.
- Architect a way to make the observer pattern easy to understand / program in.
  Target demographic for programmers is teenagers and young adults with minimal (initial) knowledge of Haskell or FP in general.

## Acknowledgements

Algorithms for collision detection and resolution contains portions and adaptations from the [Minestom project](https://github.com/Minestom/Minestom).

[pg]: https://axionbuster.github.io/mmm
