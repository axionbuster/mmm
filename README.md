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

## Immediate help

Well project is kind of less than well-documented and there's no working entry point but here's something I might need help with:

- Review the C OpenSSL adaptor for problems.
- Review the AI-generated packet formats for errors / omissions.

## What I'm doing now

- Parse "paletted container" format for chunks.
- Add a packet state registry (packet type <-> (numeric packet code * protocol state))
  that is decoupled from the actual packet types so that I can reuse the same packet types if there's not been significant changes between versions.
- Architect a way to make the observer pattern easy to understand / program in.
  Target demographic for programmers is teenagers and young adults with minimal (initial) knowledge of Haskell or FP in general.

[pg]: https://axionbuster.github.io/mmm
