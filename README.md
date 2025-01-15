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

[pg]: https://axionbuster.github.io/mmm
