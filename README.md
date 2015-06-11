csh-eval [![Circle CI](https://circleci.com/gh/ComputerScienceHouse/csh-eval.svg?style=svg)](https://circleci.com/gh/ComputerScienceHouse/csh-eval)
=========
An evaluations system for the Computer Science House at RIT.

## Project Goals
- Maintainablility: Should be easy to refactor with confidence.
- Extensibility: Should be easy to extend or modify existing features and
                 integrate with other services.
- Usability: Site should have intelligent design decisions with real use cases
             in mind.

## Setup for development

__Note:__ Requires ghc version 7.8 or above

- Install PostgreSQL (we have been using the latest stable: 9.4)
- Initialize your cabal sandbox: `cabal sandbox init`
- Install the dependencies (with tests) `cabal install --enable-tests --only-dependencies --reorder-goals`
- Initialize the database:
	- Create a user called "pvals": `createuser pvals`
	- Create a database called "pvals" owned by the pvals user: `createdb pvals -O pvals`
	- Run the following command: `cabal exec runhaskell db/DBInitMain.hs db/eval_schema.sql`
- `cabal build`
- `cabal test`

## Contributing

### Discussion
General coordination and discussion takes place in #pvals on cshrtp.slack.com.
An `@csh.rit.edu` address is required to chat. Longer form discussion for specific
features will occur under issues tagged `discussion` on the ComputerScienceHouse Github.
Bugs and feature requests may also be made through Github issues.

### Review Process
Github pull requests are used as a code review mechanism. All commits must be
sent as pull requests, typically from topic branches in contributors' forks.
Pull requests may not be merged into this repository's master branch by the requestor,
this ensures that at least one other contributor with push access has
reviewed the code. Pull requests are built and tested by circleci. Pull requests may not
be merged until the circle build status is known.

### Testing and Documentation
All functions must have at least one doctest example in their docstring.
All pure Haskell functions must be accompanied by QuickCheck props. All impure
functions must be accompanied by HUnit tests. All pure SQL must be accompanied
by offerings to the deity of the contributor's choosing, proportional in
extravagance to the complexity of the SQL's behavior.

### Versioning
No guarantees are made with regards to the stability (or sanity) of this
repository's master branch. Please checkout the latest tagged commit for
deployment purposes.
