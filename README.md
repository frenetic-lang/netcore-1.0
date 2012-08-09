NetCore
========

NetCore is a high-level network programming language. This package provides
a NetCore compiler and runtime system for OpenFlow networks.

System Requirements
-------------------

- Haskell Platform 2012.2.0.0 (uses GHC 7.4.1)

    http://hackage.haskell.org/platform//

- A real OpenFlow network, or the Mininet network simulator:

    http://yuba.stanford.edu/foswiki/bin/view/OpenFlow/Mininet

Installation
------------

Install Frenetic from Hackage:

  cabal update
  cabal install frenetic

This installs a Haskell library that you can use to write Frenetic applications.

Demo Applications
------------------

We have several demos that use Frenetic and scripts that run them on Mininet
networks:

  git clone https://github.com/frenetic-lang/netcore.git
  cd netcore/examples
  cabal configure
  cabal build
  sudo ./frenetic-demo -h # Mininet requires sudo

Slice Verification
------------------

We use translation validation to verify that Frenetic slices are correctly
compiled. Verification requires the Microsoft Z3 SMT Solver:

  http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html

Our scripts expect the z3 executable to be in the PATH.
