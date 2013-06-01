NetCore 1.0
===========

This is the old NetCore controller. We recommend using the new Frenetic controller:

  http://www.github.com/frenetic-lang/frenetic

Frenetic implements the main ideas of NetCore and has several more features and enhancements.

--------------

NetCore is a high-level network programming language. This package provides
a NetCore compiler and runtime system for OpenFlow networks.

Requirements
------------

- Haskell Platform 2012.2.0.0 (uses GHC 7.4.1)

    http://hackage.haskell.org/platform/

  This is necessary to build NetCore and the example programs.

- A real OpenFlow network, or the Mininet network simulator:

    http://yuba.stanford.edu/foswiki/bin/view/OpenFlow/Mininet

  NetCore programs run on OpenFlow networks. We've tested NetCore using the
  OpenFlow reference switch (user-mode) and a Pronto 3290.

NetCore 1.0 (stable release)
----------------------------

### Installation

NetCore is available on Hackage:

    cabal update
    cabal install netcore

This installs a Haskell library that you can use to write Frenetic applications.

### Demos

We have several demos that use Frenetic and scripts that run them on Mininet
networks. Download and expand the following tarball:

    https://github.com/downloads/frenetic-lang/netcore/netcore-1.0.0-examples.tar.gz

To build:

    cd examples
    cabal configure
    cabal build
    sudo ./demo.py <your username> # Mininet requires sudo

NetCore development version
---------------------------

### Installation

NetCore is open-source, with code available on Github:

    git clone git://github.com/frenetic-lang/netcore.git
    cd netcore
    git submodule init
    git submodule update
    make

### Demos

Within the NetCore source directory (netcore in the script above):

    cd examples
    cabal configure
    cabal build
    sudo ./demo.py <your username> # Mininet requires sudo

### Slice Verification

We use translation validation to verify that Frenetic slices are correctly
compiled. Verification requires the Microsoft Z3 SMT Solver:

  http://research.microsoft.com/en-us/um/redmond/projects/z3/download.html

Our scripts expect the z3 executable to be in the PATH.

