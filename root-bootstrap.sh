#!/bin/bash
set -x

apt-get update
apt-get upgrade -yq
apt-get install -y haskell-platform git
