#!/bin/sh
# Do 100 replicates on compilation since it's quick
echo "waxman multicast compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --waxman --multicast; done
echo "waxman shortest compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --waxman --shortest; done
echo "smallworld multicast compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --smallworld --multicast; done
echo "smallworld shortest compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --smallworld --shortest; done
echo "fattree multicast compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --fattree --multicast; done
echo "fattree shortest compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --fattree --shortest; done
echo "waxman multicast edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --waxman --multicast; done
echo "waxman shortest edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --waxman --shortest; done
echo "smallworld multicast edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --smallworld --multicast; done
echo "smallworld shortest edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --smallworld --shortest; done
echo "fattree multicast edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --fattree --multicast; done
echo "fattree shortest edge compilation"
for i in $(seq 1 100); do ./dist/build/frenetic-benchmark/frenetic-benchmark --time --edge --fattree --shortest; done
# Only do 10 replicates on verification because they're slow
echo "waxman multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --waxman --multicast; done
echo "waxman shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --waxman --shortest; done
echo "smallworld multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --smallworld --multicast; done
echo "smallworld shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --smallworld --shortest; done
echo "fattree multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --fattree --multicast; done
echo "fattree shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --fattree --shortest; done
echo "waxman multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --waxman --multicast; done
echo "waxman shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --waxman --shortest; done
echo "smallworld multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --smallworld --multicast; done
echo "smallworld shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --smallworld --shortest; done
echo "fattree multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --fattree --multicast; done
echo "fattree shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --isolation --compilation --edge --fattree --shortest; done
