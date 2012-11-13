#!/bin/sh
echo "waxman multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --waxman --multicast; done
echo "waxman shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --waxman --shortest; done
echo "smallworld multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --smallworld --multicast; done
echo "smallworld shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --smallworld --shortest; done
echo "fattree multicast verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --fattree --multicast; done
echo "fattree shortest verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --fattree --shortest; done
echo "waxman multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --waxman --multicast; done
echo "waxman shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --waxman --shortest; done
echo "smallworld multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --smallworld --multicast; done
echo "smallworld shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --smallworld --shortest; done
echo "fattree multicast edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --fattree --multicast; done
echo "fattree shortest edge verification"
for i in $(seq 1 10); do ./dist/build/frenetic-benchmark/frenetic-benchmark --vlan --ast --isolation --compilation --edge --fattree --shortest; done
