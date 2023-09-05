#!/bin/bash

FRUITPYTESTS=$(make -pn | grep 'FRUITPYTESTS :=' | cut -d' ' -f 3-)

for FRUITPYTEST in $FRUITPYTESTS; do
    python3 ${FRUITPYTEST}
done
