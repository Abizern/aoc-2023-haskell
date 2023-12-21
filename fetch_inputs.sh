#!/usr/bin/env sh

# Fetches all the inputs from 20151 and puts them in the relevant directory with the correct name
for day in $(seq -w 1 21)
do
    aocd "2023" "$day" >| inputs/day"$day".txt
    echo "Fetched input for $year $day"
done

