#!/bin/bash

for x in "001" "002" "003" "004" "005" "006" "007" "008" "009" "010"; do echo $x >> counts; echo 'bacterial' >> counts; cat $x\_seq_counts | grep -v 'GRCh38' | cut -f 2 | awk '{ sum+=$1} END {print sum}' >> counts; echo 'human' >> counts; cat $x\_seq_counts | grep 'GRCh38' | cut -f 2 | awk '{ sum+=$1} END {print sum}' >> counts; done
