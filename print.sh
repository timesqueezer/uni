#!/bin/bash
what=$1
where=$2

cat $1 | ssh 6radloff@rzssh1.informatik.uni-hamburg.de lpr -P$2 -o sides=one-sided $3
