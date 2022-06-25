#!/bin/bash

# delete the existing dot file
rm pometo.dot

echo "digraph Pometo {" > pometo.dot
# get the lines that represent declarations with grep
# delete everything after the colon
# replace double spaces with spaces
# remove the spaces around '->'
# strip off the trailing space
# replace the remaining spaces with '->'
# eliminate lines that that with the comment character %
# write to the file
grep '\->' src/pometo_parser.yrl | sed 's/[:].*//' | sed 's/  */ /g' | sed 's/ -> /->/g' | sed 's/ $//' | sed 's/ /->/g' | sed 's/^%.*//' >> pometo.dot
echo "}" >> pometo.dot
