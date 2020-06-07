#!/usr/bin/env bash

# Generate the docs individually using pandoc, keeping the headers, then stitch them together
# This is less than ideal, since I couldn't find a great cross platform pdf library

# THUS:
# this script MUST be run on MacOS, on Linux you could do:
# expression is:
# pandoc -s -o WriteYouASchemeVersion2.pdf $(ls ../docs | sort -n)

mkdir -p /tmp/scheme-pdf
rm /tmp/scheme-pdf/*.pdf

for markdown in $(ls ../docs | sort -n)
do
  echo $markdown
  pandoc --pdf-engine=xelatex -V linkcolor:blue --metadata link-citations  -o /tmp/scheme-pdf/"${markdown}_tail.pdf" ../docs/$markdown
done

"/System/Library/Automator/Combine PDF Pages.action/Contents/Resources/join.py" -o WriteYouASchemeVersion2.pdf $(ls /tmp/scheme-pdf/*.pdf)
