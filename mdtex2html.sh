#!/bin/sh
pandoc $1 -t html -s -o "${1%.*}".html --mathjax=https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML
