---
title: "SpaDES 4 Dummies guide"
author: "Ceres Barros"
date: "2022-03-17"
site: bookdown::bookdown_site
knit: "bookdown::render_book"
bibliography:
  - citations/references.bib
csl: citations/ecology-letters.csl
link-citations: true
documentclass: book
github-repo: CeresBarros/SpaDES4Dummies
url: 'https\://ceresbarros.github.io/SpaDES4Dummies/'
description: "A `SpaDES` crash-course"
---



# Preface {-}

This guide will take you through how to make and link your own modules using `SpaDES` in two examples. Both examples draw on basic uses of statistical models in ecology, notably the relationships between enviromental variables and species abundance and presence. 

Part \@ref(part1) is very minimal, and uses only dummy data. It is meant to introduce you to the different components of a `SpaDES` module.
Part \@ref(part2) uses real and freely available data, and provides a deeper look into several useful aspects of `SpaDES`, notably caching and spatial data processing.

To install `SpaDES`, please have a look at [SpaDES installation](https://github.com/PredictiveEcology/SpaDES.install/tree/installFromSource#readme).

