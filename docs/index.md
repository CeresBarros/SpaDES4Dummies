---
title: "SpaDES 4 Dummies guide"
author: "Ceres Barros"
date: "2023-08-16"
description: "A `SpaDES` crash-course"
site: bookdown::bookdown_site
knit: "bookdown::render_book"
bibliography:
  - citations/references.bib
  - citations/packages.bib
csl: citations/ecology-letters.csl
biblatexoptions: [citestyle=authoryear]
link-citations: true
documentclass: latex/krantz
github-repo: CeresBarros/SpaDES4Dummies
url: 'https\://ceresbarros.github.io/SpaDES4Dummies/'
always_allow_html: true
colorlinks: yes
graphics: yes
fontsize: 11pt
lot: yes
lof: yes
---





# Preface {-}

[![zenodoBadge](D:\GitHub\SpaDES4Dummies\figures\zenodoBadge.png)](https://zenodo.org/badge/latestdoi/112410440)


This guide will take you through how to make and link your own modules using `SpaDES` in two examples. Both examples draw on basic uses of statistical models in ecology, notably the relationships between environmental variables and species abundance and presence. 

Part \@ref(part1) is very minimal, and uses only dummy data. It is meant to introduce you to the different components of a `SpaDES` module.

Part \@ref(part2) uses real and freely available data, and provides a deeper look into several useful aspects of `SpaDES`, notably caching and spatial data processing.

To install `SpaDES`, please have a look at [SpaDES installation](https://github.com/PredictiveEcology/SpaDES/wiki/Installation), or follow steps at the start of Part \@ref(part2).
