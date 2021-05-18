# signaling2020mturk

This repository contains all code necessary to reproduce:

[*Depression and suicidality as credible signals of need*](https://michaelrgaffney.github.io/signaling2020mturk/). Michael R. Gaffney, Kai Adams, Kristen L. Syme, and Edward H. Hagen

Instructions:

1. Clone this repository
2. Open the project in RStudio or `cd` into the directory and launch `R`. This will automatically bootstrap [`renv`](https://rstudio.github.io/renv/index.html).
3. After the bootstrapping process, enter the following in the console: `renv::restore()`. This should install all the necessary packages, including the separate data package [`signaling2020data`](https://github.com/michaelrgaffney/signaling2020data), in an isolated project-specific library.
4. knit the `Paper.rmd` file using the RStudio GUI or with `rmarkdown::render('Paper.rmd')`. This will generate the preprint file [`Paper.html`](https://michaelrgaffney.github.io/signaling2020mturk/), which will display in the RStudio Viewer or can be viewed in any web browser. (Note: if not using RStudio, you will need a recent version of [pandoc](https://pandoc.org) installed.)

Note: Analyses used "R version 4.0.4 (2021-02-15)". You might need to install this version of R to reproduce them.
