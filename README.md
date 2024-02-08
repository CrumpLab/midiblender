
<!-- README.md is generated from README.Rmd. Please edit that file -->

# midiblender <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

![](https://img.shields.io/badge/WARNING-Half--Baked-red.svg)
![](https://img.shields.io/badge/Will%20it%20Blend-Question%20Mark-red.svg)
![](https://img.shields.io/badge/Dependable-No-blue.svg)
![](https://img.shields.io/badge/Hobby%20side%20project-Yes-green.svg)

<!-- badges: end -->

The goal of `midiblender` is to mangle midi files in R and listen to
what happens.

This is an experimental, use at your own frustration package. I’m
writing this for my own use cases, and using the R package format
because it helps me to clarify and track the goals I’m chasing down.
This is an R package, but perhaps should not be confused with one.

I’m sharing this for fun and in case others find it useful.

## Installation

You can install the midiblender like so:

``` r
## install remotes package if it's not already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install dev version of rtweettree from github
remotes::install_github("CrumpLab/midiblender")
```

Aspects of this package rely on
[pyramidi](https://urswilke.github.io/pyramidi/), which is also in an
experimental lifecycle. That package wraps some python libraries
([miditapyr](https://pypi.org/project/miditapyr/) and
[mido](https://mido.readthedocs.io/en/stable/)) that handle midi import
and export. See the [pyramidi](https://urswilke.github.io/pyramidi/)
documentation for more information about what is necessary to install
before this will work.

## To Do

- [x] a conceptual [getting started
  document](https://www.crumplab.com/midiblender/articles/Getting_started.html)
- \[\] Attempting to minimally document functions that are added
- \[\] Using vignettes to conduct various tests and concepts for midi
  blending
- \[\] Fill out example code in the function documentation

The primary style of development here is me trying things out for fun
and listening to them…with a side eye toward posterity in case the
mangling pattern seems like a tool I’d want to use again.

## Thanks to

Thanks to the R community for building such wonderful tools. Special
thanks to Urs Wilke for
[pyramidi](https://urswilke.github.io/pyramidi/), that helped me get up
to speed very quickly.

<!--
&#10;
-->

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-quarto" class="csl-entry">

Allaire, JJ. 2023. *<span class="nocase">quarto</span>: R Interface to
“Quarto” Markdown Publishing System*.
<https://CRAN.R-project.org/package=quarto>.

</div>

<div id="ref-rmarkdown2023" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2023.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-midiblender" class="csl-entry">

Crump, Matthew J. C. 2024. “<span class="nocase">midiblender</span>:
Experiments in genRative MIDI Mangling.”

</div>

<div id="ref-remotes" class="csl-entry">

Csárdi, Gábor, Jim Hester, Hadley Wickham, Winston Chang, Martin Morgan,
and Dan Tenenbaum. 2023. *<span class="nocase">remotes</span>: R Package
Installation from Remote Repositories, Including “GitHub”*.
<https://CRAN.R-project.org/package=remotes>.

</div>

<div id="ref-tuneR" class="csl-entry">

Ligges, Uwe, Sebastian Krey, Olaf Mersmann, and Sarah Schnackenberg.
2023. *<span class="nocase">tuneR</span>: Analysis of Music and Speech*.
<https://CRAN.R-project.org/package=tuneR>.

</div>

<div id="ref-av" class="csl-entry">

Ooms, Jeroen. 2023. *<span class="nocase">av</span>: Working with Audio
and Video in r*. <https://CRAN.R-project.org/package=av>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2023. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-usethis" class="csl-entry">

Wickham, Hadley, Jennifer Bryan, Malcolm Barrett, and Andy Teucher.
2023. *<span class="nocase">usethis</span>: Automate Package and Project
Setup*. <https://CRAN.R-project.org/package=usethis>.

</div>

<div id="ref-roxygen2" class="csl-entry">

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster.
2024. *Roxygen2: In-Line Documentation for r*.
<https://CRAN.R-project.org/package=roxygen2>.

</div>

<div id="ref-pkgdown" class="csl-entry">

Wickham, Hadley, Jay Hesselberth, and Maëlle Salmon. 2022.
*<span class="nocase">pkgdown</span>: Make Static HTML Documentation for
a Package*. <https://CRAN.R-project.org/package=pkgdown>.

</div>

<div id="ref-devtools" class="csl-entry">

Wickham, Hadley, Jim Hester, Winston Chang, and Jennifer Bryan. 2022.
*<span class="nocase">devtools</span>: Tools to Make Developing r
Packages Easier*. <https://CRAN.R-project.org/package=devtools>.

</div>

<div id="ref-lsa" class="csl-entry">

Wild, Fridolin. 2022. *<span class="nocase">lsa</span>: Latent Semantic
Analysis*. <https://CRAN.R-project.org/package=lsa>.

</div>

<div id="ref-pyramidi" class="csl-entry">

Wilke, Urs. 2024. *<span class="nocase">pyramidi</span>: Generate and
Manipulate Midi Data in r Data Frames*.
<https://github.com/urswilke/pyramidi>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2023" class="csl-entry">

———. 2023. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in r*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

<div id="ref-badger" class="csl-entry">

Yu, Guangchuang. 2023. *<span class="nocase">badger</span>: Badge for r
Package*. <https://CRAN.R-project.org/package=badger>.

</div>

</div>
