This repository contains all files related to Kuiper, Volker & Laane
(2022), AIC-type model selection in small samples. Below, you find a
description of all files, the steps you have to take to reproduce all
results, and the used packages included their versions.

# Files

<table>
<colgroup>
<col style="width: 28%" />
<col style="width: 71%" />
</colgroup>
<thead>
<tr class="header">
<th>Folder / File</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>functions.R</code></td>
<td>File containing R functions to run the simulations in R</td>
</tr>
<tr class="even">
<td><code>aic(c)_simulations.R</code></td>
<td>File containing the code to run the AIC / AICc simulations</td>
</tr>
<tr class="odd">
<td><code>goric(c)_simulations.R</code></td>
<td>File containing additional code to run GORIC / GORICc simulations in
R (part of this is included in the appendix of the paper)</td>
</tr>
<tr class="even">
<td><code>create_figures.R</code></td>
<td>Code to reproduce the figures shown in the paper (plus additional
figures, not shown in the paper), as well as some of the output reported
in the text</td>
</tr>
<tr class="odd">
<td>figures</td>
<td>Folder containing all output figures used in the paper (and
additional figures for papers not reported in the paper;
<code>aicc_out1b1_thr.pdf</code>, <code>aicc_out1b3_thr.pdf</code>,
<code>aicc_out2a_thr_weight.pdf</code>,
<code>aicc_out2b_thr_weight.pdf</code>,
<code>aicc_out2c_thr_weight.pdf</code>,
<code>goriccRatio1234Set2_thr_weight.pdf</code>,
<code>goriccRatio1234Set4_thr_weight.pdf</code>).</td>
</tr>
<tr class="even">
<td>results</td>
<td>Output of the simulations, empty because the files are too large to
store online.</td>
</tr>
</tbody>
</table>

To recreate all results and the output of the simulations, you can
sequentially run the files `functions.R`, `aic(c)_simulations.R`,
`goric(c)_simulations.R` and `create_figures.R`.

# Packages

Below, all packages used in the making of this paper are listed
(including version number), regardless of whether they were loaded
directly, or imported by a different packages. The required packages
that were loaded directly can be found in the files
`aic(c)_simulations.R` or `goric(c)_simulations.R`.

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.2.1 (2022-06-23 ucrt)
    ##  os       Windows 10 x64 (build 19042)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  Dutch_Netherlands.utf8
    ##  ctype    Dutch_Netherlands.utf8
    ##  tz       Europe/Berlin
    ##  date     2022-09-07
    ##  pandoc   2.18 @ C:/Program Files/RStudio/bin/quarto/bin/tools/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date (UTC) lib source
    ##  cli           3.3.0   2022-04-25 [1] CRAN (R 4.2.1)
    ##  digest        0.6.29  2021-12-01 [1] CRAN (R 4.2.1)
    ##  evaluate      0.15    2022-02-18 [1] CRAN (R 4.2.1)
    ##  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.2.1)
    ##  htmltools     0.5.2   2021-08-25 [1] CRAN (R 4.2.1)
    ##  knitr         1.39    2022-04-26 [1] CRAN (R 4.2.1)
    ##  magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.1)
    ##  rlang         1.0.4   2022-07-12 [1] CRAN (R 4.2.1)
    ##  rmarkdown     2.14    2022-04-25 [1] CRAN (R 4.2.1)
    ##  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.2.1)
    ##  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.1)
    ##  stringi       1.7.6   2021-11-29 [1] CRAN (R 4.2.0)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.2.1)
    ##  xfun          0.31    2022-05-10 [1] CRAN (R 4.2.1)
    ##  yaml          2.3.5   2022-02-21 [1] CRAN (R 4.2.0)
    ## 
    ##  [1] C:/Users/5868777/Documents/R/win-library/3.0
    ##  [2] C:/Program Files/R/R-4.2.1/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────
