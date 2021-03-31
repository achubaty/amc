# `amc`: Alex's miscellaneous code

<!-- badges: start -->
[![R build status](https://github.com/achubaty/amc/workflows/R-CMD-check/badge.svg)](https://github.com/achubaty/amc/actions)
[![Codecov test coverage](https://codecov.io/gh/achubaty/amc/branch/master/graph/badge.svg)](https://codecov.io/gh/achubaty/amc?branch=master)
<!-- badges: end -->

## Variously useful functions and utilities

### Installation

#### From GitHub

1. **Install development libraries:** building packages from source requires the appropriate development libraries for your operating system.
  See [here](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites) for more details.
    
    - *Windows:* install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
    - *macOS:* install Xcode commandline tools from the terminal: `xcode-select install`. 
    - *Debian/Ubuntu Linux:* ensure `r-base-dev` is installed.

2. **Install from GitHub:**
    
    ```r
    #install.packages("devtools")
    library("devtools")
    install_github("achubaty/amc")
    ```

### Reporting bugs

Contact us via the package GitHub site: [https://github.com/achubaty/amc/issues](https://github.com/achubaty/amc/issues).

### Contributions

This Git repository uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).
The [`development`](https://github.com/achubaty/amc/tree/development) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/achubaty/amc) branch contains the code of the latest release.

To make a contribution to the package, just send a [pull request](https://help.github.com/articles/using-pull-requests/). 
When you send your PR, make sure `development` is the destination branch on the [amc repository](https://github.com/achubaty/amc).
Your PR should pass `R CMD check --as-cran`, which will also be checked by when the PR is submitted.
