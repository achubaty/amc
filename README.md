# `amc`: Alex's miscellaneous code

[![Build status](https://ci.appveyor.com/api/projects/status/y541xpu8dr5icy41/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/amc/branch/master)
[![Build Status](https://travis-ci.org/achubaty/amc.svg?branch=master)](https://travis-ci.org/achubaty/amc) 
[![Coverage Status](https://coveralls.io/repos/github/achubaty/amc/badge.svg?branch=master)](https://coveralls.io/github/achubaty/amc?branch=master)

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
Your PR should pass `R CMD check --as-cran`, which will also be checked by <a href="https://travis-ci.org/achubaty/amc">Travis CI</a> and <a href="https://ci.appveyor.com/project/achubaty/amc">AppVeyor CI</a> when the PR is submitted.
