# mrgvalprep
R package containing helpers for formatting and preprocessing inputs to [mrgvalidate](https://github.com/metrumresearchgroup/mrgvalidate).

## Inputs
Supports the following data sources:

### Test Output

* `testthat::ListReporter`

### Stories and Requirements

* Googlesheets
* Github issues and milestones

## More to come...

* Intending to have connectors to Go test outputs, and others
* Probably some connectors to Jira and/or Confluence for pulling in Stories and Requirements

## Development

`mrgvalprep` uses [pkgr](https://github.com/metrumresearchgroup/pkgr) to manage 
development dependencies and [renv](https://rstudio.github.io/renv/) to provide 
isolation. To replicate this environment, 

1. clone the repo

2. install pkgr

3. open package in an R session and run `renv::init(bare = TRUE)` 
   - install `renv` > 0.8.3-4 into default `.libPaths()` if not already installed

3. run `pkgr install` in terminal within package directory

4. restart session

Then, launch R with the repo as the working directory (open the project in 
RStudio). renv will activate and find the project library.
