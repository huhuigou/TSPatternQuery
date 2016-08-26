###Time Series Pattern Query Tool

This is an R package for defining patterns in time series and using these patterns to "query" time series data. You may use this package, however be aware that it is still in development. 

Click here to read the documentation: https://joshmarsh.github.io/TSPatternQuery/index.html

Run the following commands in the R console to install the package:
<pre><code>
install.packages("devtools") #You may skip this one if you already have devtools.  
devtools::install_github("joshmarsh/TSPatternQuery")  
library(TSPatternQuery)  
library(xts)
</code></pre>


TODO:
*Allow Template Matching to be switched off, for ruleset-only matching.
* Address Feedback from Reddit:
  * The title for GetWindowVariancePDF is "Plot the PDF of Variances ..." but it doesn't do any plotting.
  * Query has a library(xts) call that isn't necessary (also flagged by check()
  * Dependencies in Imports should be Depends (this is a hairy distinction)
  * You could use some examples in your major user-facing functions
  * One of your tests fails because it relies on another package (TSTestDataUtil) which isn't defined as a dependency
  * Consider following the testthat templates described here: http://r-pkgs.had.co.nz/tests.html
  * paste0 is essentially a sep="" version of paste
  * GetLengthInSeconds seems oddly specific; do you always want seconds? You could make this more flexible by replacing it with
  * GetLength <- function(timeseries, units = "secs") { difftime(time(timeseries[length(timeseries)]), time(timeseries[1]), units = units) }
  * You've used is.pip as a variable, but it looks like a function (well, I looked for it). This is especially confusing to a dev reading your code as you've used things like
while (!is.pip[k]) { k <- k + 1 }
  * Consider turning on RStudio's syntax checker - you have a lot of missing whitespace (e.g. abc<-x+2) which is a) prone to breaking if accidentally changing to abc < -x + 2) and b) harder to read. If you can't fit the entire block of code onto a page, consider that your block might be too long.
  * Add links to the GitHub repo in DESCRIPTION, i.e.
URL: https://github.com/joshmarsh/TSPatternQuery
BugReports: https://github.com/joshmarsh/TSPatternQuery/issues
  * I see you're using TravisCI, which is great, but consider also using AppVeyor for Windows testing, and once your tests are in order, CodeCov for code test coverage.
  * Furthermore, devtools::check() will give you lots of useful information. It's a requirement that it produces no ERRORs or WARNINGs (idealy no NOTEs either) if you're going to submit to CRAN. At the moment it gives an 1 error | 3 warnings | 5 notes and points out the things you need to look at:
    * .travis.yml should be listed in .Rbuildignore
    * testdata shouldn't be a top level directory
    * Lots of 'global function definition' notes. This is very common, especially if using non-standard evaluation, but it gets flagged because of how R does scoping. e.g. GetLengthInSeconds refers to the function time without defining which namespace it's from. Either use stats::time or add @importFrom stats time in the roxygen header
    * Ditto for approxfun, cor.test, density, and var - I know, we use these all the time from auto-loaded packages, but it's best to be precise
* Add limitations section to documentation
* Move CrateRandTimeSeries to this package. Properly debug and write tests for it.
