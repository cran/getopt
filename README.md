# getopt

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/getopt)](https://cran.r-project.org/package=getopt)

[![R-CMD-check](https://github.com/trevorld/r-getopt/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/r-getopt/actions)

[![Coverage Status](https://codecov.io/github/trevorld/r-getopt/branch/master/graph/badge.svg)](https://app.codecov.io/github/trevorld/r-getopt?branch=master)

[![RStudio CRAN mirror downloads](https://cranlogs.r-pkg.org/badges/getopt)](https://cran.r-project.org/package=getopt)

[![Number of dependencies](https://tinyverse.netlify.com/badge/getopt)](https://cran.r-project.org/package=getopt)

<img src="man/figures/logo.png" align="right" width="200px" alt="optparse hex sticker">

`getopt` is an R package designed to be used with `Rscript` to write
"#!"-shebang scripts that accept short and long flags/options. Many
users will prefer using instead the package
[optparse](https://github.com/trevorld/r-optparse) which adds extra
features (automatically generated help option and usage, support for
default values, basic positional argument support).

To install the last version released on CRAN use the following command:

    install.packages("getopt")

To install the development version use the following command:

    install.packages("remotes")
    remotes:install_github("trevorld/r-getopt")

## example

An example Rscript using `getopt`:

    #!/path/to/Rscript
    library('getopt')
    # get options, using the spec as defined by the enclosed list.
    # we read the options from the default: commandArgs(TRUE).
    spec = matrix(c(
      'verbose', 'v', 2, "integer",
      'help'   , 'h', 0, "logical",
      'count'  , 'c', 1, "integer",
      'mean'   , 'm', 1, "double",
      'sd'     , 's', 1, "double"
    ), byrow=TRUE, ncol=4)
    opt = getopt(spec)

    # if help was asked for print a friendly message 
    # and exit with a non-zero error code
    if ( !is.null(opt$help) ) {
      cat(getopt(spec, usage=TRUE))
      q(status=1)
    }

    # set some reasonable defaults for the options that are needed,
    # but were not specified.
    if ( is.null(opt$mean    ) ) { opt$mean    = 0     }
    if ( is.null(opt$sd      ) ) { opt$sd      = 1     }
    if ( is.null(opt$count   ) ) { opt$count   = 10    }
    if ( is.null(opt$verbose ) ) { opt$verbose = FALSE }

    # print some progress messages to stderr, if requested.
    if ( opt$verbose ) { write("writing...",stderr()) }

    # do some operation based on user input.
    cat(rnorm(opt$count,mean=opt$mean,sd=opt$sd),sep="\n")

    # signal success and exit.
    # q(status=0)
