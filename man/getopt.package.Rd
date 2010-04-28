\name{getopt.package}
\alias{getopt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ C-like getopt behavior }
\description{
getopt is primarily intended to be used with ``\link{Rscript}''.  It facilitates writing
``\#!'' shebang scripts that accept short and long flags/options.  It can also
be used from ``R'' directly, but is probably less useful in this context.

getopt() returns a \link{list} data structure containing \link{names} of the
flags that were present in the \link{character} \link{vector} passed in under
the \emph{opt} argument.  Each value of the \link{list} is coerced to the data
type specified according to the value of the \emph{spec} argument.  See below
for details.

Notes on naming convention:

1. An \emph{option} is one of the shell-split input strings.

2. A \emph{flag} is a type of \emph{option}.  a \emph{flag} can be defined
as having no \emph{argument} (defined below), a required \emph{argument}, or an
optional \emph{argument}.

3. An \emph{argument} is a type of \emph{option}, and is the value associated
with a flag.

4. A \emph{long flag} is a type of \emph{flag}, and begins with the string
``--''.  If the \emph{long flag} has an associated \emph{argument}, it may be
delimited from the \emph{long flag} by either a trailing \emph{=}, or may be
the subsequent \emph{option}.

5. A \emph{short flag} is a type of \emph{flag}, and begins with the string
``-''.  If a \emph{short flag} has an associated \emph{argument}, it is the
subsequent \emph{option}.  \emph{short flags} may be bundled together, sharing
a single leading ``-'', but only the final \emph{short flag} is able to have a
corresponding \emph{argument}.
}
\usage{getopt( spec=NULL, opt=commandArgs(TRUE), command=strsplit(commandArgs(FALSE)[4],"=")[[1]][2], usage=FALSE, debug=FALSE )}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
  The getopt specification, or spec of what options are considered valid.  The
  specification must be either a 4-5 column \link{matrix}, or a \link{character}
  \link{vector} coercible into a 4 column \link{matrix} using
  \link{matrix}(x,ncol=4,byrow=TRUE) command.  The \link{matrix}/\link{vector}
  contains:

    Column 1: the \emph{long flag} name.  A multi-\link{character} string.

    Column 2: \emph{short flag} alias of Column 1.  A single-\link{character}
    string.

    Column 3: \emph{Argument} mask of the \emph{flag}.  An \link{integer}.
    Possible values: 0=no argument, 1=required argument, 2=optional argument.

    Column 4: Data type to which the \emph{flag}'s argument shall be cast using
    \link{storage.mode}.  A multi-\link{character} string.  This only
    considered for same-row Column 3 values of {1,2}.  Possible values:
    \link{logical}, \link{integer}, \link{double}, \link{complex},
    \link{character}.

    Column 5 (optional): A brief description of the purpose of the option.

  The terms \emph{option}, \emph{flag}, \emph{long flag}, \emph{short flag},
  and \emph{argument} have very specific meanings in the context of this
  document.  Read the ``Description'' section for definitions.
  }
  \item{opt}{
  This defaults to the return value of \link{commandArgs}(TRUE).
  
  If R was invoked directly via the ``R'' command, this corresponds
  to all arguments passed to R after the ``--args'' flag.

  If R was invoked via the ``\link{Rscript}'' command, this corresponds to all
  arguments after the name of the R script file.

  Read about \link{commandArgs} and \link{Rscript} to learn more.
  }
  \item{command}{
  The string to use in the usage message as the name of the script.  See
  argument \emph{usage}.
  }
  \item{usage}{
  If TRUE, argument \emph{opt} will be ignored and a usage statement (character
  string) will be generated and returned from \emph{spec}.
  }
  \item{debug}{
  This is used internally to debug the getopt() function itself.
  }
}
\details{
Current issues:

1. No support for multiple, identical flags, e.g. for "-m 3 -v 5 -v", the
trailing "-v" overrides the preceding "-v 5", result is v=TRUE (or equivalent
typecast).

2. No support for multi-valued flags, e.g.
"--libpath=/usr/local/lib --libpath=/tmp/foo".

3. No support for lists, e.g. "--define os=linux --define os=redhat"
would set result$os$linux=TRUE and result$os$redhat=TRUE.

4. No support for incremental, argument-less flags, e.g. "/path/to/script -vvv"
should set v=3.

5. Need more unit tests (see end of examples section).

6. Support partial-but-unique string match on options, e.g. "--verb" and
"--verbose" both match long flag "--verbose".

7. No support for mixing in positional arguments or extra arguments that don't
match any options.  For example, you can't do "my.R --arg1 1 foo bar baz" and
recover "foo", "bar", "baz" as a list.  Likewise for "my.R foo --arg1 1 bar baz".

}
\value{ }
\references{ }
\author{ Allen Day }
\note{ }
\seealso{ \code{\link{getopt}} }
\examples{
#!/path/to/Rscript
library('getopt');
#get options, using the spec as defined by the enclosed list.
#we read the options from the default: commandArgs(TRUE).
opt = getopt(c(
  'verbose', 'v', 2, "integer",
  'help'   , 'h', 0, "logical",
  'count'  , 'c', 1, "integer",
  'mean'   , 'm', 1, "double",
  'sd'     , 's', 1, "double"
));

#help was asked for.
if ( !is.null(opt$help) ) {
  #get the script name (only works when invoked with Rscript).
  self = commandArgs()[1];
  #print a friendly message and exit with a non-zero error code
  cat(paste("Usage: ",self," [-[vh]] [-[-mean|m] <mean>] [-[-sd|s] <sd>] [-[-count|c] <count>]\n",sep=""));
  q(status=1);
}

#set some reasonable defaults for the options that are needed,
#but were not specified.
if ( is.null(opt$mean    ) ) { opt$mean    = 0     }
if ( is.null(opt$sd      ) ) { opt$sd      = 1     }
if ( is.null(opt$count   ) ) { opt$count   = 10    }
if ( is.null(opt$verbose ) ) { opt$verbose = FALSE }

#print some progress messages to stderr, if requested.
if ( opt$verbose ) { write("writing...",stderr()); }

#do some operation based on user input.
cat(paste(rnorm(opt$count,mean=opt$mean,sd=opt$sd),collapse="\n"));
cat("\n");

#signal success and exit.
#q(status=0);

### END ###
#regression tests follow.  not part of the example.
spec = c(
  'verbose', 'v', 2, "integer",
  'help'   , 'h', 0, "logical",
  'dummy1' , 'd', 0, "logical",
  'dummy2' , 'e', 2, "logical",
  'count'  , 'c', 1, "integer",
  'mean'   , 'm', 1, "double",
  'sd'     , 's', 1, "double",
  'output' , 'O', 1, "character"
);
opt = getopt(spec, c('-c', '-1', '-m', '-1.2'));
opt = getopt(spec, c('-v', '-m', '3'));
opt = getopt(spec, c('-m', '3', '-v'));
opt = getopt(spec, c('-m', '3', '-v', '2', '-v'));
opt = getopt(spec, c('-O', '-', '-m', '3'));
opt = getopt(spec, c('-m', '3', '-O', '-'));
opt = getopt(spec, c('-de'));
opt = getopt(spec, c('-ed'));
opt = getopt(spec, c('-d'));
opt = getopt(spec, c('-e', '1'));
opt = getopt(spec, c('-de', '1'));
opt = getopt(spec, c('--verbose'));
opt = getopt(spec, c('--help'));
opt = getopt(spec, c('--verbose', '--help'));
opt = getopt(spec, c('--verbose', '--mean', '5'));
opt = getopt(spec, c('--mean=5'));
opt = getopt(spec, c('--verbose', '--mean=5'));
opt = getopt(spec, c('--verbose', '--mean=5', '--sd', '5'));
opt = getopt(spec, c('--mean=5', '--sd', '5', '--verbose'));
opt = getopt(spec, c('--mean=5', '--verbose', '--sd', '5'));

spec = c(
  'date'     , 'd', 1, "character",
  'help'     , 'h', 0, "logical",
  'getdata'  , 'g', 0, "logical",
  'market'   , 'm', 1, "character",
  'threshold', 't', 1, "double"
);
opt = getopt(spec, c('--date','20080421','--market','YM','--getdata'));
opt = getopt(spec, c('--date','20080421','--getdata','--market','YM'));
opt = getopt(spec, c('--date','20080421','--getdata','--market','YM'),usage=TRUE,debug=TRUE);
print(getopt(spec, c('--date','20080421','--getdata','--market','YM'),usage=TRUE));
}
\keyword{ data }% at least one, from doc/KEYWORDS
