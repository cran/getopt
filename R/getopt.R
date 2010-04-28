getopt = function (spec=NULL,opt=commandArgs(TRUE),command=strsplit(commandArgs(FALSE)[4],"=")[[1]][2],usage=FALSE,debug=FALSE) {

  # notes on naming convention:

  # 1. an "option" is one of the shell-split input strings

  # 2. a "flag" is a subclass of "option".  a "flag" can be defined as having
  # no "argument" (see below), a required "argument", or an optional
  # "argument".

  # 3. an "argument" is a subclass of "option", and is the value associated
  # with a flag.

  # 4. a "long flag" is a subclass of flag, and begins with the string "--".
  # if the "long flag" has an associated "argument", it may be delimited from
  # the "long flag" by either an "=", or may be the subsequent "option".

  # 5. a "short flag" is a subclass of flag, and begins with the string "-".
  # if a "short flag" has an associated "argument", it is the subsequent
  # "option".  "short flags" may be bundled together, sharing a single leading
  # "-", but only the final "short flag" is able to have a corresponding
  # "argument".

  #spec contains at least 4 columns, as many as 5 columns.
  #
  # column 1: long name of flag
  #
  # column 2: short name of flag
  #
  # column 3: argument flag.  0=no argument, 1=required argument, 2=optional
  # argument
  #
  # column 4: mode of argument.  one of "logical", "integer", "double",
  # "complex", "character"
  #
  # column 5 (optional): description of option.
  #

  # littler compatibility - map argv vector to opt
  if (exists("argv", where = .GlobalEnv, inherits = FALSE)) {
    opt = get("argv", envir = .GlobalEnv);
  }

  ncol=4;
  maxcol=6;
  col.long.name    = 1;
  col.short.name   = 2;
  col.has.argument = 3;
  col.mode         = 4;
  col.description  = 5;

  flag.no.argument = 0;
  flag.required.argument = 1;
  flag.optional.argument = 2;

  result = list();
  result$ARGS = vector(mode="character");

  #no spec.  fail.
  if ( is.null(spec) ) {
    stop('argument "spec" must be non-null.');

  #spec is not a matrix.  attempt to coerce, if possible.  issue a warning.
  } else if ( !is.matrix(spec) ) {
    if ( length(spec)/4 == as.integer(length(spec)/4) ) {
      warning('argument "spec" was coerced to a 4-column (row-major) matrix.  use a matrix to prevent the coercion');
      spec = matrix( spec, ncol=ncol, byrow=TRUE );
    } else {
      stop('argument "spec" must be a matrix, or a character vector with length divisible by 4, rtfm.');
    }

  #spec is a matrix, but it has too few columns.
  } else if ( dim(spec)[2] < ncol ) {
    stop(paste('"spec" should have at least ",ncol," columns.',sep=''));

  #spec is a matrix, but it has too many columns.
  } else if ( dim(spec)[2] > maxcol ) {
    stop(paste('"spec" should have no more than ",maxcol," columns.',sep=''));

  #spec is a matrix, and it has some optional columns.
  } else if ( dim(spec)[2] != ncol ) {
    ncol = dim(spec)[2];
  }

  #sanity check.  make sure long names are unique, and short names are unique.
  if ( length(unique(spec[,col.long.name])) != length(spec[,col.long.name]) ) {
    stop(paste('redundant long names for flags (column ',col.long.name,').',sep=''));
  }
  if ( length(na.omit(unique(spec[,col.short.name]))) != length(na.omit(spec[,col.short.name])) ) {
    stop(paste('redundant short names for flags (column ',col.short.name,').',sep=''));
  }

  # if usage=TRUE, don't process opt, but generate a usage string from the data in spec
  if ( usage ) {
    ret = '';
    ret = paste(ret,"Usage: ",command,sep='');
    for ( j in 1:(dim(spec))[1] ) {
      ret = paste(ret,' [-[-',spec[j,col.long.name],'|',spec[j,col.short.name],']',sep='');
      if (spec[j,col.has.argument] == flag.no.argument) {
        ret = paste(ret,']',sep='');
      } else if (spec[j,col.has.argument] == flag.required.argument) {
        ret = paste(ret,' <',spec[j,col.mode],'>]',sep='');
      } else if (spec[j,col.has.argument] == flag.optional.argument) {
        ret = paste(ret,' [<',spec[j,col.mode],'>]]',sep='');
      }
    }
    # include usage strings
    if ( ncol >= 5 ) {
      max.long = max(apply(cbind(spec[,col.long.name]),1,function(x)length(strsplit(x,'')[[1]])));
      ret = paste(ret,"\n",sep='');
      for (j in 1:(dim(spec))[1] ) {
        ret = paste(ret,sprintf(paste("    -%s|--%-",max.long,"s    %s\n",sep=''),
          spec[j,col.short.name],spec[j,col.long.name],spec[j,col.description]
        ),sep='');
      }
    }
    else {
      ret = paste(ret,"\n",sep='');
    }
    return(ret);
  }

  #XXX check spec validity here.  e.g. column three should be convertible to integer

  i = 1;

  while ( i <= length(opt) ) {
    if ( debug ) print(paste("processing",opt[i]));

    current.flag = 0; #XXX use NA
    optstring = opt[i];


    #long flag
    if ( substr(optstring, 1, 2) == '--' ) {
      if ( debug ) print(paste("  long option:",opt[i]));

      optstring = substring(optstring,3);

      this.flag = NA;
      this.argument = NA;
      kv = strsplit(optstring, '=')[[1]];
      if ( !is.na(kv[2]) ) {
        this.flag = kv[1];
        this.argument = paste(kv[-1], collapse="=");
      } else {
        this.flag = optstring;
      }

      rowmatch = grep( this.flag, spec[,col.long.name],fixed=TRUE );

      #long flag is invalid, matches no options
      if ( length(rowmatch) == 0 ) {
        stop(paste('long flag "', this.flag, '" is invalid', sep=''));

      #long flag is ambiguous, matches too many options
      } else if ( length(rowmatch) > 1 ) {
        stop(paste('long flag "', this.flag, '" is ambiguous', sep=''));
      }

      #if we have an argument
      if ( !is.na(this.argument) ) {
        #if we can't accept the argument, bail out
        if ( spec[rowmatch, col.has.argument] == flag.no.argument ) {
          stop(paste('long flag "', this.flag, '" accepts no arguments', sep=''));

        #otherwise assign the argument to the flag
        } else {
          storage.mode(this.argument) = spec[rowmatch, col.mode];
          result[spec[rowmatch, col.long.name]] = this.argument;
	  i = i + 1;
	  next;
        }

      #otherwise, we don't have an argument
      } else {
        #if we require an argument, bail out
        ###if ( spec[rowmatch, col.has.argument] == flag.required.argument ) {
        ###  stop(paste('long flag "', this.flag, '" requires an argument', sep=''));

        #long flag has no attached argument. set flag as present.  set current.flag so we can peek ahead later and consume the argument if it's there
        ###} else {
          result[spec[rowmatch, col.long.name]] = TRUE;
          current.flag = rowmatch;
        ###}
      }

    #short flag(s)
    } else if ( substr(optstring, 1, 1) == '-' ) {
      if ( debug ) print(paste("  short option:",opt[i]));

      these.flags = strsplit(optstring,'')[[1]];

      done = FALSE;
      for ( j in 2:length(these.flags) ) {
        this.flag = these.flags[j];
        rowmatch = grep( this.flag, spec[,col.short.name],fixed=TRUE );

        #short flag is invalid, matches no options
        if ( length(rowmatch) == 0 ) {
          stop(paste('short flag "', this.flag, '" is invalid', sep=''));

        #short flag is ambiguous, matches too many options
        } else if ( length(rowmatch) > 1 ) {
          stop(paste('short flag "', this.flag, '" is ambiguous', sep=''));

        #short flag has an argument, but is not the last in a compound flag string
        } else if ( j < length(these.flags) & spec[rowmatch,col.has.argument] == flag.required.argument ) {
          stop(paste('short flag "', this.flag, '" requires an argument, but has none', sep=''));

        #short flag has no argument, flag it as present
        } else if ( spec[rowmatch,col.has.argument] == flag.no.argument ) {
          result[spec[rowmatch, col.long.name]] = TRUE;
	  done = TRUE;

        #can't definitively process this flag yet, need to see if next option is an argument or not
        } else {
          result[spec[rowmatch, col.long.name]] = TRUE;
          current.flag = rowmatch;
          done = FALSE;
        }
      }
      if ( done ) {
        i = i + 1;
        next;
      }
    }

    #invalid opt
    if ( current.flag == 0 ) {
      stop(paste('"', optstring, '" is not a valid option, or does not support an argument', sep=''));
      #TBD support for positional args
      #if ( debug ) print(paste('"', optstring, '" not a valid option.  It is appended to getopt(...)$ARGS', sep=''));
      #result$ARGS = append(result$ARGS, optstring);

    # some dangling flag, handle it
    } else if ( current.flag > 0 ) {
      if ( debug ) print('    dangling flag');
      if ( length(opt) > i ) {
        peek.optstring = opt[i + 1];
        if ( debug ) print(paste('      peeking ahead at: "',peek.optstring,'"',sep=''));

        #got an argument.  attach it, increment the index, and move on to the next option.  we don't allow arguments beginning with '-' UNLESS
	#specfile indicates the value is an "integer" or "double", in which case we allow a leading dash (and verify trailing digits/decimals).
        if ( substr(peek.optstring, 1, 1) != '-' |
	  #match negative double
	  ( substr(peek.optstring, 1, 1) == '-'
	  & regexpr('^-[0123456789]*\\.?[0123456789]+$',peek.optstring) > 0
	  & spec[current.flag, col.mode]== 'double'
	  ) |
	  #match negative integer
	  ( substr(peek.optstring, 1, 1) == '-'
	  & regexpr('^-[0123456789]+$',peek.optstring) > 0
	  & spec[current.flag, col.mode]== 'integer'
	  )
	) {
          if ( debug ) print(paste('        consuming argument *',peek.optstring,'*',sep=''));

          storage.mode(peek.optstring) = spec[current.flag, col.mode];
          result[spec[current.flag, col.long.name]] = peek.optstring;
          i = i + 1;

	#a lone dash
	} else if ( substr(peek.optstring, 1, 1) == '-' & length(strsplit(peek.optstring,'')[[1]]) == 1 ) {
          if ( debug ) print('        consuming "lone dash" argument');
          storage.mode(peek.optstring) = spec[current.flag, col.mode];
          result[spec[current.flag, col.long.name]] = peek.optstring;
          i = i + 1;

        #no argument
        } else {
          if ( debug ) print('        no argument!');

          #if we require an argument, bail out
          if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
            stop(paste('flag "', this.flag, '" requires an argument', sep=''));

          #otherwise set flag as present.
          } else if (
	    spec[current.flag, col.has.argument] == flag.optional.argument |
	    spec[current.flag, col.has.argument] == flag.no.argument 
	  ) {
  	    x = TRUE;
  	    storage.mode(x) = spec[current.flag, col.mode];
            result[spec[current.flag, col.long.name]] = x;
          } else {
            stop("this should never happen (1).  please inform the author.");
	  }
        }
      #trailing flag without required argument
      } else if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
        stop(paste('flag "', this.flag, '" requires an argument', sep=''));

      #trailing flag without optional argument
      } else if ( spec[current.flag, col.has.argument] == flag.optional.argument ) {
        x = TRUE;
        storage.mode(x) = spec[current.flag, col.mode];
        result[spec[current.flag, col.long.name]] = x;

      #trailing flag without argument
      } else if ( spec[current.flag, col.has.argument] == flag.no.argument ) {
        x = TRUE;
        storage.mode(x) = spec[current.flag, col.mode];
        result[spec[current.flag, col.long.name]] = x;
      } else {
        stop("this should never happen (2).  please inform the author.");
      }
    #no dangling flag, nothing to do.
    } else {
    }

    i = i+1;
  }
  return(result);
}
