getopt = function (spec=NULL,opt=commandArgs(TRUE),debug=FALSE) {

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

  #spec contains 4 columns.
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

  ncol=4;
  col.long.name    = 1;
  col.short.name   = 2;
  col.has.argument = 3;
  col.mode         = 4;

  flag.no.argument = 0;
  flag.required.argument = 1;
  flag.optional.argument = 2;

  result = list();

  if ( is.null(spec) ) {
    stop('argument "spec" must be non-null.');
  } else if ( !is.matrix(spec) ) {
    spec = matrix( spec, ncol=ncol, byrow=TRUE );
  } else if ( dim(spec)[2] != ncol ) {
    stop(paste('"spec" should have ",ncol," columns.',sep=''));
  }
  #XXX check spec validity here.  e.g. column three should be convertible to integer

  i = 1;

  while ( i <= length(opt) ) {
    if ( debug ) {
      print(paste("processing",opt[i]));
    }

    current.flag = 0; #XXX use NA
    optstring = opt[i];


    #long flag
    if ( substr(optstring, 1, 2) == '--' ) {
      if ( debug ) {
        print(paste("  long option:",opt[i]));
      }
      optstring = substring(optstring,3);

      this.flag = NA;
      this.argument = NA;
      kv = strsplit(optstring, '=')[[1]];
      if ( !is.na(kv[1]) ) {
        this.flag = kv[1];
        this.argument = kv[2];
      }

      rowmatch = grep( this.flag, spec[,col.long.name],fixed=TRUE );

      #short flag is invalid, matches no options
      if ( length(rowmatch) == 0 ) {
        stop(paste('long flag "', this.flag, '" is invalid', sep=''));

      #short flag is ambiguous, matches too many options
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

      these.flags = strsplit(optstring,'')[[1]];

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

        #can't definitively process this flag yet, need to see if next option is an argument or not
        } else {
          result[spec[rowmatch, col.long.name]] = TRUE;
          current.flag = rowmatch;
        }
      }
    #invalid opt
    } else {
      stop(paste('"', optstring, '" is not a valid option', sep=''));
    }

    # some dangling flag, handle it
    if ( current.flag > 0 & length(opt) > i+1 ) {
      peek.optstring = opt[i+1];
      #got an argument.  attach it, increment the index, and move on to the next option.  we don't allow arguments beginning with '-'
      if ( substr(peek.optstring, 1, 1) != '-' ) {
        storage.mode(peek.optstring) = spec[current.flag, col.mode];
        result[spec[current.flag, col.long.name]] = peek.optstring;
        i = i+1;

      #no argument
      } else {
        #if we require an argument, bail out
        if ( spec[current.flag, col.has.argument] == flag.required.argument ) {
          stop(paste('flag "', this.flag, '" requires an argument', sep=''));

        #otherwise set flag as present.
        } else {
          result[spec[current.flag, col.long.name]] = TRUE;
        }
      }
    }



    i = i+1;
  }
  return(result);
}
