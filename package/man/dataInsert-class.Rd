\name{dataInsert-class}
\Rdversion{1.1}
\docType{class}
\alias{dataInsert-class}
\alias{coerce,dataInsert,character-method}

\title{Class \code{"dataInsert"}}
\description{
This class provides a convenient method for converting native R data to
a representation appropriate for an SQL insert statement
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dataInsert", data.frame.instance)}.
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "dataInsert", to =
	"character")}:
      This method coverts the object to a string that is suitable for
      substitution into an SQL insert statement after INSERT INTO [table
      name].  For example, one could construct an insert statement by
      saying: \code{sql <- sprintf('INSERT INTO my_table \%s',
	as(dataInsert.object, 'character'))}
    }
    \item{initialize}{\code{signature(.Object = "dataInsert")}: ... }
  }
}

\section{Warning}{
  coercing a dataInsert object to a character works by checking the type
  of each of the elements in the data.frame.  If one instantiates a
  data.frame without explicitly specifying stringAsFactors=F, it will
  convert all strings to factors and class() will return 'factor'.  This
  will cause the type checking to miss characters and not escape them
  properly.  Generally, one should use stringsAsFactors=FALSE when
  instantiating a data.frame to be used for dataInsert.
}
  
\examples{
df <- data.frame(station_id=54, station_name="My station",
network_name="EC", favorite_stn=FALSE,
start_date=as.POSIXct('2012/01/10', tz='GMT'), stringsAsFactors=FALSE)
di <- new('dataInsert', df)
as(di, 'character')
}

\keyword{classes}
