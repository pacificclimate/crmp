\name{is.significant}
\alias{is.significant}
\title{is.significant}
\description{
Calculates whether the KS statistic is significant or not
}
\usage{
is.significant(ks)
}

\arguments{
  \item{ks}{Result from running the \code{ks.test()} function; a list with
    class "htest"
  }
}
\details{
This function just uses the simple formula: \code{ks.stat > (1.22/sqrt(n))}
  when \code{n > 40}.
}
\value{
Returns TRUE if the KS statistic is significant, FALSE if it is not, NA
if ks is NA or comes from
}
\references{
\url{http://www.york.ac.uk/depts/maths/tables/pdf.htm}
}
\seealso{
  \link{ks.test}
}
