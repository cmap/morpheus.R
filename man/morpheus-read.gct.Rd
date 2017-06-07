\name{read.gct}
\alias{read.gct}
\title{Read a gct file}
\usage{
read.gct(file)
}
\arguments{
\item{file}{the name of the file which the data are to be read from}
}
\value{
A list with data, rowAnnotations, and columnAnnotations.
}
\description{
Read a gct formatted file. The selection in morpheus can be copied to the clipboard in gct format
 by selecting Edit>Copy Selected Dataset. To read clipboard data use read.gct(pipe("pbpaste")) on a
  Mac and read.gct("clipboard") on Windows.
}
