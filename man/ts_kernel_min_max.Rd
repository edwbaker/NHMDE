% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_kernels.R
\name{ts_kernel_min_max}
\alias{ts_kernel_min_max}
\title{Apply maximum/minimum kernel to data}
\usage{
ts_kernel_min_max(data, n = 60, mode = "max")
}
\arguments{
\item{data}{A numeric vector of data to be processed.}

\item{n}{An integer specifying the size of the kernel. Default is 60.}

\item{mode}{A character string specifying the mode of operation (max or min).}
}
\value{
A numeric vector of the same length as the input data.
}
\description{
This function applies a maximum kernel to the data. It uses a
Savitzky-Golay filter to smooth the data and then applies a kernel
to find local maxima.
}
