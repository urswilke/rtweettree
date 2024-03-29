% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtweettree_plot.R
\name{autoplot.character}
\alias{autoplot.character}
\title{Plot rtweettree}
\usage{
\method{autoplot}{character}(x, add_profile_pics = TRUE, ...)
}
\arguments{
\item{x}{rtweet status_id (character string), rtweettree_data or
rtweettree_tbl_graph object. The object is first transformed to a
rtweettree_tbl_graph which is then plotted with ggraph.}

\item{add_profile_pics}{logical whether to add the profile pictures of the
users to the graph; defaults to TRUE; (should be set to FALSE for large graphs)}

\item{...}{arguments passed to methods}
}
\value{
An rtweettree ggraph plot.
}
\description{
Plot a tree graph of the data resulting of the `status_id` of a tweet.
}
\details{
The following functions are imported and then re-exported
from the ggplot2 package to avoid loading them.

rtweettree exported operators and S3 methods
}
\examples{
main_status_id <- "1438481824922181635"
\dontrun{
rtweettree_data_scraped <- rtweettree_data(main_status_id)
ggplot2::autoplot(rtweettree_data_scraped)
# when you're not interested to store the scraped data in an R object,
# you can also directly plot it with:
# autoplot(main_status_id)
}
# With package example dataset included:
ggplot2::autoplot(rtweettree_data_example)

}
