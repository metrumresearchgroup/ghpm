% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectboard.R
\name{clone_projectboard}
\alias{clone_projectboard}
\title{Clones a projectboard}
\usage{
clone_projectboard(org, repo_from, number, repo_to, .api_url = api_url())
}
\arguments{
\item{org}{Name of organization to query}

\item{repo_from}{Name of repo to clone the projectboard from}

\item{number}{Number of the projectboard to clone}

\item{repo_to}{Name of repo to clone the projectboard to}

\item{.api_url}{Optional API url to query. Defaults to the value set by `api_url()`. Usually it's "https://api.github.com/graphql"}
}
\value{
The ID of the projectboard that was cloned
}
\description{
Clones a projectboard
}
