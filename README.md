GHPM
================

Overview
--------

GHPM is a library written in R that grabs and parses data from a github repository making it easier for users to perform analytics. Using Github's flexible GraphQL API and R's dataframe structure, ghpm returns repository data about issues, milestones, project boards, and pull requests which can be melded together to identify useful analytical information.

GHPM provides the following functions to grab information:

#### Issues

-   `get_issues()` Gets general information about each issue.
-   `get_issue_labels()` Gets the labels associated with each issue.
-   `get_issue_assignees()` Gets the assigned people to each issue.
-   `get_issue_events()` Gets the events associated with being added to a project board or moving to a different column for each issue
-   `get_issue_comments()` Gets the comment history of each issue.

#### Project Board

-   `get_projectboard()` Gets general information about each project board involving which issues are in what columns.

#### Milestones

-   `get_milestones()` Gets general information about each milestone in a repository.

#### Pull Requests

-   `get_pullrequests()` Gets general information about each pull request
-   `get_pullrequest_reviewers()` Gets information about the reviewers associated with each pull request
-   `get_pullrequest_commits()` Gets the commits associated with each pullrequest.

There's also a general query function included `graphql_query()`.

Usage
-----

As an example, say I wanted to get detailed information about each commit with the label "feature" associated with a specified milestone. I could do the following:

``` r
library(ghpm)
library(dplyr)

# Returns a dataframe displaying the general information of each issue
issues <- get_issues("groupname", "packagename") 

# Filter for a specific milestone
issues_v1 <- issues %>% filter(milestone == "v0.1.0")

# Returns a dataframe of the labels of each issue and filter on "features"
features <- get_issue_labels("groupname", "packagename") %>% filter(label == "feature")

# Then all we need to do is join!

issue_features <- issues_v1 %>% left_join(features, by = issue)
```

GraphQL
-------

GraphQL's flexible API structure allows us to exactly specify the data we want and retrieve it. Looking through the `inst/` folder, I can specify for each query the type of information I want to retrieve (commits, pull requests, issues, project boards) and then what I want included in that data (ie: the name, id, author, date, etc...). This allows for creating custom designed queries specific to a user's needs.

Pending Problems
----------------

Here are the current obstacles:

-   Resolving tokens for two different API paths, Github Enterprise vs Github. Ideally this library will be used on repos hosted on both platforms and as such different authentication tokens need to be used to resolve this.

-   Pagination is also an annoying issue as there may come instances in the future where several queries need to be performed to retrieve data. As of currently, I can only query the first 100 items at a time (ie: only the first 100 commits/pull requests/issues). I'm also limited in size by the amount of nodes to query as I can't return a result with more than 500,000 nodes. As such pagination is needed to bind queries together.

Issues/Bugs
-----------

If you encounter an issue/bug, please create a new issue within the Github Issues Page including as much detail as possible.

License
-------

Released under the [BSD License](https://ghe.metrumrg.com/tech-experiments/ghpm/blob/master/LICENSE)
