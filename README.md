GHPM
================

Overview
--------

GHPM is a library written in R that grabs and parses data from a github repository making it easier for users to perform analytics. Using Github's flexible GraphQL API and R's dataframe structure, ghpm returns repository data about issues, milestones, project boards, and pull requests which can be melded together to identify useful analytical information.

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

Issues/Bugs
-----------

If you encounter an issue/bug, please create a new issue within the Github Issues Page including as much detail as possible.

License
-------

Released under the [BSD License](https://ghe.metrumrg.com/tech-experiments/ghpm/blob/master/LICENSE)
