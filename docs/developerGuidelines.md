---
title: "Atlantis Development Workflow"
output: 
  html_document:
    keep_md: true
    css: styling.css
---




Once a task item has been created and assigned to a developer the following set of steps should be used as a guide.

1. Create a branch from the [master](https://github.com/NOAA-EDAB/neus-atlantis) and name it. The naming convention should include the task item ID plus a description. For example,

  `migratorySpecies-ATLNTS-45` or `RealisticZoo-ATLNTS-43`

1. Create "archive" folder in the testing branch if developer needs to run multiple instances of Atlantis (for a given task) in parallel.

1. When a task is complete:

    i. Copy the contents of the .prm files to the appropriate at_xxx_test.prm file in the `testing` folder.
    
    ii. Any R files created during this task should be made into a function with arguments and its directions for use should be well documented. All R code should reside in the `R` folder of the branch. For example: see [exampleFunction.R](https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/R/exampleFunction.R)
    

1. Commit the changes to the branch

1. Initiate a pull request and make sure the title of the pull request is descriptive. Since other team members will be reviewing the pull request please make detailed comments regarding what was changed.

1. Make sure there a no conflicts.

1. Other team member will be notified of your request via email. They will be required to review the request, possibly test the changes (by running the code locally), and ultimately merge the pull request.

1. Delete branch on gitHub 

    > Note: The branch will remain on your PC even if you try to pull the deleted branch from gitHub. If you make changes to this branch locally in the future and then commit and push these changes,  the branch will be recreated.
    >
    > To remove the branch locally type the following in the terminal:
    >
    > `git branch -d branchName`
    >
    > `git fetch -p`
    >
    > For more info see [here](https://www.freecodecamp.org/news/how-to-delete-a-git-branch-both-locally-and-remotely/)

1. Repeat with additional tasks


## Testing environment

A test environment (repo) has been created called [atlantis_test](https://github.com/andybeet/atlantis_test) for developers to practive creating branches, conflicts, pull requests, and for merging pull requests. 

This repo can be cloned in Rstudio.

1. File -> New Project
1. Select Version Control
1. Select Git
1. Enter the url of the gitHub repository
1. Give the name of the directory to be created on your machine to hold the repo
1. Select the location of the directory (named above)
1. Click "Create Project"
