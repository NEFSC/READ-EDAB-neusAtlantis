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

## Change the default editor

You will find the terminal is needed for some git commands, mostly to "get out of trouble". A few steps for Rstudio

* First change the environment in which you'll be making these commands. Change the terminal:

    > `Tools -> Global Options -> Terminal`

Select Git Bash from the dropdown under the `shell` header. 

* Open a new Terminal:

    > `Tools -> Terminal -> New Terminal`

* Check to see if you have `nano` editor installed

    > Type `nano test` in the terminal. 
    > If it opens type Ctrl + X to close.

* Change the default editor `VI` to [nano](https://www.oreilly.com/library/view/gitlab-cookbook/9781783986842/apas07.html)

    > Type `git config --global core.editor "nano"` in the terminal

You should now have a good editor for custom git commands.

## Useful git commands

Git resources

* [https://git-scm.com/docs](https://git-scm.com/docs)
* [Undo stuff - 2015](https://github.blog/2015-06-08-how-to-undo-almost-anything-with-git/)
* [Pro Git book](https://git-scm.com/book/en/v2) and its [repo](https://github.com/progit/progit2)

### git rebase

Scenario: You commit a large file (100+MB) then subsequently make additional commits. When you push to GitHub, you recieve an error similar to this.

![](https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/docs/GitError-LargeFileSize.PNG)<!-- -->

You now want to remove the commit for the large file and push all subsequent commits.

1. Make sure you have no pending commits and that (in the following steps) you remove all commits related to the large file (for example, if you tried a git revert)

2. This removes the commit AND THE LARGE FILE from your local repo. Be sure to make a copy of the large file!

  > git rebase -i \< commit SHA \>
  >
  > Select a commit message before the one you want to remove
  >
  > The \<commit SHA\> can be found in the history in RStudios commit window.
    
3. When the `nano` editor opens find the line that reflects the commit you want to delete (circled)

![](https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/docs/rebaseInNano-LargeFileSize.PNG)<!-- -->

Follow these steps

  > Delete the line
  >
  > Ctrl + X to exit
  >
  > Save \"modified buffer\" to default location

### git reset

Scenario: You just made a local commit and either: 
  a. tried to push to GitHub and got an error message saying you can not push the changes (maybe due to file size) or
  b. you just changed your mind and would like to "undo" the commit
  
  Follow these steps
  
  > Make sure you are in the correct branch
  >
  > Open the terminal window
  >
  > git reset --soft HEAD~1
    
If you are in Rstudio, you will see the files reappear in your git tab assigned with check marks (ie. in the state right before you previously committed them)

### git revert

examples coming ...

### git lfs (large file storage)

Scenario: Large files (> 100MB) get rejected by GitHub when trying to push from a repo. 

Download `git lfs` for [windows, mac, or linux](https://github.com/git-lfs/git-lfs/releases/tag/v2.10.0).
This is a git extension called `lfs` which handles large files in need of tracking.

To track a file(s) use one of the following:

  > git lfs track "*.nc"
  >
  > git lfs track "images/*.nc"
  >
  > git lfs track "images"
    
To see which file types are being tracked by `lfs` and to list all files being tracked:

  > git lfs track
  > git lfs ls-files

"Git LFS handles large files by storing references to the file in the repository, but not the actual file itself. To work around Git's architecture, Git LFS creates a pointer file which acts as a reference to the actual file (which is stored somewhere else). GitHub manages this pointer file in your repository. When you clone the repository down, GitHub uses the pointer file as a map to go and find the large file for you."

The `.gitattributes` file is used to store information about the files being tracked by `lfs`. This file becomes part of the repo. Anyone who clones the repo is automatically configured to handle these large files.

* There are a few [limitations](https://github.com/git-lfs/git-lfs/wiki/Limitations), the most important being a file size cap of 2GB 

* Every account has 1 GB free. Or increments of 50GB for $5 a month. [Fee structure](https://help.github.com/en/github/setting-up-and-managing-billing-and-payments-on-github/about-billing-for-git-large-file-storage)

* Getting started [Video](https://www.youtube.com/watch?v=uLR1RNqJ1Mw)
* Git lfs [site](https://git-lfs.github.com/)



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
