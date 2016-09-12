# Overview
A Haskell Grader (AHG) is an automated grading system built in Haskell and using QuickCheck where professors can upload homework
assignments with a specified format into a Git repo.  Students will pull the repo and push their homework solutions to their respective
branch.  AHG will use QuickCheck to create test cases and verify student's solutions. It will then post their grade to the the student's branch
and automatically email them that their homework has been graded.  

All of this is enabled by using [GitLab](https://about.gitlab.com/).  Gitlab must be configured approapriately to work with AHG.  These configurations
are located in the document titled: "Gitlab Documentation.txt" located in the AHG repo.  