# Juan's B.Sc Thesis in Economics

Welcome to my B.Sc Thesis in Economics repository. The research title is "A characterization of Colombian industries using Schumpeter's patterns of innovation". Set to be submitted on November 25th, 2022

## What for

This is meant to be a public access repository for my thesis in the future, just in case someone wants to check what I did or use my script as groundwork for their own stuff.

## Key points

- Objective: Classify colombian industries using Joseph Schumpeter's Mark I and Mark II innovation archetypes
- Why: Characterizations using Schumpeterian archetypes have been made in the world, but not in Colombia.
- Usage: Econometrics and Policy implications
- How: Exploiting information from DANE's EAM and EDIT surveys (2018)
- Measures: Three commonly used in the literature: Concentration, Technological Opportunities and Stability
- Cluster: k-means algorithm (Lloyd). 2 groups and 10 repetitions. Data is on euclid distance (pca employed), and distance is standardized.

## Softwares used

R and LaTeX

## Small guide to the repo

The root folder contains the main lineages of the course (rules, due dates and so on). 

The ### ./src folder contains three folders. 

### ./src/data

This is the back end of my research. There are four important .R files: 

dataCleaning: Main filters and cleaning the database
dataAnalysis: Creation of measures
dataVisualization: All things figures + the cluster
dataMapping: All things maps

### ./src/defenses and ./src/writing

The front end of my research. Each has three folders, one for every due date during the course and the progress made up to that point.
Those folders with "Final" on the name indicate that is the last version of the file (my finished thesis)


## Inquiries

[E-Mail](mailto:jtabordaj@uninorte.edu.co)
