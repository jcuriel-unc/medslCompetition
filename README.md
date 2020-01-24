# medslCompetition
A set of functions to calculate competition at all levels of U.S. geography via the herfindahl index

## Purpose 
The medslCompetition package exists to make use of MIT Elections Data and Science Lab elections returns and calculate levels of competition
for various levels of elections. The link to the dataverse can be found here:
https://dataverse.harvard.edu/dataverse/medsl_election_returns

The medslCompetition package calculates competition not as an index with artificial cutpoints, but rather as a continous 0 - 1 variable
as calculated with the Herfindahl index.

## What is the Herfindahl index? 

The herfindahl index is a measure traditionally employed to calculate diversity. The calculation takes the sum of squared proportions, for
whatever level the proportions are of interest. For example, consider a hypothetical state where 80 percent of the population is white,
15 percent black, 4 percent latino, and 1 percent asian. To calculate diversity, the final score would be calculated as 

0.8^2 + 0.15^2 + 0.04^2 + 0.01^2 = 0.6642

Therefore, the Herfindahl index works such that scores closer to one equate to less diversity, and scores closer to zero greater diversity.
Within the context of elections, the score is calculated as the sum of squared vote share proportions. Therefore, most highly competitive 
elections between two parties would be centered around a value of 0.5 (i.e. 0.5^2 + 0.5^2 = 0.5), with areas where one party's candidates 
dominate closer to one, and areas where there are numerous parties/candidates all on par with each other closer to zero. 

### Why use the Herfindahl index over other measures? 
Typical election indices rely upon cutpoints that might occassionally be arbitrary and lead to noise in measuring election outcomes. The 
Herfindahl index avoids such problems by allowing the various components that can be too rigid in electoral indices to instead be fluid. 
The herfindahl index will as a mathematical function perform no worse than other electoral competitivenes indices, and likely better, 
especially in the event where there are higher third party votes or in the context of non-partisan elections. 

## How to make use of the package and functions

The medslCompetition package contains three fucntions, two related directly to processing MEDSL election returns data at the precinct 
level, and one malleble enough to be applicable to any standard long form data set. The two MEDSL functions are localCompetition and 
medslCompetition, with the first for local elections returns data, and the second for all other levels of competition. So long as 
the data is downloaded from the MEDSL dataverse, all one needs to do is provide the function the data.frame object, and whether they 
would like district magnitude to be included in the calculation. 

### Important note on district magnitude
As of January 24, 2020, the field district magnitude is not included in MEDSL data. This is important because elections where the top N 
vote getters win can throw off the result. For example, consider a typical New England town council where the top 5 candidates win. If
one were to assume that only one person wins the office, than the herfindahl index would caluclate a scenario where each candidate won 
one-fifth of the vote as 0.2, highly competitive, when in reality the election is not competitive at all.

Therefore, the functions of localCompetition and medslCompetition have the option to incorporate district magnitude. If the user sets 
the argument mag=TRUE, then the functions will search for a column named "magnitude". Once the field is found, the calculation of the 
Herfindahl index will instead take the sum of squared proportions for each candidate starting with the last place candidate who still
places in the top N. Therefore, in the event that the top 5 candidates win an election and each candidate won 100 votes, the 
Herfindahl index would be calculated as 1^2/1 = 1, i.e. not competitive at all. If the user sets the magnitude = 1 for all races, 
the results are the same as if mag = FALSE. 

MEDSL plans to incorporate such information starting with legislative elections later in 2020, and will seek outside help with on 
the ground knowledge for localities starting later in 2020. In the event that you have questions, concerns, or comments, please 
contact John Curiel at jcuriel@mit.edu. 
