# MPHIL
Domesticating Cognition: Personality and Social Cognition in Dog/Human Dyads

Hello :) Here's a brief description of all the files I've uploaded:

CBARQ_Curation.R: Does what it says on the tin. Data curation of the large CBARQ dataset we acquired from Dr James Serpell at UPenn - removing questions/individuals with >15% missing data, removing individuals further than 4sd from the mean in personality traits, imputing missing data, etc. The curation process is described in Section 3.1.

CBARQ_Exp.R: Initial exploration of the CBARQ dataset after it's been curated - correlations, PCAs, etc. Followed by more detailed analysis, the results of which are reported in Section 3.3. 

Qualtrics_formatting.R: Curates novel paired dog and owner personality data collected through the Qualtrics platform. Curation process described in Section 3.2.

Qualtrics_Exp.R: Exploration and analysis of the Qualtrics dataset, the results of which are reported in Section 3.3.

Qualtrics variable and factor names.pdf: describes the variables and the factor names used in the Qualtrics dataset.

Heritability_Estimation.R: Self-explanatory, but has two parts. The first part determines which breeds I have personality data for, and creates a text file of a list of these breeds. This list is then to be used in plink to calculate a genomic relationship matrix, which is then fed back into R. The rest of the script estimates heritability of dog personality traits, first without and then with owner personality traits as fixed effects. This is described in Section 4.

Nei_SGD.R: Calculates a genetic distance matrix based on Nei's standard genetic distance.

NSD_for_phylip: This script also has two parts. First, formats the genetic distance matrix to make it suitable for phylip (e.g. giving each breed a unique 10-character name). The matrix is then used to make a midpoint-rooted neighbour-joining tree using phylip, and the tree is read back into R. The second part of the script reads in the tree and estimates correlations and rates of evolution of personality traits, as described in Section 5.
