Breed  Management - Package: BreedManag
==========================

If you just arrived:

Make sure that you have all necessary packages for our functions.

 (shiny | readxl | pedigreemm | DT | plyr | reshape2 | readxl)


If necessary use installBM() function to download all required packages (internet access).

Available Functions:

IndexSM():
In this function is possible to calculate index based on EBV from different or unique analysis (BLUP). It is also possible to calculate multiple index (eg. for multibreed analysis). Optional functions include the use of mating recommendations (male/female), estimation of inbreeding coefficient, addition of phenotypic information, classification within families and count females of the same family by location.

MatingRecommendation(): Makes it possible to make the mating recommendation based on the limit of inbreeding coefficient in the progeny. Also it is possible to limit the number of generations traced back and to include limit matings per male.

CheckMating(): Checks if mates are being done according to the recommendation.

Inb(): Calculate inbreeding coefficients for a animal list.