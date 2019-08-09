
## submission (version 0.6) [xx/08/2019]

**new**: ...

<!--- ## submission (version 0.5.6) [17/12/2018]

**new**: very minor update (one function change and a few documentation fixes) --->

<!--- ## submission (version 0.5.5) [15/11/2018]

**new**: minor additions and simplifications

- resolved failing test for old R version 3.4.4
- diminished the number of Imports --->

<!--- ## submission (version 0.5.1) [20/09/2018]

**new**: minor modifications, mainly to resolve CRAN check issues

- set number of default threads used to 1, to avoid UBSAN warnings coming from usage of RcppParallel
- modified C++ code to avoid Solaris error --->

<!--- ## submission (version 0.5) [18/09/2018]

**new**: reimplementation of sentiment calculation code in C++, final set of API changes for better overall clarity, small bug and documentation fixes

- installed size > 5Mb, due to more compiled code
- examples now run significantly faster because of speed improvements --->

<!--- ## resubmission (version 0.4) [28/05/2018]

- modified example that took to long (to pass pre-test) --->

<!--- ## submission (version 0.4) [28/05/2018]

**new**: several additional functions and functionalities, and a few API changes --->

<!--- ## submission (version 0.3.5) [26/03/2018]

**new**: minor but necessary patches in to_global() and compute_sentiment() functions --->

<!--- ## resubmission (version 0.3) [18/03/2018]

- some examples modified to diminish elapsed time (to pass pre-test)
- R depends now >= 3.3.0, import of sentimentr omitted

## submission (version 0.3) [18/03/2018]

**new**: several additional functions and arguments, small bug fixes and clarifications in documentation 

- marked UTF-8 strings will remain; this is intentional and comes from the built--in French (mostly) and Dutch word lists --->

<!--- ## Re-submission (version 0.2) [12/11/2017]

- added reference to vignette paper in 'Description' field of DESCRIPTION file
- we relocated the code to the GitHub repo 'sborms/sentometrics' 
- changed quanteda::tokenize() to quanteda::tokens() due to errors in automatic checks by CRAN --->

