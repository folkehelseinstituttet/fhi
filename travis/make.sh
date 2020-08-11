#!/bin/bash
#TZ=":CET"
DATE=`date +%Y.%-m.%-d`
DATETIME=`date +%Y.%m.%d\ at\ %H:%M`
sed -i "s/^Version: .*$/Version: $DATE/" DESCRIPTION
head DESCRIPTION

sed -i "s/packageStartupMessage(\"Version.*$/packageStartupMessage(\"Version $DATETIME\")/" R/onAttach.R

Rscript -e 'install.packages("devtools");devtools::install_version("cpp11", version = "0.1", repos = "http://cran.us.r-project.org")'

## Other options:
## Only add if the commit is tagged: so something like:
#if [ $TRAVIS_TAG ] ; then
#   addToDrat
#fi
##but will need to edit .travis.yml since $TRAVIS_BRANCH will now equal $TRAVIS_TAG
