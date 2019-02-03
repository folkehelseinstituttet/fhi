#!/bin/bash

Rscript -e "devtools::load_all('$PWD'); fhi:::GenData(file.path('$PWD','inst','createddata'))"
