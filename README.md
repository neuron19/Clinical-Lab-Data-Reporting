# Clinical Labs & Reporting of Abnormal Values

Repo for generating a random data set of clinical labs and applying logic to flag abnormal values using R programming language.

# Overview

In a typical hospital setting, various tests are ordered on a regular basis and  there will be abnormal values. Here we 
randomly generate a Clinical Lab Data Set for 1000 patients, each having 32 different tests. Ranges are selected based on 
normal reference ranges which are routinely used in a hospital setting. Goal is to generate a separate report with ONLY 
abnormal values.

# Project Summary

The project is broken down into following steps.
1. Generate a random data set for 1000 patients with 32 different lab tests 
2. Based on the normal reference ranges, write the logic to create an end data frame with only Out of Range(OoR) values
3. Plot the frequecy of Out of Range(OoR) clinical labs

**ClinicalLabs.R** contains the R code script.
**Clinical Labs Data.txt** is the randomly generated data set with BOTH normal and abnormal values.
**Out of Range(OoR) Labs Data.txt** has the list of ONLY abnormal values of patients


# Additional Information

You can find additional information about clincal labs and  referenc ranges in Clinical Labs Reference Ranges file.