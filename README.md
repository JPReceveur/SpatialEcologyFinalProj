# Quantitative landscape metrics as indicators of the environmental pathogem Mycobacterium ulcerans
This repository contains code for the spatial ecology final project (Fall 2020): Quantitative landscape metrics as indicators of the environmental pathogen Mycobacterium ulcerans

##Data files:

GhanaMUSurvey.csv, This file contains the raw data collected during the field survey:Benbow et. al. (2014)

##Legends/Metadata files:

GhanaMUSurveyColDescriptions.csv, This file contains descriptions of all column labels used in GhanaMUSurvey.csv

WorkflowDiagram.png, Contains a visual representation of the workflow used for analysis in this paper.

NumericLandUseClassDescriptions.xlsx, Cover classes coorosponding to numeric classes used in West Africa land cover used in this study (https://www.sciencebase.gov/catalog/item/5deffc05e4b02caea0f4f3fc)

##Code files:

FinalProject.rmd, contains all code used in this analysis. RMarkdown formatted

ImportData.r, contains code used in importing data from local (avialable in repository) and publicly available datasets.

LandscapeMetrics.r, contains code used in calculating landscape metrics around sampling points and building land use graphs (fig2, figS1), and overview graphs (fig1).

BuildingSDMModels.r, contains code used in building SDM models and graphs as well as evaluating models.

##Output files:

FinalProject.pdf, contains the computed output from FinalProject.rmd