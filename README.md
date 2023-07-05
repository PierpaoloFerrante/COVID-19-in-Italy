# Epidemiology of COVID-19 in Italy, 2020-2022

This project aims to provide access to and analyze Italian institutional data on COVID-19. 

The used data are freely available and collected by the COVID-19 surveillance system, which is managed by the Italian Health Institute (ISS). The Civil Protection daily publishes regional data [here](https://github.com/pcm-dpc/COVID-19), while the ISS maintains, checks, corrects and updates regional records on COVID-19 infections in a national register as described [here]
(https://www.epicentro.iss.it/coronavirus/sars-cov-2-sorveglianza)

To estimate daily incidence cases based on the number of daily deaths and virus lethality, a negative binomial model was developed. For more details on the model, refer to the
https://www.frontiersin.org/articles/10.3389/fpubh.2022.986743/full


# Content

***Data***
- The input data are stored in the folder "DATA\Files", which contains daily condirmed cases, deaths, vaccine coverage of population and variants prevalence as well as estimated relative risks vanninated/unvaccinated of take the infection or die for it. 

\
***Statistical analysis*** 
- The "main.r" file contains the R code to import data COVID-19 data and makes the analyzes to proide estimates of incidence and lethality as well as the evaluation of helth policies
- The folder "plots" contains 3 .r files to make respectively incidence lethality and deaths plots. The incidence and death plots includes the vaccine campaign impact 


# Usage
- Download all the data from the folder data/files
- The  "main.R" file makes the statistical analysis.
- Before running  "main.R" file change in line 152 the path in the variable "your_data_folder" from "C:\\your\\local\\folder\\" to your local folder containing the data downoaded before.
- Run files in the folder plot to make incidence, lethality and death curves
