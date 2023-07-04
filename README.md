# Epidemiology of COVID-19 in Italy, 2020-2022

This project aims to provide access to and analyze Italian institutional data on COVID-19. 

The used data are free and collected by the COVID-19 surveillance system, which is managed by the Italian Health Institute (ISS). The ISS maintains, checks, corrects and updates regional records on COVID-19 infections in a national register. 
https://www.epicentro.iss.it/coronavirus/sars-cov-2-sorveglianza

To estimate daily incidence cases based on the number of daily deaths and virus lethality, a negative binomial model was developed. For more details on the model, refer to the
https://www.frontiersin.org/articles/10.3389/fpubh.2022.986743/full


# Content

***Data***
- The input data are stored in the folder "DATA": You have to to download them in a your local folder
- Data can be directly downloaded through the "download.r" file

\
***Statistical analysis*** 
- The "main.r" file contains the R code which analyzes the data and proides estimates of incidenced and lethality as well as the evaluation of helth policies
- The folder "plots" contains 3 .r files to make respectively incidence lethality and deaths plots. The incidence and death plots includes the vaccine campaign impact 


# Usage
