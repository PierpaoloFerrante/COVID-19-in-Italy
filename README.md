# Epidemiology of COVID-19 in Italy, 2020-2022

This project aims to provide access to and analyze Italian institutional data on COVID-19. 

The used data are freely available and collected by the COVID-19 surveillance system, which is managed by the [Italian Health Institute](https://www.iss.it/)  (ISS). The [Civil Protection](https://emergenze.protezionecivile.gov.it/it/) daily publishes regional data [here](https://github.com/pcm-dpc/COVID-19), while the ISS maintains, checks, corrects and updates regional records on COVID-19 infections in a national register as described [here](https://www.epicentro.iss.it/coronavirus/sars-cov-2-sorveglianza).

To estimate daily incidence cases based on the number of daily deaths and virus lethality, a negative binomial model was developed. For more details on the model, refer to the [published article](https://www.frontiersin.org/articles/10.3389/fpubh.2022.986743/full).



# Content

***Data***
- The input data are stored in the folder "DATA\Files", which contains daily condirmed and deaths cases, vaccine coverage of the population, prevalence of variants, lethality of the original strain as well as estimated relative risks of acquiring the infection or dying from it for vanninated Vs unvaccinated individuals. 

\
***Statistical analysis*** 
- The "main.r" file contains the R code to import COVID-19 data and perform  analyzes to estimate incidence, lethality and evaluate helth policies.
- The folder "plot" contains three R files to generate  incidence lethality, and deaths plots. The incidence and death plots include the impact of the vaccine campaign. 


# Usage
- Download all the data from the "data/files" folder to your local directory.
- Open the "main.R" file and modify line 152 by changing the path in the variable "your_data_folder" from "C:\\your\\local\\folder\\" to the path of your local folder containing the downloaded data.
- Run the corrected "main.R" file to perform the statistical analysis.
- Run the files in the "plot" folder to generate incidence, lethality and death curves.
