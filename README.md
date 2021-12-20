# Exposure to extremely partisan news from the other political side shows scarce boomerang effects

This repository contains the replication material of the article "Exposure to extremely partisan news from the other political side shows scarce boomerang effects", to be published at _Political Behavior_, by Andreu Casas, Ericka Menchen-Trevino, and Magdalena Wojcieszak.

## Data
The `./data/` directory contains the necessary data to replicate the analytical figures and tables of the paper. Below, we describe each of the datasets in this directory:

- `extreme-sites-experiment-maindb.csv`: pre and post survey data. The dataset also contains information about treatment compliance for those assigned to treatment.

- `extreme-sites-exp-browsing-data-toplot.csv`: browsing data for respondents in the treatment group. Contains information about how many times they accessed the extreme sites of the opposing ideology the days before, during, and after participating in the experiment.

- `robertson-fig2-sites-ideo-scores.csv`: ideology scores from Robertson et al. 2018, for the sites plotted in Fig.2.


## Code
The `./code/` directory contains separate scripts to replicate each analytical figure in the article. The `./figures/` directory contains a copy of each of the figures generated by these scripts. 

- [`01-fig1b-browsing-data.R`](https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/code/01-fig1b-browsing-data.R): replicates Figure 1b of the paper, in which we use the browsing  data we collected for some of the participants assigned to treatment to check whether on average they actually complied with the treatment.

<img src = "https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/figures/fig1b-browsing-data.jpeg">

- [`02-fig2-ideo-scores.R`](https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/code/02-fig2-ideo-scores.R): to replicate Figure 2 of the paper, where we show the distribution of the ideology scores (from Robertson et al. 2018) of the extreme  sites to which respondents in the treatment group were assigned, as well as the scores for some well-known sites, for face-validity purposes.

<img src = "https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/figures/fig2-ideo-scores.jpeg">

- [`03-fig3-results-attitude-polarization.R`](https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/code/03-fig3-results-attitude-polarization.R): to replicate Figure 3 of the paper, where we show the main (3.A) and moderator (3.B) effects of the treatment on attitude polarization. In the script we first generate Figure 3.A and then Figure 3.B. For the paper, we then pasted the two subfigures together.

<img src = "https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/figures/fig3a-extreme-sites-att-pol-MAIN.jpeg">

<img src = "https://github.com/CasAndreu/extreme_sites_scarce_boomerang/blob/main/figures/fig3b-extreme-sites-att-pol-MOD.jpeg">
