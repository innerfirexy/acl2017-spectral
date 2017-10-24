# Spectral Analysis of Information Density in Dialogue Predicts Collaborative Task Performance
Paper:
```
@inproceedings{xu2017spectral,
  title={Spectral Analysis of Information Density in Dialogue Predicts Collaborative Task Performance},
  author={Xu, Yang and Reitter, David},
  booktitle={Proceedings of the 55th Annual Meeting of the Association for Computational Linguistics (Volume 1: Long Papers)},
  volume={1},
  pages={623--633},
  year={2017}
}
```

## Steps to generate the figs and tables in the paper
1. Table 2 and Figure 4 can be reproduced using the code in DJD_PSO.R and MapTask_PSO.R
2. Table 3 can be reproduced by DJD_RP.R and MapTask_RP.R
3. The results in Table 4 can be reproduced by svm.R (the gamma value needs to be tuned in order to get exactly the same results)

## To be updated
1. Code and data for Table 1 (stationarity check) and Figure 2 (average spectra).
2. The optimal gamma values for generating the best R-squared.
