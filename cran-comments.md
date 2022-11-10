## Package update

Updated the package and fixed the warnings raised in CRAN checks. 


## Test environments
* local Windows 10, R 4.1.2
* Windows Server 2022, R-oldrel, 32/64 bit
* Windows Server 2022, R-devel, 64 bit
* Debian Linux, R-release, GCC
* Debian Linux, R-patched, GCC
* Debian Linux, R-devel, GCC
* Debian Linux, R-devel, clang, ISO-8859-15 locale
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC
* Fedora Linux, R-devel, clang, gfortran
* Fedora Linux, R-devel, GCC


## R CMD check results

There were no ERRORs or WARNINGs.

Some environments throw following NOTES:



* checking installed package size ... NOTE
  installed size is 26.0Mb
  sub-directories of 1Mb or more:
    libs  24.4Mb
    
* checking examples ... [68s/254s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                                user system elapsed
plot_empirical_icc2            5.350  0.012  19.472
qip_index                      5.077  0.004  18.627
classification_indices         3.859  0.005  14.257
plot.cat_output                3.615  0.001  14.204
ks                             2.929  0.505  12.720
plot.Itempool                  3.383  0.043  13.038
plot_info                      3.336  0.008  12.475
plot_empirical_icc             3.080  0.001  11.563
classification_agreement_index 2.267  0.003   7.949
generate_ip                    2.109  0.012   7.888
plot.Item                      2.047  0.025   7.492
summary.cat_output             1.629  0.004   6.609
info                           1.381  0.004   5.846
