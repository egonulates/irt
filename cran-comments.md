## Package update

Updated the package and fixed the warnings raised in CRAN checks. Removed C++11 specification. 


## Test environments
* local Windows 11, R 4.3.2
* Debian Linux, R-devel, clang, ISO-8859-15 locale
* Fedora Linux, R-devel, GCC
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2022, R-oldrel, 32/64 bit
* Windows Server 2022, R-devel, 64 bit



## R CMD check results

There were no ERRORs or WARNINGs.

Some environments throw following NOTES:


* checking installed package size ... NOTE
  installed size is 17.9Mb
  sub-directories of 1Mb or more:
    libs  16.2Mb


* checking examples ... [68s/277s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                                user system elapsed
qip_index                      5.428  0.012  21.409
plot_empirical_icc2            5.095  0.028  20.984
plot.cat_output                3.421  0.006  13.919
ks                             2.915  0.464  14.079
plot_info                      3.325  0.016  13.324
classification_indices         3.291  0.015  13.107
plot.Itempool                  3.175  0.024  12.643
plot_empirical_icc             2.899  0.008  11.378
plot.Item                      2.187  0.020   8.859
classification_agreement_index 2.180  0.004  10.544
generate_ip                    2.109  0.004   8.285
summary.cat_output             1.660  0.004   6.481
ipd                            1.620  0.004   6.050
info                           1.419  0.004   5.864
generate_testlet               1.304  0.008   5.168


* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
