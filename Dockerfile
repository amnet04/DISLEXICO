FROM rocker/shiny

MAINTAINER "Carl Boettiger" cboettig@ropensci.org

RUN R -e "install.packages(c('devtools','Rcpp','dendextend','RColorBrewer'))" && \
    R -e "library(devtools);library(Rcpp);install_github('ramnathv/rCharts', force= TRUE)" && \
    R -e "library(devtools);install_github('talgalili/dendextendRcpp')" 
