FROM rocker/shiny

MAINTAINER "Carl Boettiger" cboettig@ropensci.org

RUN R -e "install.packages('devtools')"
Run R -e "install.packages('Rcpp')"
RUN R -e "install.packages('dendextend')"
RUN R -e "install.packages('RColorBrewer')"
RUN R -e "library(devtools);library(Rcpp);install_github('ramnathv/rCharts', force= TRUE)"
RUN R -e "library(devtools);install_github('talgalili/dendextendRcpp')" 
