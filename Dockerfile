FROM rocker/verse:latest
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

COPY docker /docker
RUN Rscript /docker/install.R
