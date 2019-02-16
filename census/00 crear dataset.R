source("init.R")

zz=gzfile('data/input/Census/census-income.data.gz','rt')   
dataset_raw = read.csv(zz,header = F)

colnames(dataset_raw) = c("") # me sobran columnas...