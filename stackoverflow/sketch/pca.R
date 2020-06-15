tmp = select_if(dataset,function(x) table(x)>1)

dummifier = dummyVars(Male~.,dataset[,sapply(dataset,function(x) length(unique(x))) > 1])
dummy = predict(dummifier,dataset)
covariance = cov.wt(dummy)
covariance = covariance[complete.cases(covariance),complete.cases(covariance)]
princomp(covmat = covariance)
