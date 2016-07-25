raw.ssn <- createSSN(n = 50,
                           obsDesign = systematicDesign(0.51),
                           predDesign = systematicDesign(0.51),
                           importToR = TRUE, path = "data/SimIterative.ssn",
                           treeFunction = iterativeTreeLayout)
plot(raw.ssn, lwdLineCol = "addfunccol", lwdLineEx = 8,
     lineCol = "blue", xlab = "x-coordinate",
     ylab = "y-coordinate",pch=NA)

createDistMat(raw.ssn, "preds", o.write=TRUE, amongpred = TRUE)

rawDFobs <- getSSNdata.frame(raw.ssn, "Obs")
rawDFpred <- getSSNdata.frame(raw.ssn, "preds")

#continuous covariates
rawDFobs[,"X1"] <- rnorm(length(rawDFobs[,1]))
rawDFpred[,"X1"] <- rnorm(length(rawDFpred[,1]))
rawDFobs[,"X2"] <- rnorm(length(rawDFobs[,1]))
rawDFpred[,"X2"] <- rnorm(length(rawDFpred[,1]))

#categorical covariate
rawDFobs[,"F1"] <- as.factor(sample.int(4,length(rawDFobs[,1]),
                                        replace = TRUE))
rawDFpred[,"F1"] <- as.factor(sample.int(4,length(rawDFpred[,1]),
                                         replace = TRUE))

#random effect
rawDFobs[,"RE1"] <- as.factor(sample(1:3,length(rawDFobs[,1]),
                                       replace = TRUE))
rawDFobs[,"RE2"] <- as.factor(sample(1:4,length(rawDFobs[,1]),
                                     replace = TRUE))
rawDFpred[,"RE1"] <- as.factor(sample(1:3,length(rawDFpred[,1]),
                                      replace = TRUE))
rawDFpred[,"RE2"] <- as.factor(sample(1:4,length(rawDFpred[,1]),
                                      replace = TRUE))

sim.out <- SimulateOnSSN(raw.ssn, ObsSimDF = rawDFobs,
                         PredSimDF = rawDFpred, PredID = "preds",
                         formula = ~ X1 + X2 + F1, coefficients = c(2,-1,5,3,2,10),
                         CorModels = c("LinearSill.tailup",
                                       "Exponential.Euclid"), use.nugget = TRUE,
                         CorParms = c(2, 1, 0.5, 1, 10),
                         addfunccol = "addfunccol")
sim.ssn<-sim.out$ssn.object

plot(sim.ssn, "Sim_Values",
     xlab = "x-coordinate", ylab = "y-coordinate",
     cex = 1.5)
