library("mixexp")


 #DESIGN for fist experiment
 #a=Greencompost
 #b=fibercompost
 #c=Fiber roh
 #d= Fiber soft
 #design 4 compoent (fill= peat eg 50%)
 #4 mixtures with boundaries and fiber content max 30%
 coef <-matrix(c(0, 0, 1, 1), ncol = 4, byrow = TRUE)
 des<-Xvert(nfac = 4, lc = c(0, 0, 0, 0), uc = c(0.7, 0.7, 0.3, 0.3), nlc = 1, lb = c( 0), ub = c(0.3), coef,ndm=1,pseudo=F)
 des
 des$values<-runif(16,2,10)#random data
 
 mod1 <- lm(values ~ -1 + x1 + x2 + x3 + x4 + x1:x2 + x1:x3 + x2:x3 + x3:x4+ x1:x2:x3:x4, data = des)
 summary(mod1)

 
 test<-MixModel(frame = des, "values", mixcomps = c("x1", "x2", "x3","x4"), model = 4)
test$coefficients
summary(aov(test))
 

ModelPlot(model = test, dimensions = list(x1 = "x1", x2 = "x2", x3 = "x3"), 
          main = title["values"],  constraints = TRUE, contour = TRUE,  fill = TRUE)

option <- c(FALSE, TRUE) 
title <- c("Actual", "Pseudo") 
for ( i in 1:2){
for (x4 in c(0.0,0.3)) { 
  ModelPlot(model = test,      
            dimensions = list(x1 = "x1", x2 = "x2", x3 = "x3"), 
            
            slice = list(mix.vars = c(x4 = x4)), 
            main = paste(main = title[i], toString(x4, width = 4)), constraints = FALSE,
            lims=c(0,7,0,7,0,3),
            contour = TRUE, fill = T,
            axislabs = c("x1", "x2", "x3"),
            cornerlabs = c("x1", "x2", "x3"), pseudo = option[i]) 
}
}







title <- c("Actual", "Pseudo") 
option <- c(FALSE, TRUE) 
for (i in 1:2) { 
   ModelPlot(model = test, 
             dimensions = list(x1 = "x1", x2 = "x2", x3 = "x3"), 
             main = title[i], lims = c(0.35, 1, 0.20, 1, 0.15, 1),
             constraints = TRUE, contour = TRUE, cuts = 6, fill = TRUE,
             axislabs = c("x1", "x2", "x3"), cornerlabs = c("x1", "x2", "x3"),
             pseudo = option[i])
  }




#test_data

 orig <- Xvert(nfac = 3, lc = c(0.35, 0.2, 0.15), uc = c(1, 1, 1),
                 ndm = 1, plot = FALSE) 
y <- c(15.3, 20.0, 28.6, 12.5, 32.7, 42.4) 
orig <- cbind(orig[1:6, ], y) 
quadm <- lm(y ~ -1 + x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3, data = orig) 
quadm2<-MixModel(frame = orig, "y", mixcomps = c("x1", "x2", "x3"), model = 4)

title <- c("Actual Component Space", "Pseudo Component Space") 
option <- c(FALSE, TRUE) 
for (i in 1:2) { ModelPlot(model = quadm,
                        dimensions = list(x1 = "x1", x2 = "x2", x3 = "x3"),
                        main = title[i], lims = c(0.35, 1, 0.20, 1, 0.15, 1), 
                        constraints = TRUE, contour = TRUE, cuts = 6, fill = TRUE,
                        axislabs = c("x1", "x2", "x3"), cornerlabs = c("x1", "x2", "x3"), pseudo = option[i])
  }







mite <- SCD(4)
yavg <- c(1.8, 25.4, 28.6, 38.5, 4.9, 3.1, 28.7, 3.4, 37.4, 10.7, 22.0, + 2.6, 2.4, 11.1,0.8) 
mitemdM <- MixModel(frame = cbind(mite, yavg), response = "yavg", 
                    mixcomps = c("x1", "x2", "x3", "x4"), model = 4) 



 # DesignPoints(des)