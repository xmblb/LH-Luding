library(mgcv)
library(ordPens)
library(Metrics)
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.raw$Label = ifelse(data.raw$Area_SUM > 0, 1, 0)


covariates.names = c("Shape_Area","Aspect_m","Aspect_s","Epicent_m" ,
                     "Epicent_s" , "Fault_m" , "Fault_s" , "PLC_m", "PLC_s",
                     "PRC_m","PRC_s", "Relief_m", "Relief_s","River_m", "River_s", "Road_m","Road_s", "Slope_m",
                     "Slope_s" , "PAG_3M_m", "PGA_3M_s","travel_ang", "travel_dis",
                     "delta_z", "Avg_Veloci", "STD_Veloci", "MAX_Veloci")

data4cor = data.raw[covariates.names]
cor.res = cor(data4cor, method = "pearson")
library(corrplot)
windows()
corrplot(cor.res, method = "color", addCoef.col = "black", number.cex = 0.4)
###############################################
### nonlinear civariates
############################################
covariates.names = c("Aspect_s", "Fault_m","PLC_m", 
                     "River_m",
                     "Slope_s")

num.baseFun = 5
for (cor.name in covariates.names) {
  Formula.size = Label ~ 
    s(Epicent_m, k = num.baseFun)+
    s(Road_m, k = num.baseFun)+
    s(PRC_s, k = num.baseFun)+
    s(PAG_3M_m, k = num.baseFun)+
    s(Aspect_m, k = num.baseFun)+
    s(Aspect_s, k = num.baseFun)+
    s(Slope_m, k = num.baseFun)+
    s(PRC_m, k = num.baseFun)+
    # s(Dis2FaultsMean, k = num.baseFun) +
    # s(NDVIm, k = num.baseFun) +
    as.factor(Lith_magor)+
    as.factor(LULC_magor)+
    s(get(cor.name), k = num.baseFun)
  start.time = Sys.time()
  Fit.size = mgcv::gam(Formula.size, family = "binomial", data = data.raw)  
  end.time = Sys.time()
  aic.value = AIC(Fit.size)
  print(paste(cor.name, " AIC:", round(aic.value, 0), "time: ", end.time-start.time))
}

###############################################
### linear covariates
############################################
## "Lith_magor", "LULC_magor",
Formula.size1 = Label ~ 
  s(Epicent_m, k = num.baseFun)+
  s(Road_m, k = num.baseFun)+
  s(PRC_s, k = num.baseFun)+
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun)+
  s(Aspect_s, k = num.baseFun)+
  s(Slope_m, k = num.baseFun)+
  as.factor(Lith_magor)+
  as.factor(LULC_magor)
Fit.size1 = mgcv::gam(Formula.size1, family = "binomial", data = data.raw, method = 'GCV.Cp')  

aic.value1 = AIC(Fit.size1)
print(paste(" AIC:", round(aic.value1, 0)))
  

