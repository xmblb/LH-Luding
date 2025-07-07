library(mgcv)
library(Metrics)
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.size = data.raw
# data.size$threshold = data.size$UnitArea/100.0
# ratio = c(0.40, 0.31, 0.24, 0.33, 0.14, 1.06, 0.39, 0.4, 0.25, 0.37, 0.22, 0.15, 0.45, 0.36)
# for (i in 1:14) {
#   idx = which(data.size$Year == i)
#   data.size$threshold[idx] = data.size$threshold[idx]*ratio[i]
# }
data4fit = subset(data.size, Area_SUM > 0)
data4fit$AreaLog = log(data4fit$Area_SUM)
###############################################
### nonlinear civariates
############################################
# covariates.names = c("Shape_Area","Aspect_m","Aspect_s","Epicent_m" ,
#                      "Epicent_s" , "Fault_m" , "Fault_s" , "PLC_m", "PLC_s",
#                      "PRC_m","PRC_s", "Relief_m", "Relief_s","River_m", "River_s", "Road_m","Road_s", "Slope_m",
#                      "Slope_s" , "PAG_3M_m", "PGA_3M_s","travel_ang", "travel_dis",
#                      "delta_z", "Avg_Veloci", "STD_Veloci", "MAX_Veloci")

covariates.names = c("Shape_Area","Aspect_m","Aspect_s","Epicent_m" ,
 "Epicent_s" , "Fault_m" , "Fault_s" , "Lith_magor", "LULC_magor","PLC_m", 
 "PRC_m","PRC_s","River_m", "Road_m", "Slope_m",   
 "Slope_s" , "PAG_3M_m", "PGA_3M_s","travel_ang", "travel_dis",
 "delta_z", "Avg_Veloci")


data4cor = data4fit[covariates.names]
cor.res = cor(data4cor, method = "pearson")
library(corrplot)
windows()
corrplot(cor.res, method = "color", addCoef.col = "black", number.cex = 0.4)
###############################################
### nonlinear civariates
############################################
covariates.names = c("Aspect_s",
                     "Epicent_s" ,"Fault_s" , "PLC_m", 
                     "PRC_s","River_m", 
                     "Slope_s" , "PGA_3M_s","travel_dis",
                    "Avg_Veloci", "delta_z")

num.baseFun = 8
for (cor.name in covariates.names) {
  Formula.size = log(Area_SUM) ~ 
    s(Epicent_m, k = num.baseFun) +
    s(Slope_m, k = num.baseFun) +
    s(Road_m, k = num.baseFun) +
    s(Fault_m, k = num.baseFun)+
    s(Shape_Area, k = num.baseFun) +
    s(PAG_3M_m, k = num.baseFun)+
    s(Aspect_m, k = num.baseFun) +
    s(PRC_m, k = num.baseFun) +
    s(travel_ang, k = num.baseFun)+
    as.factor(Lith_magor)+
    s(get(cor.name), k = num.baseFun)
  
  Fit.size = mgcv::gam(Formula.size, family = "gaussian", data = data4fit)  
  
  cor.value = cor(Fit.size$fitted.values, log(data4fit$Area_SUM), method = "spearman")
  aic.value = AIC(Fit.size)
  print(paste(cor.name, " AIC:", round(aic.value, 0), "R: ", cor.value))
}


###############################################
### linear covariates
############################################
## "Lith_magor", "LULC_magor",
Formula.size1 = log(Area_SUM) ~ 
  s(Epicent_m, k = num.baseFun) +
  s(Slope_m, k = num.baseFun) +
  s(Road_m, k = num.baseFun) +
  s(Fault_m, k = num.baseFun)+
  s(Shape_Area, k = num.baseFun) +
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun) +
  s(PRC_m, k = num.baseFun) +
  s(travel_ang, k = num.baseFun)+
  as.factor(Lith_magor)+
  as.factor(LULC_magor)

Fit.size1 = mgcv::gam(Formula.size1, family = "gaussian", data = data4fit)  
aic.value1 = AIC(Fit.size1)
cor.value1 = cor(Fit.size1$fitted.values, log(data4fit$Area_SUM), method = "spearman")
print(paste(" AIC:", round(aic.value1, 0), "R: ", cor.value1))


