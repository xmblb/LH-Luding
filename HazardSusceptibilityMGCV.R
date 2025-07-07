library(mgcv)
library(ordPens)
library(Metrics)
#####################################################
#########read data and get target variable###########
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.raw$Label = ifelse(data.raw$Area_SUM > 0, 1, 0)

library(parallel)
nc=10
cl=makeCluster(nc)

num.baseFun = 5
Formula.sus = Label ~ 
  s(Epicent_m, k = num.baseFun)+
  s(Road_m, k = num.baseFun)+
  s(PRC_s, k = num.baseFun)+
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun)+
  s(Aspect_s, k = num.baseFun)+
  s(Slope_m, k = num.baseFun)+
  # s(Lon, Lat, bs = "gp", k=40, m=2)+
  as.factor(Lith_magor)+
  as.factor(LULC_magor)
# s(ID_SU, bs = 'mrf', xt = list(nb latitude= nb))

start.time = Sys.time()
Fit.sus = mgcv::gam(Formula.sus, family = "binomial", data = data.raw)
# Fit.susSpa = mgcv::gam(Formula.size, family = "binomial", data = data.raw)
end.time = Sys.time()
print(end.time-start.time)


# save and load the model
# save(Fit.susSpa,file=paste0(data.path, "ModelFitSusSpa.RData"))
save(Fit.sus,file=paste0(data.path, "ModelFitSus.RData"))
# load("ModelFitSusSpatial.RData")


library(pROC)
dataLabel = data.raw$Label
predictedSus = Fit.sus$fitted.values
ROC.fit = roc(dataLabel~predictedSus)
ROC.fit$auc


#####################################################################
## plot goodness of fit##############################################
####################################################################
dev.off()
windows()
load(paste0(data.path, "ModelFitSusSpatial.RData"))
ROC.fitSpa = roc(data.raw$Label ~ Fit.sus$fitted.values)

load(paste0(data.path, "ModelFitSus.RData"))
ROC.fit = roc(data.raw$Label ~ Fit.sus$fitted.values)

library(ggplot2)

# 创建第一个模型的数据框
roc_data1 = data.frame(
  Specificity = ROC.fit$specificities,
  Sensitivity = ROC.fit$sensitivities,
  Model = "Model 1"
)

# 创建第二个模型的数据框
roc_data2 = data.frame(
  Specificity = ROC.fitSpa$specificities,
  Sensitivity = ROC.fitSpa$sensitivities,
  Model = "Model 2"
)

# 合并数据
roc_data = rbind(roc_data1, roc_data2)

# 使用 ggplot2 进行绘图
plot.roc = ggplot(roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
  geom_line(size = 1) +  # 线条宽度
  scale_color_manual(values = c("Model 1" = "#4682B4", "Model 2" = "#DC143C")) + # 颜色
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal() + 
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size=0.5),
    axis.title = element_text(size = 24),  # 坐标轴标题大小
    axis.text = element_text(size = 20, family = "sans", color="black"),   # 坐标轴刻度大小
    legend.title = element_blank(),        # 去掉图例标题
    legend.position = c(0.8, 0.1),
    legend.text = element_text(size = 20)  # 图例文本大小
  ) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, 
           linetype = "dashed", size = 1, color = "black")  # 添加虚线

ggsave(
  filename = "FittingSus.pdf",  # PDF 文件名
  plot = plot.roc,            # 要保存的图形对象
  path = "C:/Users/DELL/OneDrive/data_Luding/Results/",  # 文件保存路径（可根据需要修改）
  width = 6,                   # PDF 宽度（单位：英寸）
  height = 6,                  # PDF 高度（单位：英寸）
  units = "in"                 # 单位（英寸）
)

#####################################################
###plot Coefficients of  nonlinear covariates########
#####################################################
figure.info = plot.gam(Fit.size, pages = 1) # extract the effect information from the plot.gam
dev.off()
windows()
par(mar=c(8,6,2,2.3))


effect.cor = figure.info[[10]] # denote which covariate extract: plot the nonlinear effect
pdf(file = "sus slopeM.pdf", width = 7, height=7)
par(mar=c(4.7,4.7,2,2.3))
## plot the mean coefficients value, x is the x value, fit is the y value, se is the confidence interval
## in fact, the se is the 2 standard error value
plot(effect.cor$x, effect.cor$fit, type = "l", col = "#0000FF", lwd = 3, ylim = c(-8.,4),
     xlab = "Mean slope", ylab = "Regression coefficients",
     family = "sans", cex.lab=2, xaxt = 'n', yaxt = 'n')
box(lwd = 1.5)
axis(side = 1, lwd.ticks = 1.5, cex.axis = 1.6)
# axis(side = 1, at = c(-0.35, -0.15,0,0.15,0.35), labels= c(-0.35, -0.15,0,0.15,0.35),
#      lwd.ticks = 1.5, cex.axis = 1.6)
axis(side = 2, at = c(-8, -6,-4,-2,0,2,4), labels= c(-8, -6,-4,-2,0,2,4),
     lwd.ticks = 1.5, cex.axis = 1.6)
# axis(side = 2, at = c(-4, -3,-2,-1,0,1,2), labels=c(-4, -3,-2,-1,0,1,2),
#      lwd.ticks = 1.5, cex.axis = 1.6)

polygon(c(rev(effect.cor$x), effect.cor$x), 
        c(rev(effect.cor$fit - effect.cor$se), effect.cor$fit + effect.cor$se),
        col = 'gray80', border = NA)
## the polygon will cover the line, so we plot the line again
lines(effect.cor$x, effect.cor$fit, type = "l", col = "#0000FF", lwd = 3)
abline(h=0, col = "gray40", lwd = 2, lty=2)
dev.off()
#############################################################################



#####################################################
###plot Coefficients of  categorical covariates######
#####################################################
## plot the effect of lithology
dev.off()
windows()

pdf(file = "sus lithology.pdf", width = 7, height=7)
par(mar=c(5,5.5,2,2.3))
summ.mod = summary(Fit.size)
coeff.mod = summ.mod$p.coeff  #get all the coefficient of linear/categorical covariates
error.mod = summ.mod$se  #get all the standard error of all covariates
lith.names = c("as.factor(Lithology)2","as.factor(Lithology)3","as.factor(Lithology)4","as.factor(Lithology)5",
               "as.factor(Lithology)6","as.factor(Lithology)7","as.factor(Lithology)8","as.factor(Lithology)9",
               "as.factor(Lithology)10","as.factor(Lithology)11","as.factor(Lithology)12","as.factor(Lithology)13",
               "as.factor(Lithology)14","as.factor(Lithology)15")
lith.coeff = coeff.mod[which(names(coeff.mod) %in% lith.names)]
lith.error = error.mod[which(names(error.mod) %in% lith.names)]
## plot 
library(plotrix)
plotCI(x=1:15,y=c(0, lith.coeff),
       li=c(0, lith.coeff - 2*lith.error),ui=c(0, lith.coeff + 2*lith.error),
       ylim=c(-4,2.5), xlab = "Lithology", ylab = "Regression coefficients",
       family = "sans",cex.lab=2,xaxt="n", pch=19, col="red", scol="black",
       lwd=3, xaxt='n', yaxt='n')
box(lwd = 1.5)
axis(side = 2, at = c(-4, -3,-2,-1,0,1,2), labels=c(-4, -3,-2,-1,0,1,2),
     lwd.ticks = 1.5, cex.axis = 1.6)
abline(h=0, col = "gray40", lwd = 2, lty=2)
axis(side = 1, at=1:15,labels=c(1,'',3,'',5,'',7,'',9,'',11,'',13,'',15), 
     cex.axis = 1.6, family = "sans", lwd.ticks = 1.5)
abline(h=0, col = "gray40", lwd = 2, lty=2)

text(1,0.25,"A",cex=1.6,family = "sans")
text(2,0.8,"B",cex=1.6,family = "sans")
text(3,0.6,"C",cex=1.6,family = "sans")
text(4,0.45,"D",cex=1.6,family = "sans")
text(5,0.2,"E",cex=1.6,family = "sans")
text(6,2.35,"F",cex=1.6,family = "sans")
text(7,0.22,"G",cex=1.6,family = "sans")
text(8,0.9,"H",cex=1.6,family = "sans")
text(9,0.75,"I",cex=1.6,family = "sans")
text(10,1.9,"J",cex=1.6,family = "sans")
text(11,0.7,"K",cex=1.6,family = "sans")
text(12,1,"L",cex=1.6,family = "sans")
text(13,0.65,"M",cex=1.6,family = "sans")
text(14,2.,"N",cex=1.6,family = "sans")
text(15,0.7,"O",cex=1.6,family = "sans")

dev.off()
##########################################################################################


#####################################################
###plot Coefficients of  linear covariates######
#####################################################
dev.off()
windows()

pdf(file = "sus linear.pdf", width = 7, height=7)
par(mar=c(4.7,5.2,2,2.3))
summ.mod = summary(Fit.size)
coeff.mod = summ.mod$p.coeff  #get all the coefficient of linear/categorical covariates
error.mod = summ.mod$se  #get all the standard error of all covariates
lith.names = c("SlopeStd","Dis2FaultsStd")
lith.coeff = coeff.mod[which(names(coeff.mod) %in% lith.names)]
lith.error = error.mod[which(names(error.mod) %in% lith.names)]
## plot 
library(plotrix)
# par(mar=c(4.5,4.5,2,2.3))
plotCI(x=c(0.5,1.5),y=c(lith.coeff),
       li=c(lith.coeff - 2*lith.error),ui=c(lith.coeff + 2*lith.error),
       xlim = c(0,2), ylim=c(-4,2.5), ylab = "Regression coefficients",
       xlab = 'Linear covariates',family = "sans",cex.lab=2.5, pch=19, col="red", scol="black",
       lwd=3, xaxt='n', yaxt='n')
box(lwd = 1.5)
axis(side = 2, at = c(-4, -3,-2,-1,0,1,2), labels=c(-4, -3,-2,-1,0,1,2),
     lwd.ticks = 1.5, cex.axis = 2)
abline(h=0, col = "gray40", lwd = 2, lty=2)
axis(side = 1, at=c(0.5,1.5),labels=c('Slope-SD','DisFault-SD'), 
     cex.axis = 1.6, family = "sans", lwd.ticks = 2)
abline(h=0, col = "gray40", lwd = 2, lty=2)

dev.off()
###############################################


#####################################################
###plot Coefficients of  coordX and coordY##########
#####################################################
vis = getViz(Fit.size)
library(mgcViz)

dev.off()
windows()
par(mar=c(4.5,4.5,2,2.3))

## Plot 2D effect with noised-up raster, contour and rug for design points 
## Opacity is proportional to the significance of the effect
# plot(sm(vis, 12)) + l_fitRaster(noiseup = TRUE) + l_fitContour() 
plot(sm(vis, 11)) + l_fitRaster(noiseup = F) + l_fitContour() + 
  labs(title= "",x="Longitude", y="Latitude", fill = "sdf")+
  theme(axis.text=element_text(size=16, family = "serif", color = "black"), 
        axis.title=element_text(size=20, family = "serif"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect( colour = "black",size = 0.8),
        legend.position = c(0.89,0.2),
        legend.text = element_text(size=12, family = "serif", color = "black"),
        # legend.title = element_blank()
        legend.title = element_text(size=16, family = "serif", color = "black"))
#############################################################################



#####################################################
########plot Coefficients of cateforical year########
#####################################################
year.names = c("as.factor(Year)2","as.factor(Year)3","as.factor(Year)4","as.factor(Year)5",
               "as.factor(Year)6","as.factor(Year)7","as.factor(Year)8","as.factor(Year)9",
               "as.factor(Year)10","as.factor(Year)11","as.factor(Year)12","as.factor(Year)13",
               "as.factor(Year)14")
year.coeff = coeff.mod[which(names(coeff.mod) %in% year.names)]
year.error = error.mod[which(names(error.mod) %in% year.names)]
## plot the effect of categorical covariates
library(plotrix)
par(mar=c(4.3,4.3,0.5,2))
plotCI(x=1:14,y=c(0, year.coeff),
       li=c(0, year.coeff - 2*year.error),ui=c(0, year.coeff + 2*year.error),
       ylim=c(-1,1), xlab = "Year", ylab = "Regression coefficient",
       family = "serif",cex.axis = 1.7, cex.lab=2,xaxt="n", pch=19, col="red", scol="black",
       lwd=2)
axis(side = 1, at=1:14,labels=1:14, cex.axis = 1.2, family = "serif")
abline(h=0, col = "gray40", lwd = 2, lty=2)
##########################################################################################



