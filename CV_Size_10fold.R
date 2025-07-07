library(mgcv)
library(Hmisc)
library(Metrics)
#####################################################
#########read data and get target variable###########
#####################################################
data.path = "C:/Users/DELL/OneDrive/data_Luding/Codes/"

## load data from file: 
data.raw = read.table(paste0(data.path, "LuDingData.csv"),sep=",", header=TRUE) ## read data matrix
data.size = data.raw
data4fit = subset(data.size, Area_SUM > 0)
data4fit$AreaLog = log(data4fit$Area_SUM)
#########################################################################


##################################
###create the formula for model###
##################################
num.baseFun = 8
Formula.size = log(Area_SUM) ~ 
  s(Epicent_m, k = num.baseFun) +
  s(Slope_m, k = num.baseFun) +
  s(Road_m, k = num.baseFun) +
  s(Fault_m, k = num.baseFun)+
  s(Shape_Area, k = num.baseFun) +
  s(PAG_3M_m, k = num.baseFun)+
  s(Aspect_m, k = num.baseFun) +
  s(Lon, Lat, bs = "gp", k=200, m=2)
#########################################################################


##################################
######execute the 10-fold CV######
##################################
n = 1878
data4fit$newID = 1:n
n.partition=c(rep(188,9),186)## get the number of 10 subsets
set.seed(1)
idxlist=partition.vector(sample(1:n,size=n),n.partition) # get the idx for each subset


## loop for 10-fold CV modelling
predict.all = data.frame() # save all the predictions
for (i in 1:10) {
  idx2pred=idxlist[[i]] # find the index
  data.train = data4fit[-idx2pred,] # nine subsets
  data.val = data4fit[idx2pred,] # the left-out subset
  
  Fit.size = mgcv::gam(Formula.size, family = "gaussian", data = data.train) 
  predict.val = predict(Fit.size, data.val)  # predict the validation data
  temp = cbind(ID = data.val$newID, ExpandedSlide = data.val$Area_SUM, 
               predictSize = predict.val, randomID = replicate(length(idx2pred), c(i)))
  predict.all = rbind(predict.all, temp)
  
  ## calculate the metrics
  train.mae = mae(Fit.size$fitted.values, log(data.train$Area_SUM))
  train.rmse = rmse(Fit.size$fitted.values, log(data.train$Area_SUM))
  train.cor = cor(Fit.size$fitted.values, log(data.train$Area_SUM), method = "spearman")
  
  val.mae = mae(predict.val, log(data.val$Area_SUM))
  val.rmse = rmse(predict.val, log(data.val$Area_SUM))
  val.cor = cor(predict.val, log(data.val$Area_SUM), method = "spearman")
  
  print(paste0(i," train data: MAE:", round(train.mae,3), " RMSE:", round(train.rmse,3),
               " R:", round(train.cor, 3)))
  print(paste0(i," val data: MAE:", round(val.mae,3), " RMSE:", round(val.rmse,3),
               " R:", round(val.cor, 3)))
  print('------------------------------------')
}
#########################################################################
write.table(predict.all, file = paste0(data.path,"Predictions_SizeSpa_10fold.csv"), sep = ",", row.names =FALSE)





########################################################
#################predictive performance#################
########################################################
library(ggplot2)
library(grid)
predict.all = read.csv(paste0(data.path,"Predictions_Size_10fold.csv"))
predict.allSpa = read.csv(paste0(data.path,"Predictions_SizeSpa_10fold.csv"))

data.fitplot = data.frame(obs = log(predict.all$ExpandedSlide), fit = predict.all$predictSize)
data.fitplotSpa = data.frame(obs = log(predict.allSpa$ExpandedSlide), fit = predict.allSpa$predictSize)
data.lineplot = data.frame(linx = c(0,15), liny = c(0,15))
# dev.off()
# windows()

plot.sizeSpa = ggplot(data.fitplotSpa, aes(x = obs, y = fit)) + xlim(4, 15) + ylim(4, 15)+
  geom_point(size = 1) +  # 绘制散点
  labs(x = "Observed"~"["*"ln"*"("*"m"^"2"*")"*"]", y = "Predicted"~"["*"ln"*"("*"m"^"2"*")"*"]") +
  theme(panel.grid = element_blank(), # 让图中间没有灰色格网,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 24,  color = "black"),
        axis.ticks = element_line(color = "black", size=0.5),
        axis.text = element_text(size = 20, family = "sans", color="black"),
        plot.title = element_text(size = 24, color = "black", family = "sans"))+ 
  geom_abline(intercept = 0, slope = 1, colour="red", linetype="dashed",size=1)
  # annotation_custom(
    # grob = textGrob("MAE = 1.006\nRMSE = 1.272\nPCC = 0.676",
    #                 gp = gpar(fontsize = 20, col = "black"),
    #                 just = "left"),
  #   xmin = 8.5, xmax = 13, ymin = 3, ymax = 7
  # )

plot.size = ggplot(data.fitplot, aes(x = obs, y = fit)) + xlim(4, 15) + ylim(4, 15)+
  geom_point(size = 1) +  # 绘制散点
  labs(x = "Observed"~"["*"ln"*"("*"m"^"2"*")"*"]", y = "Predicted"~"["*"ln"*"("*"m"^"2"*")"*"]") +
  theme(panel.grid = element_blank(), # 让图中间没有灰色格网,
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 24,  color = "black"),
        axis.ticks = element_line(color = "black", size=0.5),
        axis.text = element_text(size = 20, family = "sans", color="black"),
        plot.title = element_text(size = 24, color = "black", family = "sans"))+ 
  geom_abline(intercept = 0, slope = 1, colour="red", linetype="dashed",size=1)
  # annotation_custom(
    # grob = textGrob("MAE = 1.051\nRMSE = 1.329\nPCC = 0.643",
    #                 gp = gpar(fontsize = 20, col = "black"),
    #                 just = "left"),
    # xmin = 8.5, xmax = 13, ymin = 3, ymax = 7)

main.plot = gridExtra::grid.arrange(
  plot.sizeSpa, plot.size,   # 两个图
  ncol = 2, nrow = 1      
)
ggsave(
  filename = "10foldSize.pdf",  # PDF 文件名
  plot = main.plot,            # 要保存的图形对象
  path = "C:/Users/DELL/OneDrive/data_Luding/Results/",  # 文件保存路径（可根据需要修改）
  width = 12,                   # PDF 宽度（单位：英寸）
  height = 6,                  # PDF 高度（单位：英寸）
  units = "in"                 # 单位（英寸）
)
#######################################################################################

