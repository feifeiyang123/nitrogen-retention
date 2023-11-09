##############################################################################
#This script is meant to reproduce the main results of: 
#Natural ecosystems outperform agricultural lands in global soil nitrogen retention

##############################################################################
#Fig1-b
library(rgdal)
library(ggplot2)
library(tidyr)
library(ggalluvial)
library(stringr)
library(reshape2)
library(ggforce)
sample<-read.csv("globaldata2039.csv")
size <- read.csv("size_2039.csv")
sample <- merge(sample,size,by="Sample.id")

# read shapefile
wmap <- readOGR(dsn="ne_110m_land", layer="ne_110m_land")

# convert to dataframe
wmap_df <- fortify(wmap)

# create a blank ggplot theme
theme_opts <-list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill="white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=22,hjust = .5)))

sample.public <- sample[(sample$Type=="Public"),]
sample.public <- data.frame(lat=sample.public$lat,lon=sample.public$lon,size=sample.public$num_seq,landtype=sample.public$LandcoverClass)
sample.inhouse <- sample[(sample$Type=="In-house"),]
sample.inhouse <- data.frame(lat=sample.inhouse$lat,lon=sample.inhouse$lon,size=sample.inhouse$num_seq,landtype=sample.inhouse$LandcoverClass)
sample.inhouse$size <- as.numeric(as.character(sample.inhouse$size))
sample.public$size <- as.numeric(as.character(sample.public$size))
sample$size<-as.numeric(as.character(sample$num_seq))

sample$LandcoverClass<-factor(sample$LandcoverClass,levels = c(
  "Agricultural land","Wetland","Forest","Grassland","Shrubland","Bare Land",
  "Tundra"
))

ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon(fill = "gray90") + 
  coord_equal() + 
  theme_opts+
  geom_point(data=na.omit(sample),
             aes(lon, lat,color=LandcoverClass,size=size),
             alpha=.8,
             inherit.aes = FALSE)+
  scale_size_continuous(range = c(1,4))+
  scale_color_manual(values=c("#C17360","#586384","#367168","#7F9793","#DBCDA7","#EABA91","#7E7E7E"))+
  theme(legend.position = "none")

##############################################################################
#Fig2-a
library(ggplot2)
data1<-read.csv("pro.csv")
library(ggpie)
colors <- c("#D7867F", "#99C7D7", "#818AA6", '#80B6A8', "#E7B7A9")
pathway <- c("AON","NR","NOA","NF","M")
data1$X<-factor(data1$X,levels=pathway)

ggplot(data1)+
  geom_bar(aes(x=1,y=Grassland, fill=X),
           stat = 'identity', width = .8, color = 'white')+
  scale_fill_manual(values = colors)

last_plot() +
  coord_polar(theta="y")

last_plot()+
  xlim(c(-1, 2)) +
  theme_void()+
  theme(legend.position = "none",
        text = element_text(family='mono', color='white'),
        plot.title = element_text(family='Roboto-Thin', size=20, hjust=0.5),
        plot.margin = unit(c(1,2,1,2), 'cm'))

#Fig2-b
a<-read.csv("pathwayI11.csv")
a1<-a[,-c(1:2)]

data<-melt(a1,id.vars = c("type"),variable="Pathway",value.name = "value")

ggplot(data,aes(x = type, y = value, color = type))+
  geom_boxplot(outlier.color = "red",outlier.shape = 7,outlier.size=1)+
  facet_wrap(~ Pathway,nrow = 4,
             as.table = TRUE,
             shrink = TRUE,
             scales = "free")

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

library(dplyr)
df2 <- data %>%
  group_by(type) %>%
  mutate(value.1= remove_outliers(value))
df2<-df2[complete.cases(df2),]

ggplot(df2,aes(x = type, y = value.1, color = type))+
  geom_boxplot(outlier.color = "red",outlier.shape = 7,outlier.size=1)+
  facet_wrap(~ Pathway,nrow = 4,
             as.table = TRUE,
             shrink = TRUE,
             scales = "free")

library(ggplot2)
library(ggsignif)
library(ggpubr)
library(RColorBrewer)
compaired <- list(c("Agricultural land", "Forest"), 
                  c("Agricultural land","Grassland"), 
                  c("Agricultural land","Wetland"),
                  c("Forest","Grassland"),
                  c("Forest","Wetland"),
                  c("Wetland","Grassland")
)

data$type<- factor(data$type,
                   levels=c("Agricultural land","Wetland","Forest","Grassland"))

library(ggh4x)
colnames(Ag)
library(ggplot2)
write.csv(df2,"df.csv")

p1<-ggplot(df2, aes(x = type, y =value, color = type)) +
  #geom_blank(data=blank_data,aes(x=type,y=value))+
  geom_jitter(size = .2,alpha = .3,
              position = position_jitterdodge(jitter.width = 0.35,
                                              jitter.height = 0,
                                              dodge.width = 0.8))+
  geom_violin(width=1,alpha=0.2,
              position=position_dodge(width = 0.8),
              size=0.75) +
  geom_boxplot(alpha=0.2,
               width=0.2,position=position_dodge(0.9),
               size=0.75,outlier.colour = NA)+
  theme_bw() +
  ylab("Relative Abundance")+ xlab("")+
  facet_wrap(~ Pathway,nrow = 1,
             as.table = TRUE,
             shrink = TRUE,
             scales = "free_y")+
  expand_limits(y = 0)+
  theme(panel.background = element_blank(),
    axis.text.x = element_text(angle =-65, hjust =0, vjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "none"
  )+
  scale_color_manual(values=c("#C17360","#367168","#7F9793","#586384"))+
  geom_signif(comparisons = compaired,
              step_increase = 0.05,
              #y_position = 3.5,
              #y_position=c(2.8,3.0,3.2,3.4,3.6,3.8),
              map_signif_level = T,
              tip_length = 0,
              size=0.5,
              textsize=2,
              test = wilcox.test)

facetted_pos_scales(
  y = list(
    pathway == "Ammonium oxidation to nitrate" ~ scale_y_continuous(limits = c(0.0,0.12)),
    pathway == "Nitrogen removal" ~ scale_y_continuous(limits = c(0.0,0.4)),
    pathway == "Nitrogen fixation" ~ scale_y_continuous(limits = c(0.0,0.5)),
    pathway == "Nitrate reduction to ammonium" ~ scale_y_continuous(limits = c(0,0.8)),
    pathway == "Mineralization" ~ scale_y_continuous(limits = c(0,0.8))
  )
)
p1

##############################################################################
##Fig3
library(data.table)
library(car)
library(ClustOfVar)
library(data.table)
require(lme4)
require(modelr)
require(car)
library(readr)
pathwayI <- read_csv("pathwayI.csv")
metadata <- read.csv("envdata_2039.csv",row.names = 1,as.is = TRUE)

env_A<-merge(pathwayI,metadata,by="Sample.id")
env<-na.omit(env_A)
write.csv(env,"env_A.csv")
envdata<-read.csv("envdata.csv")
data<-merge(metadata,envdata,by="Sample.id")
write.csv(data,"data.csv")

## random forest
#options(repos='http://cran.rstudio.com/')
library(xgboost)
require(randomForest)
#install.packages("xgboost")
#BiocManager::install("xgboost")
library(readr)
library(stringr)
library(caret)
library(car)
require(lme4)
library(data.table)
library(car)
library(cAIC4)
library(caret)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
colnames(env_A)[c(4:8)]<-c("AON","NR","NOA","NF","M")
                           
## random forest
require(randomForest)
env<-read.csv("envdata.csv",header=T)
pathwayI<-read_csv("pathwayI.csv")
colnames(pathwayI)
env_path<-merge(pathwayI,env,by="Sample.id")
colnames(env_path)[c(1:5)]<-c("AON","NR","NOA","NF","M")
write.csv(env_path,"env_path.csv")
env_path<-env_path[,-c(1:3,9:10)]
colnames(env_path)[,c(1:5)]
dat.rf<-env_path[,-c(2:5)]
dat.rf.NR<-env_path[,-c(1,3:5)]
dat.rf.NOA<-env_path[,-c(1:2,4:5)]
dat.rf.NF<-env_path[,-c(1:3,5)]
dat.rf.M<-env_path[,-c(1:4)]
colnames(dat.rf.NR)[1]<-"NR"
colnames(dat.rf)[1]<-"AON"
colnames(dat.rf.NOA)[1]<-"NOA"
colnames(dat.rf.NF)[1]<-"NF"
colnames(dat.rf.M)[1]<-"M"
write.csv(dat.rf,"AON.csv")

env<-read.csv("env_path.csv")
colnames(env)
dat.rf.AON<-env[,-c(2:3,4:7)]
dat.rf.NF<-env[,-c(1:3,5:7)]
dat.rf.M<-env[,-c(1:4)]

rf_ntree = randomForest(AON ~., data=dat.rf.AON, na.action=na.omit)
plot(rf_ntree)
rf_ntree = randomForest(NR ~., data=dat.rf.NR, na.action=na.omit)
plot(rf_ntree)
rf_ntree = randomForest(NOA ~., data=dat.rf.NOA, na.action=na.omit)
plot(rf_ntree)
rf_ntree = randomForest(NF ~., data=dat.rf.NF, na.action=na.omit)
plot(rf_ntree)
rf_ntree = randomForest(M ~., data=dat.rf.M, na.action=na.omit)
plot(rf_ntree)

########try mtry###########
dat.sel.AON=na.omit(dat.rf.AON)
dat.sel.NR=na.omit(dat.rf.NR)
dat.sel.NOA=na.omit(dat.rf.NOA)
dat.sel.NF=na.omit(dat.rf.NF)
dat.sel.M=na.omit(dat.rf.M)
mtry=tuneRF(x=dat.sel.AON[,-1], y=dat.sel.AON[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
mtry=tuneRF(x=dat.sel.NR[,-1], y=dat.sel.NR[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
mtry=tuneRF(x=dat.sel.NOA[,-1], y=dat.sel.NOA[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
mtry=tuneRF(x=dat.sel.NF[,-1], y=dat.sel.NF[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
mtry=tuneRF(x=dat.sel.M[,-1], y=dat.sel.M[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
set.seed(123)
AON.rf = randomForest(AON ~., data=dat.rf.AON, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE) 
print(AON.rf)
set.seed(123)
NR.rf = randomForest(NR ~., data=dat.rf.NR, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE) 
print(NR.rf)
set.seed(123)
NOA.rf = randomForest(NOA ~., data=dat.rf.NOA, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE) 

colnames(dat.rf.M)
mtry
NF.rf = randomForest(NF ~., data=dat.rf.NF, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE) 
print(NF.rf)

set.seed(123)
M.rf = randomForest(M ~., data=dat.rf.M, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE) 

print(M.rf)
imp=data.frame(importance(M.rf))

varImpPlot(M.rf, n.var = min(30, nrow(NOA.rf$importance)), main = 'Top 30 - variable importance')

###construct a table for variable class#####
variable=data.frame(colnames(dat.rf.AON[-1])); colnames(variable)=c("variable")
class=data.frame(c(rep("soil",11),rep("climate",8),rep("vegetation",3),rep("spatial",1),rep("vegetation",24),rep("climate",19))); colnames(class)=c("class")
tab.class=cbind(variable,class)
tab.class=cbind(imp,tab.class)
tab.class$number=c(1:66)
col=pal_nejm("default",alpha=0.6)(8)

####plot importance######
p.IncNode=ggplot(data=importance_tab[1:30,], aes(x=reorder(variable,IncNodePurity), y=IncNodePurity, fill=class)) +
  geom_bar(stat="identity", color="black") +
  ylab("Increase in node purity") +
  coord_flip() +
  theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.title.x=element_text(size=10, face="bold"))+
  theme(axis.text.x = element_text(size=10, face="bold", color="black"), axis.text.y = element_text(size=10, face="bold", color="black"))+
  theme( legend.title = element_text(size = 12), legend.text = element_text(size = 10) ) +
  scale_fill_manual(values = c(col[2],col[6],col[1],col[4]))+
  theme(legend.position = c(.8,.2))

p.IncNode=ggplot(tab.class, aes(x=reorder(variable,-IncNodePurity), y=IncNodePurity, fill=class)) + 
  geom_bar(stat="identity", color="black")+
  ylab("Increase in node purity")+ 
  theme_bw()+ theme(panel.grid.minor = element_blank()) +
  xlab("")+
  scale_fill_manual(values = c(col[2],col[6],col[1],col[4]))+
  theme( legend.title = element_text(size = 12), legend.text = element_text(size = 10) )+
  theme(legend.position = "top",
        axis.text.y = element_text(size=10, face="bold", color="black"),
        axis.title.y=element_text(size=10, face="bold"),
        axis.text.x = element_text(angle=90,face="bold",size=10,
                                   hjust=1,vjust=0.5))

p.IncNode ##view plot

p.IncMSE=ggplot(tab.class, aes(x=reorder(variable,-X.IncMSE), y=X.IncMSE, fill=class)) + 
  geom_bar(stat="identity", color="black")+
  ylab("Increase in node MSE")+ 
  theme_bw()+ theme(panel.grid.minor = element_blank()) +
  xlab("")+
  scale_fill_manual(values = c(col[2],col[6],col[1],col[4]))+
  theme( legend.title = element_text(size = 12), legend.text = element_text(size = 10) )+
  theme(legend.position = "top",
        axis.text.y = element_text(size=10, face="bold", color="black"),
        axis.title.y=element_text(size=10, face="bold"),
        axis.text.x = element_text(angle=90,face="bold",size=10,
                                   hjust=1,vjust=0.5))
p.IncMSE ##view plot

set.seed(123)
otu_train.cv <- replicate(5, rfcv(dat.sel.NF[-ncol(dat.sel.NF)], dat.sel.NF$NF, cv.fold = 10, step = 1.5), simplify = FALSE)
otu_train.cv

#####Variable selection###

library(VSURF)
set.seed(123)
var.sel.AON = VSURF(AON ~., data=dat.rf.AON, na.action=na.omit,parallel = TRUE, ncores = 10)
summary(var.sel.AON)
var.sel.NOA = VSURF(NOA ~., data=dat.rf.NOA, na.action=na.omit,parallel = TRUE, ncores = 10)

var.sel.NR = VSURF(NR ~., data=dat.rf.NR, na.action=na.omit,parallel = TRUE, ncores = 10)
summary(var.sel.NR)
var.sel.NF = VSURF(NF ~., data=dat.rf.NF, na.action=na.omit,parallel = TRUE, ncores = 30)
summary(var.sel.NF)

plot(var.sel.M, step = "pred", imp.sd = FALSE, var.names = TRUE, cex.lab=1.4)
var.prd=colnames(dat.rf.M[-1])[var.sel.M$varselect.pred]
var.sel.M$varselect.pred
var.prd

var.sel.M = VSURF(M ~., data=dat.rf.M, na.action=na.omit,parallel = TRUE, ncores = 30)
summary(var.sel.M)

#####select the most important using var.prd###
dat.rf.sel=dat.sel[,var.prd]
dat.rf.sel=data.frame(M=dat.sel.M$M, dat.rf.sel)

#######try ntree############
rf_ntree = randomForest(AON ~., data=dat.rf.sel)
plot(rf_ntree)
rf_ntree = randomForest(NR ~., data=dat.rf.sel)
plot(rf_ntree)
rf_ntree_NF = randomForest(NF ~., data=dat.rf.sel)
plot(rf_ntree)
rf_ntree_NOA = randomForest(NOA ~., data=dat.rf.sel)
plot(rf_ntree_NOA)
rf_ntree_M = randomForest(M ~., data=dat.rf.sel)
plot(rf_ntree_M)
colnames(dat.rf.sel)

########try mtry###########
mtry=tuneRF(x=dat.rf.sel[,-1], y=dat.rf.sel[,1], ntreeTry=100, plot=TRUE, doBest=T) ##2:10 for diversity
mtry=mtry$mtry
mtry
set.seed(123)
m.sel = randomForest(AON ~., data=dat.rf.sel, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE, keep.inbag = T, keep.forest = T) 
print(m.sel)
m.sel.NR = randomForest(NR ~., data=dat.rf.sel, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE, keep.inbag = T, keep.forest = T) 
print(m.sel)
m.sel.NF = randomForest(NF ~., data=dat.rf.sel, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE, keep.inbag = T, keep.forest = T) 

print(m.sel.NF)
m.sel.NOA = randomForest(NOA ~., data=dat.rf.sel, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE, keep.inbag = T, keep.forest = T) 
print(m.sel.NOA)
m.sel.M = randomForest(M ~., data=dat.rf.sel, na.action=na.omit, ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE, keep.inbag = T, keep.forest = T) 
print(m.sel.M)

imp.sel=importance(m.sel.M)
imp.sel

varImpPlot(m.sel.M)

#######crossValidation############
library(rfUtilities)
rf.cv=rf.crossValidation(m.sel.M, xdata=dat.rf.sel[,2:4], p = 0.1, n = 99) ##2:10 for diversity
#dev.new(width=10, height=7.5)
plot(rf.cv, stat = "var.exp", xlab="Bootstrap iteration", ylab="Variance explained", main=NULL, cex.lab=1.4, cex.axis=1.2)

#devtools::install_github("sorhawell/forestFloor",force = TRUE)

library(forestFloor)
library(ggsci)
col=pal_npg("nrc")(9)

#######partial plot############
set.seed(123)
ff = forestFloor(m.sel, X=dat.rf.sel[2:5], binary_reg = F, calc_np=F)
Col = fcol(ff,cols=1)

###check sequence, Col###
dev.new()
plot(ff, col=Col)
imp.sel=data.frame(imp.sel)
imp.MSE=imp.sel[order(-imp.sel$X.IncMSE),]
imp.Node=imp.sel[order(-imp.sel$IncNodePurity),]
match(imp.Node$IncNodePurity, imp.MSE$IncNodePurity)
plot(ff, plot_seq=c(match(imp.Node$IncNodePurity, imp.MSE$IncNodePurity)))
plot(ff, col=alpha(col[3], 0.6), plot_seq=c(match(imp.Node$IncNodePurity, imp.MSE$IncNodePurity)), plot_GOF = T, GOF_args = list(col=col[4]), pch = 19, cex=1.2, cex.axis=1.2, main=NULL, mfrow=c(3,3))

env_A<-read.csv("env_A.csv",header=T)
colnames(env_A)
env.meta.red<-env_A[,-c(1:3,5:8)]
env.meta.red<-na.omit(env.meta.red)
colnames(env.meta.red)
mtry=tuneRF(x=env.meta.red[,-1], y=env.meta.red[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry

set.seed(123)
AON.rf <- randomForest(AON ~ soc+dem+clay+nitrogen+ph+soc+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                       data = env_A,
                       na.action = na.omit,
                       ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE)

AON.rf.pred <- predict(AON.rf, env_A[,-c(1:3,5:8)])
AON.rf.pred.10f <- NULL
for(i in 1:1785){
  AON.rf <- randomForest(AON ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                         data = env.meta.red[-i,],
                         na.action = na.omit)
  AON.rf.pred <- predict(AON.rf, env.meta.red[i,])
  AON.rf.pred.10f <- c(AON.rf.pred.10f, AON.rf.pred)
  print(i)
}

AON.ni.pred.df <- data.frame(ob = env_A$AON, 
                             pred = AON.rf.pred.10f)

plot(env.meta.red[,1],
     AON.rf.pred.10f)

library(ggplot2)
library(ggpmisc)
ggplot(AON.ni.pred.df,aes(x = ob, y = pred))+
  stat_poly_eq(
    aes(label = paste(..rr.label.., 
                      sep = "~~~")),
    label.y = "bottom", label.x = "right",
    size=4,
    parse = TRUE)+
  geom_point(size=1.5,
             alpha=0.7,
             color="#405a7f")+
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)+
  theme(aspect.ratio = 1,
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13))+
  geom_smooth(method="lm",linetype=2,size=0.8,
              color="#33325a",fill="lightgrey")+
  theme(panel.grid = element_blank())+
  xlab("")+
  ylab("")

abline(0,1)
library(modelr)
rmse(model = AON.rf,
     data = env.meta.red)
#26.62
rsquare(model = AON.rf,
        data = env.meta.red)
#0.837
imp=data.frame(importance(AON.rf))
varImpPlot(AON.rf)

#NR.rf
colnames(env_A)
env.meta.red<-env_A[,-c(1:4,6:8)]
env.meta.red<-na.omit(env.meta.red)
mtry=tuneRF(x=env.meta.red[,-1], y=env.meta.red[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
set.seed(123)
NR.rf <- randomForest(NR ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                      data = env_A,
                      na.action = na.omit,
                      ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE)
NR.rf.pred.10f <- NULL
for(i in 1:1785){
  NR.rf <- randomForest(NR ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                        data = env.meta.red[-i,],
                        na.action = na.omit)
  NR.rf.pred <- predict(NR.rf, env.meta.red[i,])
  NR.rf.pred.10f <- c(NR.rf.pred.10f, NR.rf.pred)
  print(i)
}

NR.ni.pred.df <- data.frame(ob = env_A$NR, 
                            pred = NR.rf.pred.10f)
ggplot(NR.ni.pred.df,aes(x = ob, y = pred))+
  stat_poly_eq(
    aes(label = paste(..rr.label.., 
                      sep = "~~~")),
    label.y = "bottom", label.x = "right",
    size=4,
    parse = TRUE)+
  #geom_density2d() +
  geom_point(size=1.5,
             alpha=0.7,
             color="#405a7f")+
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)+
  theme(aspect.ratio = 1,
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13))+
  geom_smooth(method="lm",linetype=2,size=0.8,
              color="#33325a",fill="lightgrey")+
  theme(panel.grid = element_blank())+
  xlab("")+
  ylab("")

rmse(model = NR.rf,
     data = env.meta.red)
rsquare(model = NR.rf,
        data = env.meta.red)
imp=data.frame(importance(NR.rf))
varImpPlot(NR.rf)

#NOA.rf
env.meta.red<-env_A[,-c(1:3,4:5,7:8)]
env.meta.red<-na.omit(env.meta.red)
mtry=tuneRF(x=env.meta.red[,-1], y=env.meta.red[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
set.seed(123)
NOA.rf <- randomForest(NOA ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                       data = env_A,
                       na.action = na.omit,
                       ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE)

rmse(model = NOA.rf,
     data = env.meta.red)
#178.71
rsquare(model = NOA.rf,
        data = env.meta.red)
#0.7832
imp=data.frame(importance(NOA.rf))
varImpPlot(NOA.rf)

NOA.rf.pred.10f <- NULL
for(i in 1:1785){
  NOA.rf <- randomForest(NOA ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                         data = env.meta.red[-i,],
                         na.action = na.omit)
  NOA.rf.pred <- predict(NOA.rf, env.meta.red[i,])
  NOA.rf.pred.10f <- c(NOA.rf.pred.10f, NOA.rf.pred)
  print(i)
}
NOA.ni.pred.df <- data.frame(ob = env_A$NOA, 
                             pred = NOA.rf.pred.10f)
ggplot(NOA.ni.pred.df,aes(x = ob, y = pred))+
  stat_poly_eq(
    aes(label = paste(..rr.label.., 
                      sep = "~~~")),
    label.y = "bottom", label.x = "right",
    size=4,
    parse = TRUE)+
  geom_point(size=1.5,
             alpha=0.7,
             color="#405a7f")+
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)+
  theme(aspect.ratio = 1,
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13))+
  geom_smooth(method="lm",linetype=2,size=0.8,
              color="#33325a",fill="lightgrey")+
  theme(panel.grid = element_blank())+
  xlab("")+
  ylab("")

##NF.rf
env.meta.red<-env_A[,-c(1:3,4:6,8)]
env.meta.red<-na.omit(env.meta.red)
mtry=tuneRF(x=env.meta.red[,-1], y=env.meta.red[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
set.seed(123)
NF.rf <- randomForest(NF ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                      data = env_A,
                      na.action = na.omit,
                      ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE)
imp=data.frame(importance(NF.rf))
varImpPlot(NF.rf)
rmse(model = NF.rf,
     data = env.meta.red)

rsquare(model = NF.rf,
        data = env.meta.red)

###M.rf
colnames(env_A)
env.meta.red<-env_A[,-c(1:3,4:6,7)]
env.meta.red<-na.omit(env.meta.red)

mtry=tuneRF(x=env.meta.red[,-1], y=env.meta.red[,1], ntreeTry=100, plot=TRUE, doBest=T)
mtry=mtry$mtry
set.seed(123)
M.rf <- randomForest(M ~ soc+dem+clay+nitrogen+ph+variance+entropy+NDVImax+Nppmax+ref+LWR+MAT+TDR+TWQ, 
                     data = env_A,
                     na.action = na.omit,
                     ntree=100, mtry=mtry, proximity=TRUE, importance=TRUE)
imp=data.frame(importance(M.rf))
varImpPlot(M.rf)

###construct a table for variable class#####
colnames(env.meta.red)
variable<-rownames(imp)

class=data.frame(c(rep("soil",1),rep("geographical",1),rep("soil",3),rep("vegetation",4),rep("climate",5)))
colnames(class)=c("class")

tab.class=cbind(variable,class)
tab.class=cbind(imp,tab.class)

tab.class$number=c(1:14)
library(ggplot2)
p.IncNode=ggplot(data=tab.class, aes(x=reorder(variable,IncNodePurity), y=IncNodePurity, fill=class)) +
  geom_bar(stat="identity", color="black") +
  ylab("Increase in node purity") +
  coord_flip() +
  theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.title.x=element_text(size=5, face="bold"))+
  theme(axis.text.x = element_text(size=5, face="bold", color="black"), axis.text.y = element_text(size=5, face="bold", color="black"))+
  theme( legend.title = element_text(size = 8), legend.text = element_text(size = 8) ) +
  theme(legend.position = c(.8,.2))

p.IncNode

library(ggsci)
p.IncMSE=ggplot(data=tab.class, aes(x=reorder(variable,X.IncMSE), y=X.IncMSE, fill=class)) +
  geom_bar(stat="identity", color="black") +
  ylab("Increase in MSE") +
  coord_flip() +
  theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.y=element_blank(), axis.title.x=element_text(size=16, face="bold"))+
  theme(axis.text.x = element_text(size=16, face="bold", color="black"), axis.text.y = element_text(size=16, face="bold", color="black"))+
  theme( legend.title = element_text(size = 16), legend.text = element_text(size = 16) ) +
  scale_fill_manual(values = c(values = c(col[2],col[6],col[1],col[4])))+
  theme(legend.position = c(.8,.2))
p.IncMSE

##predict.py

export PATH=$PATH:/public/home/bma/miniconda2/bin
source activate gdal
python

import gdal
import numpy as np
import sys
import pandas as pd

data=np.loadtxt("NOA.csv",delimiter=",")

file1=gdal.Open('New_Average_nitrogen2.tif')
band1=file1.GetRasterBand(1)
nitrogen=band1.ReadAsArray()
file1=gdal.Open('New_Average_phh2o2.tif')
band1=file1.GetRasterBand(1)
ph=band1.ReadAsArray()
file1=gdal.Open('New_Nppmax.tif')
band1=file1.GetRasterBand(1)
Nppmax=band1.ReadAsArray()
file1=gdal.Open('New_Average_clay2.tif')
band1=file1.GetRasterBand(1)
clay=band1.ReadAsArray()
file1=gdal.Open('New_wc2.1_30s_bio_10.tif')
band1=file1.GetRasterBand(1)
bio10=band1.ReadAsArray()
file1=gdal.Open('New_wc2.1_30s_bio_2.tif')
band1=file1.GetRasterBand(1)
bio2=band1.ReadAsArray()
file1=gdal.Open('New_bio22.tif')
band1=file1.GetRasterBand(1)
bio22=band1.ReadAsArray()
file1=gdal.Open('New_Variance_01_05_1km_uint32.tif')
band1=file1.GetRasterBand(1)
variance=band1.ReadAsArray()
file1=gdal.Open('New_Average_soc2.tif')
band1=file1.GetRasterBand(1)
soc=band1.ReadAsArray()
file1=gdal.Open('New_dem.tif')
band1=file1.GetRasterBand(1)
dem=band1.ReadAsArray()
file1=gdal.Open('New_NDVImax.tif')
band1=file1.GetRasterBand(1)
NDVImax=band1.ReadAsArray()

nitrogen[1,1]
-1.3382553
nitrogen[nitrogen!=-1.3382553]=1
nitrogen[nitrogen==-1.3382553]=0                         
ph[1,1]
-4.999859
ph[ph!=-4.999859]=1
ph[ph==-4.999859]=0
Nppmax[1,1]
-0.37629047
Nppmax[Nppmax!=-0.37629047]=1
Nppmax[Nppmax==-0.37629047]=0
clay[1,1]
-2.9782193
clay[clay!=-2.9782193]=1
clay[clay==-2.9782193]=0

NDVImax[1,1]
-0.49398774
NDVImax[NDVImax!=-0.49398774]=1
NDVImax[NDVImax==-0.49398774]=0
bio10[1,1]
-3.4e+38
bio10[bio10!=-3.4e+38]=1
bio10[bio10==-3.4e+38]=0
bio2[1,1]
-3.4e+38
bio2[bio2!=-3.4e+38]=1
bio2[bio2==-3.4e+38]=0
bio22[1,1]
-3.4028235e+38
bio22[bio22!=-3.4028235e+38]=1
bio22[bio22==-3.4028235e+38]=0
variance[1,1]
4294967300.0
variance[variance!=4294967300]=1
variance[variance==4294967300]=0
soc[1,1]
soc[soc!=-1.0012592]=1
soc[soc==-1.0012592]=0
dem[1,1]
dem[dem!=-0.3034016]=1
dem[dem==-0.3034016]=0

file1=gdal.Open('New_TotalNumber_Bootstrap_CoefVar.tif')
band1=file1.GetRasterBand(1)
maskocean = band1.ReadAsArray()
maskocean[np.isnan(maskocean)] = maskocean[1,1]
maskocean[maskocean==maskocean[1,1]]=0
maskocean[maskocean!=0]=1

mask=nitrogen*soc*dem*ph*Nppmax*clay*NDVImax*maskocean
data_mask=data*mask
path1='New_Average_nitrogen2.tif'
output_path='NOA.tif'
driver=gdal.GetDriverByName('GTiff')
file1=gdal.Open(path1)
geotrans=file1.GetGeoTransform()
proj=file1.GetProjection()
band1=file1.GetRasterBand(1)
file2=driver.Create(output_path,band1.XSize,band1.YSize,1,band1.DataType)
file2.SetGeoTransform(geotrans)
file2.SetProjection(proj)
band2=file2.GetRasterBand(1)
data_mask[data_mask<0]=0
Array_New=data_mask
band2.WriteArray(Array_New)
file2=None

##############################################################################
#Fig4-a
library(ggtree)
library(tidytree)
library(tidyverse)
library(ggtreeExtra)
library(ggsci)
library(ggnewscale)
library(colorspace)
sample.info <- read_csv("m1.csv")

sample.df <- sample.info %>% 
  filter(!duplicated(paste0(LandcoverClass, family))) %>% 
  select(6,9,10) %>% 
  spread(key = LandcoverClass, value = 3)

pathway.df <- sample.info %>% 
  filter(!duplicated(paste0(Pathway, family))) %>% 
  select(1,6) %>% 
  spread(key = Pathway, value = 1)

taxa.df  <- sample.info %>% 
  filter(!duplicated(family)) %>% 
  select(2:6) 

tree.file <- read.tree("tof.tre")

tree.df <- tree.file  %>% 
  as_tibble() %>%
  full_join(taxa.df, by = c("label" = "family")) %>%
  full_join(pathway.df, by = c("label" = "family")) %>%
  full_join(sample.df, by = c("label" = "family")) %>%
  as.treedata()

edge.colors <- rep(alpha("black", alpha = .25), 3327)
edge.colors[tree.df@data$kingdom == "Archaea"] <-  c("#F14140")
edge.colors[tree.df@data$kingdom == "Bacteria"] <- c("#86CFA8")

tree.df2 <- tibble(
  node = 1:3327,
  color = edge.colors
)
library(ggsci)
gptree <- ggtree(
  tree.df,
  layout = "fan",
  ladderize = TRUE,
  open.angle = 20,
  size = .1) %<+% 
  tree.df2 +  
  aes(color = I(color)) +
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = .1,
    mapping = aes(
      y = tip,
      fill = AON,
      color = AON
    ))+
  scale_color_manual(values=pal_npg(alpha = 0.8)(5)[1],na.value  ="white")+
  scale_fill_manual(values=pal_npg(alpha = 0.8)(5)[1],na.value  ="white")+
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = .1,
    mapping = aes(
      y = tip,
      fill = NR,
      color = NR
    ))+
  scale_color_manual(values = pal_npg(alpha = 0.8)(5)[2],na.value  ="white")+
  scale_fill_manual(values = pal_npg(alpha = 0.8)(5)[2],na.value  ="white")+
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = .1,
    mapping = aes(
      y = tip,
      fill = NOA,
      color = NOA
    ))+
  scale_color_manual(values = pal_npg(alpha = 0.8)(5)[4],na.value  ="white")+
  scale_fill_manual(values = pal_npg(alpha = 0.8)(5)[4],na.value  ="white")+
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = .1,
    mapping = aes(
      y = tip,
      fill = NF,
      color = NF
    ))+
  scale_color_manual(values = pal_npg(alpha = 0.8)(5)[3],na.value  ="white")+
  scale_fill_manual(values =pal_npg(alpha = 0.8)(5)[3],na.value  ="white")+
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = .1,
    mapping = aes(
      y = tip,
      fill = M,
      color = M
    ))+
  scale_color_manual(values = pal_npg(alpha = 0.8)(5)[5],na.value  ="white")+
  scale_fill_manual(values = pal_npg(alpha = 0.8)(5)[5],na.value  = "white")+
  geom_fruit(
    geom = geom_bar,
    pwidth = 0.1,
    offset = .1,
    orientation = "y",
    stat = 'identity',
    color = "#7F9793",
    #axis.params = list(title = "Domain",size = 4),
    mapping = aes(
      y = tip,
      x = Grassland
    ))+
  geom_fruit(
    geom = geom_bar,
    pwidth = 0.1,
    offset = .1,
    orientation = "y",
    stat = 'identity',
    color = "#367168",
    mapping = aes(
      y = tip,
      x = Forest
    ))+
  geom_fruit(
    geom = geom_bar,
    pwidth = 0.1,
    offset = .1,
    orientation = "y",
    stat = 'identity',
    color = "#586384",
    mapping = aes(
      y = tip,
      x = Wetland
    ))+
  geom_fruit(
    geom = geom_bar,
    pwidth = 0.1,
    offset = .1,
    orientation = "y",
    stat = 'identity',
    color = "#C17360",
    mapping = aes(
      y = tip,
      x = `Agricultural land`
    ))+
  new_scale_color()+new_scale_fill()+
  geom_fruit(
    geom = geom_tile,
    pwidth = 0.1,
    offset = 0.1,
    mapping = aes(
      y = tip,
      fill = fct_lump(phylum,30),
      color = fct_lump(phylum,30)
    ))+
  scale_color_manual(values =myclor)+
  scale_fill_manual(values = myclor)

library(paletteer)
palettes_d_names

write.csv(tree.df@data,"taxa_df.csv")

library(paletteer)
d_palettes <- palettes_d_names
mycolor<-paletteer_d("pals::polychrome", n = 31)

devtools::install_github("caleblareau/BuenColors")

ggsave(gptree,
       filename = "tree311.pdf",
       height = 10,
       width = 13)
install.packages("colourpicker")
library(colourpicker)
library(BuenColors)
colourpicker:::colourPickerAddinc("#FFE7BA", "#EED8AE", "#CDBA96", "#8B7E66", "#D8BFD8", "#FFE1FF", "#EED2EE", "#8B7B8B", "#FA8072", "#FF8C69", "#EE8262", "#8B4C39", "#B0E0E6", "#AEEEEE", "#96CDCD", "#668B8B", "#4682B4", "#63B8FF", "#5CACEE", "#4F94CD", "#36648B", "#FFF5EE", "#EEE5DE", "#CDC5BF", "#8B8682", "#CDC9C9", "#FFB6C1", "#FFAEB9", "#EEA2AD", "#CD8C95", "#8B5F65")
colourpicker:::colourPickerAddin("#FFE7BA", "#EED8AE", "#CDBA96", "#8B7E66", "#D8BFD8", "#FFE1FF", "#EED2EE", "#8B7B8B", "#C6E2FF", "#B9D3EE", "#9FB6CD", "#6C7B8B", "#FFC0CB", "#FFB5C5", "#EEA9B8", "#8B636C", "#EEDD82", "#FFEC8B", "#EEDC82", "#CDBE70", "#FA8072", "#FF8C69", "#CD7054", "#8B4C39", "#FFE4E1", "#EED5D2", "#CDB7B5", "#8B7D7B", "#CDCDC1", "#EEE9BF", "#CDC9C9")
myclor<-c("#FFE7BA", "#EED8AE", "#CDBA96", "#8B7E66", "#D8BFD8", "#FFE1FF", "#EED2EE", "#8B7B8B", "#FA8072", "#FF8C69", "#EE8262", "#8B4C39", "#B0E0E6", "#AEEEEE", "#96CDCD", "#668B8B", "#4682B4", "#63B8FF", "#5CACEE", "#4F94CD", "#36648B", "#FFF5EE", "#EEE5DE", "#CDC5BF", "#8B8682", "#CDC9C9", "#FFB6C1", "#FFAEB9", "#EEA2AD", "#CD8C95", "#8B5F65")
a <- as.data.frame(table(taxa.df[,c(1,2)]))

##Fig4-b
library(ggtree)
library(tidytree)
library(tidyverse)
library(ggtreeExtra)
library(ggsci)
library(ggnewscale)
library(colorspace)

sample.info <- read_csv("m1.csv")

sample.AON <- sample.info[(sample.info$Pathway=="AON"),]
sample.NR <- sample.info[(sample.info$Pathway=="NR"),]
sample.NOA <- sample.info[(sample.info$Pathway=="NOA"),]
sample.NF <- sample.info[(sample.info$Pathway=="NF"),]
sample.M <- sample.info[(sample.info$Pathway=="M"),]

sample.Ag <- sample.info[(sample.info$LandcoverClass=="Agricultural land"),]
sample.W <- sample.info[(sample.info$LandcoverClass=="Wetland"),]
sample.F <- sample.info[(sample.info$LandcoverClass=="Forest"),]
sample.G <- sample.info[(sample.info$LandcoverClass=="Grassland"),]

taxa.AON<- aggregate(abundance~Pathway+kingdom+phylum,sample.AON,sum)
taxa.NR<- aggregate(abundance~Pathway+kingdom+phylum,sample.NR,sum)
taxa.NOA<- aggregate(abundance~Pathway+kingdom+phylum,sample.NOA,sum)
taxa.NF<- aggregate(abundance~Pathway+kingdom+phylum,sample.NF,sum)
taxa.M<- aggregate(abundance~Pathway+kingdom+phylum,sample.M,sum)
taxa.W<- aggregate(abundance~LandcoverClass+kingdom+phylum,sample.W,sum)
taxa.Ag<- aggregate(abundance~LandcoverClass+kingdom+phylum,sample.Ag,sum)
taxa.F<- aggregate(abundance~LandcoverClass+kingdom+phylum,sample.F,sum)
taxa.G<- aggregate(abundance~LandcoverClass+kingdom+phylum,sample.G,sum)

df_G <- taxa.G %>% 
  group_by(phylum) %>% 
  summarise(hl:=sum(abundance)) %>%
  mutate(pct_hl:=round(hl/sum(hl)*100,1)) %>%
  arrange(desc(pct_hl)) 

df_5<-df_G[c(1:5),]
others<-colSums(df_F[6:nrow(df_G),2:ncol(df_G)])
others<-as.data.frame(t(others))
others$phylum<-"others"
sumall<-rbind(df_5,others)

colors<-c("#4E5085","#1C7FAB","#7DCBC9","#CDD3AC","#F9E7D1","#EDD885")
library(ggfittext)
ggplot(sumall, aes(x=1, y=pct_hl, fill=reorder(phylum,pct_hl),label=phylum)) +
  geom_col(position=position_stack()) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  coord_polar(theta="y") +
  xlim(c(-0.5, 2)) +
  theme_void()+
theme(legend.position = "right",
      text = element_text(family='mono', color='white'),
      plot.title = element_text(family='Roboto-Thin', size=20, hjust=0.5),
      plot.margin = unit(c(1,2,1,2), 'cm'))

##############################################################################
##Fig5-a
nitrogen_info<-read.csv("nitrogen_info1.csv")
nitrogen_info$type<-factor(nitrogen_info$type,levels=c("Grassland","Forest","Wetland","Agricultural land"))
library(data.table)
library(ggcorrplot)
library(colorspace)
library(Hmisc)
library(vegan)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(ggpubr)

p=nitrogen_info %>% ggplot(aes(x=NIRI,y=TN,color=type,fill=type))+
  geom_point(size=3,alpha=0.5,aes(color=type))+
  geom_smooth(method = "lm", formula = y~x,linetype=2)+ 
  theme_bw()+
  theme(
    panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )+
  stat_cor(method="pearson")+
  scale_fill_manual(values=c("#367168","#7F9793","#586384","#C17360"))+
  scale_color_manual(values=c("#367168","#7F9793","#586384","#C17360"))+
  xlab("NIRI")+
  ylab("TN")
