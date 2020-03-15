set.seed(19970501)
library(ggplot2)
library(gridExtra)
library(mice)
library(xtable)
library(lattice)
library(splines)
############################## Load data 
titanic <- read.csv(
  "http://www.math.ku.dk/~susanne/titanic.txt",
  header = TRUE,
  colClasses = c("factor", "integer","factor", "numeric", "integer", "integer"))


############################## Prelimenary inspection of data
head(titanic)
summary(titanic)  
#We note 263 missing values of age.
#Also some small values but not really a problem


############################## Treatment of age variable
#Transform to squaroot of age (Lauritzens golden rule of thumb)
titanic["sqrtage"] <- sqrt(titanic$age) 
titanic <- subset(titanic, select = c("pclass","survived", "sex", "sibsp","parch","sqrtage"))

#Consider dataset of observations missing age.
titanicmiss <- subset(titanic, is.na(titanic$sqrtage), 
                      select = c("pclass", "survived", "sex", "sibsp", "parch"))


############################## Plot marginal distributions
tmp1 <- lapply(names(titanic), function(x)  
  ggplot(data = titanic[,x, drop = F]) +
    aes_string(x) + xlab(x) + ylab(""))   #library(ggplot2)
tmp2 <- lapply(names(titanicmiss), function(x)
  ggplot(data = titanic[,x, drop = F]) +
    aes_string(x) + xlab(x) + ylab(""))
gd <- geom_density(adjust = 2, fill = gray(0.5))
gb <- geom_bar(fill = gray(0.5))

#Following code prints marginal distributions for entire dataset.
#Change the list to get other marginal distributions.
grid.arrange( #library(gridextra)
  grobs = list(
    tmp1[[1]] + gb,
    tmp1[[2]] + gb,
    tmp1[[3]] + gb,
    tmp1[[4]] + gb,
    tmp1[[5]] + gb,
    tmp1[[6]] + gd),
  nrow = 2
) #Observe that marginal distributions for age, siblings and parch are quite skew



############################## Imputations of missing values:
titanicMice <- mice(titanic, m=5, maxit = 40) #library(mice)
titanicImputed <- complete(titanicMice,5)


############################## Plot spearman correlations
#define corelation matrices
cp1 <- cor(data.matrix(na.omit(titanic)), method = "spearman")
cp2 <- cor(data.matrix(na.omit(titanicmiss)), method = "spearman")
cp3 <- cor(data.matrix(na.omit(titanicImputed)), method = "spearman")

#plot function
lp <- function(cp,ord){
  colPal <- colorRampPalette(c("red", "cyan"), space = "rgb")(100)
  levelplot(cp[ord, ord],  
            xlab = "", 
            ylab = "",
            col.regions = colPal, 
            at = seq(-1, 1, length.out = 100),
            colorkey = list(space = "top", labels = list(cex = 1.5)),
            scales = list(x = list(rot = 45), 
                          y = list(draw = FALSE),
                          cex = 1.2)
  )
}  #library(lattice)
#Following code prints spearman correlations titanic data set and
# for titanicmiss dataset. Can easily be changed to other correlations. 
grid.arrange(lp(cp1,colnames(titanic)),
             lp(cp2,colnames(titanicmiss)),ncol=2)


############################## Logistic regression moddelling
#Main effects
main.start <- glm(survived~., family = binomial(link = logit), data=titanicImputed)
summary(main.start) #We decide to drop parch
xtable(main.start) #converts summary to TeX code. library(xtable)

#Main effects without parch
M1 <- glm(survived~.-parch, family = binomial(link = logit), data=titanicImputed)
summary(M1)
xtable(M1)

#Model diagnostics plots:
resids <- function(glmmod){
  p1 <- qplot(fitted(glmmod), residuals(glmmod), alpha =I(.5)) + 
    geom_smooth() + xlab("Fitted values") + ylab("Residuals")
  p2 <- qplot(residuals(glmmod), geom = "histogram") + 
    xlab("residuals")
  p3 <- qplot(sample=rstandard(glmmod), geom = "qq") + geom_abline(slope=1, intercept = 0) +
    xlab("QQ-plot")
  grid.arrange(p1,p2,p3,ncol=3)
}
simplots <- function(mod){
  yNew1 <- simulate(mod)[,1]
  yNew2 <- simulate(mod)[,1]
  yNew3 <- simulate(mod)[,1]
  yNew4 <- simulate(mod)[,1]
  simGlmNew1 <- glm(yNew1~pclass+sex+sqrtage+sibsp, family = "poisson"
                    ,data = titanicImputed)
  simGlmNew2 <- glm(yNew2~pclass+sex+sqrtage+sibsp, family = "poisson"
                    ,data = titanicImputed)
  simGlmNew3 <- glm(yNew3~pclass+sex+sqrtage+sibsp, family = "poisson"
                    ,data = titanicImputed)
  simGlmNew4 <- glm(yNew4~pclass+sex+sqrtage+sibsp, family = "poisson"
                    ,data = titanicImputed)
  simDiagNew1 <- fortify(simGlmNew1)
  simDiagNew2 <- fortify(simGlmNew2)
  simDiagNew3 <- fortify(simGlmNew3)
  simDiagNew4 <- fortify(simGlmNew4)
  p1 <- qplot(.fitted, .resid, data = simDiagNew1) +
    geom_smooth()
  p2 <- qplot(.fitted, .resid, data = simDiagNew2) +
    geom_smooth()
  p3 <- qplot(.fitted, .resid, data = simDiagNew3) +
    geom_smooth()
  p4 <- qplot(.fitted, .resid, data = simDiagNew4) +
    geom_smooth()
  grid.arrange(p1,p2,p3,p4,ncol=4)
}
resids(M1)  #residual plots
simplots(M1)  #simulate under model 2 and generate residual plots

#All interactions, very big model
M2 <-glm(survived ~ pclass*sex*sibsp*sqrtage,family = binomial(link = logit), data=titanicImputed)
summary(M2)

#Interaction between pclass and sex:
M3 <- glm(survived~ pclass*sex+sqrtage+sibsp
            , family = binomial(link = logit), data=titanicImputed)
summary(M3)
resids(M3)
xtable(M3)

#Model with splines on age variable
#library(splines)
M4 <- glm(survived~pclass+sex+ns(sqrtage,df=2)+sibsp
                , family = binomial(link = logit), data=titanicImputed)
summary(M4)
resids(M4)
xtable(M4)

#Model with splines and interaction
M5 <- glm(survived~pclass*sex+ns(sqrtage,df=2)+sibsp
            , family = binomial(link = logit), data=titanicImputed)
summary(M5)
resids(M5)

#One could try making pclass binary, but it leads to nothing:
#titanic3 <- titanicImputed
#titanic3["binarypclass"] <-ifelse(titanic3$pclass=="3", 1,0)

############################## Crossvalidating prediction accuracy of models:
cross_val <- function(reps,model,dataset){  
  ans <- rep(0,reps)
  for (i in 1:reps) {
    k <- nrow(dataset)
    train = sample(1:k, k*0.8)
    test = (-train)
    dataset_train = dataset[train,] 
    dataset_test = dataset[test,]
    glmodel = glm(formula = model, 
                  family = binomial(link = "logit"), 
                  data = dataset_train)  #Insert exponential family
    fitted.results <- predict(glmodel,dataset_test, type='response')
    fitted.results <- ifelse(fitted.results > 0.5,1,0)
    misClasificError <- mean(fitted.results != dataset_test$survived)
    ans[i] = misClasificError
  }
  return(1-mean(ans))
}
##See estimated prediciton accuracy of models
cross_val(reps=100,model=M1,dataset =titanicImputed)
cross_val(reps=100,model=M2,dataset =titanicImputed)
cross_val(reps=100,model=M3,dataset =titanicImputed)
cross_val(reps=100,model=M4,dataset =titanicImputed)

############################## CI intervals and odds 

1/exp(mod2$coefficients["sexmale"])
1/exp(mod2$coefficients["pclass3"])
1/exp(mod2$coefficients["pclass2"])
1/exp(mod2$coefficients["sexmale"]+mod2$coefficients["pclass3"])
1/exp(confint(mod2))[-1,]



