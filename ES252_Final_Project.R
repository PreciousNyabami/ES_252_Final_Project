install.packages(c(
  'pls',
  'corrplot', 'tidyr',
  'gridExtra', 'ggrepel'
))

# Load libraries 
library(pls)
library(corrplot)
library(tidyr)
library(tidyverse)
library(gridExtra)
library(ggrepel)

# the data
data2<- read_csv('/Users/pnyabami2/Desktop/Rye_4.csv')
plsr_clean <- data2 %>% na.omit()

#Subsetting the data
evi<- plsr_clean$sampling_EVI
ndvi<- plsr_clean$sampling_NDVI
lai<- plsr_clean$sampling_LAI
savi<- plsr_clean$preplant_SAVI

nitrogen<- plsr_clean$N_g_Kg
carbon<- plsr_clean$C_g_Kg
biomass<- plsr_clean$Biomass_ton_acre
treatment<- plsr_clean$cover_crop
plantheight<- plsr_clean$CC_height_cm
groundcover<- plsr_clean$`Ground cover`

slope<- plsr_clean$Slope
sand<- plsr_clean$Sand
clay<- plsr_clean$Clay
silt<- plsr_clean$Silt
soc<- plsr_clean$SOC_g_Kg
sn<- plsr_clean$TN_g_Kg
plot<- plsr_clean$id

newdata<- data.frame(treatment, plot, biomass, carbon, nitrogen, evi, ndvi, lai, savi,plantheight, groundcover,
                     slope, sand, clay, silt, soc, sn)
view(newdata)


# Notes to keep in mind
      ## plsr model
      # you need atleast 40 samples to do BIC
      # lOO in plsr for smaller data sets

# Trial of Plsr
model2<- plsr (biomass~ ndvi+lai+savi, data= newdata, scale= TRUE, validation = "LOO")
summary(model2) # looks like savi is not really needed

validationplot(model2)
validationplot(model2, val.type="MSEP")
validationplot(model2, val.type="R2") # 2 indices look like they would be enough

# A correlation matrix to understand our predictors

x_v<- c('evi', 'ndvi', 'lai', 'savi')
y_v<- c('biomass', 'nitrogen', 'carbon')

## scaling that is important for correlation
x_matrix <- newdata %>%
  select(all_of(x_v)) %>%
  as.matrix()

x_scaled <- scale(x_matrix)

cor_matrix <- cor(x_matrix, use = 'complete.obs')

png('Correlation_Matrix.png',
    width = 10, height = 10,
    units = 'in', res = 300)

corrplot(
  cor_matrix,
  method      = 'color',
  type        = 'upper',
  addCoef.col = 'black',
  number.cex  = 0.7,
  tl.cex      = 0.8,
  tl.col      = 'black',
  col         = colorRampPalette(
    c('#FF0000','#FFFFFF','#006400'))(200),
  #title       = 'Predictor Correlation Matrix',
  mar         = c(0,0,2,0)
)
dev.off()

# Let us do pearson correlations first
# check on the mixtures vs monocultures
rye_data <- newdata %>%
  filter(treatment == 'Rye')

mixture_data<- newdata %>%
  filter(treatment == 'Mixtures')


# Rye
cor_results_rye <- data.frame()

for(y in y_v) {
  for(x in x_v) {
    
    test <- cor.test(
      rye_data[[x]],
      rye_data[[y]],
      method = 'pearson'
    )
    
    cor_results_rye <- rbind(cor_results_rye, data.frame(
      predictor   = x,
      response    = y,
      r           = round(test$estimate,   3),
      r_squared   = round(test$estimate^2, 3),
      p_value     = round(test$p.value,    4),
      df          = test$parameter,
      significant = ifelse(
        test$p.value < 0.05, 'Yes *', 'No')
    ))
  }
}

print(cor_results_rye)

# Mixture
cor_results_mix <- data.frame()

for(y in y_v) {
  for(x in x_v) {
    
    test <- cor.test(
      mixture_data[[x]],
      mixture_data[[y]],
      method = 'pearson'
    )
    
    cor_results_mix <- rbind(cor_results_mix, data.frame(
      predictor   = x,
      response    = y,
      r           = round(test$estimate,   3),
      r_squared   = round(test$estimate^2, 3),
      p_value     = round(test$p.value,    4),
      df          = test$parameter,
      significant = ifelse(
        test$p.value < 0.05, 'Yes *', 'No')
    ))
  }
}

print(cor_results_mix)

# PLSR rero
  # All predictors but choosing the lowest CV
      #1. Indexes alone for monocultures

model1rye<- plsr (biomass~ evi+ndvi+lai+savi, data= rye_data, scale= TRUE, validation = "LOO")
summary(model1rye)##

model2rye<- plsr (biomass~ ndvi, data= rye_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model2rye) 

model3rye<- plsr (nitrogen~ evi+ndvi+lai+savi, data= rye_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model3rye) 

model4rye<- plsr (nitrogen~ ndvi, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model4rye)

model5rye<- plsr (carbon~ evi+ndvi+lai+savi, data= rye_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model5rye) 

model6rye<- plsr (carbon~ ndvi, data= rye_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model6rye) 

       #2.indexes for mixture

model1mixture<- plsr (biomass~ evi+ndvi+lai, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model1mixture) 

model2mixture<- plsr (biomass~ ndvi, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model2mixture)

model3mixture<- plsr (carbon~ evi+ndvi+lai, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model3mixture)

model4mixture<- plsr (carbon~ ndvi, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model4mixture)

model5mixture<- plsr (nitrogen ~ evi+ndvi+lai, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model5mixture)

model6mixture<- plsr (nitrogen ~ ndvi, data= mixture_data, scale= TRUE, validation = "LOO") # lOO for smaller data sets
summary(model6mixture)
    # 3. Adding addition plant parameters: environmental covariates

                 #1. monocultures
model5m<- plsr (biomass~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model5m) 

model6m<- plsr (carbon~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model6m) 

model6m<- plsr (nitrogen~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model6m) # the lowest cv is desired. This doesnot improve model performance

                #4. Mixture
model7<- plsr (biomass~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model7) 

model8<- plsr (nitrogen~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model8) 

model9<- plsr (carbon~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model9) 

# 4adding addition plant parameters: environmental covariates, field properties

                                  #1. Monoculture
model10<- plsr (biomass~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay + slope , data= rye_data, scale= TRUE, validation = "LOO") 
summary(model10)

model11<- plsr (carbon~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay + slope, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model11)

model12<- plsr (nitrogen~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay + slope, data= rye_data, scale= TRUE, validation = "LOO") 
summary(model12)

                                     #2. Rye mixture
model13<- plsr (biomass~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay+ slope, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model13)

model14<- plsr (carbon~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay + slope, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model14)

model15<- plsr (nitrogen~ ndvi+ evi+ lai+ savi+ soc + sn + plantheight + silt + sand + 
                  clay + slope, data= mixture_data, scale= TRUE, validation = "LOO") 
summary(model15)



















  
  




