library(metafor)

## Open csv file and see categories of the variables - Example of dataset in Github
my_data <- read.csv("~/Desktop/Metanalise_denise_codigos/extracao_TC_lucas.csv", header=TRUE)
sapply(my_data, mode)

### Change categorical variables
transform(my_data, mean_controls = as.numeric(mean_controls), 
          mean_patients = as.numeric(mean_patients))

###If need to deleto some line (specific study)
my_data <- extracao_imc[-c(6, 10, 11, 15, 19, 21, 22), ]

# To calculate SMD
## escalc() function to calculate effect sizes and their variance.
my_data <- escalc(n1i = n_patients, n2i = n_controls, m1i = mean_patients, m2i = mean_controls, sd1i = sd_patients, sd2i = sd_controls, data = my_data, measure = "SMD", append = TRUE)

## Fixed model - method="FE", randon model - method="DL" OR "RE"
ma_model_1 <- rma(yi, vi, data = my_data)
summary(ma_model_1)

## 
## escalc() function to calculate effect sizes and their variance.
my_data <- escalc(n1i = n_controls, n2i = n_patientsC, m1i = mean_controls, m2i = mean_patientsC, sd1i = sd_controls, sd2i = sd_patientsC, data = my_data, measure = "SMD", append = TRUE)
ma_model_1 <- rma(yi, vi, data = my_data)
summary(ma_model_1)

## escalc() function to calculate effect sizes and their variance.
my_data <- escalc(n1i = n_controls, n2i = n_patientsO, m1i = mean_controls, m2i = mean_patientsO, sd1i = sd_controls, sd2i = sd_patientsO, data = my_data, measure = "SMD", append = TRUE)
## Fixed model - method="FE", randon model - method="DL" OR "RE" 
ma_model_1 <- rma(yi, vi, method="RE", data = my_data)
summary(ma_model_1)

## florest plot
forest(ma_model_1, showweights=TRUE, slab = paste(my_data$name, as.character(my_data$year), sep = ", "))
## margin fix
par(mar=c(4,4,1,2))
### add column headings to the plot
## primeiro numero é horizontal, segundo vertical
text(-5, 17, "Author(s) and Year", pos=1)
text(2.1, 17, "Weight", pos=1)
text(3.7, 17, "Rel. Risk [95% CI]", pos=1)
text(-3.1, -0.45, "(heterogeneity I2 =83.47%, p=<0.001)", pos=1)
## funnel plot 
funnel(ma_model_1)

##egger test
regtest(ma_model_1, model = "lm")

#To calculate OR
## compute the log odds ratios (and corresponding sampling variance) for each study -- ci e di são controles
dat1 <- escalc(measure="RR", ai=controlspo, bi=controlsne, ci=patientspo2, di=patientsne2, data= my_data)
dat1

## random-effects model

res1 <- rma(yi, vi, data=dat1)
res1

##  log odds ratio -- back-transform the results
predict(res1, transf=exp, digits=2)

##forest plot
forest(res1, showweights=TRUE, atransf=exp, slab = paste(my_data$name, as.character(my_data$year), sep = ", "))
### add column headings to the plot
text(-4.7, 4.5, "Author(s) and Year", pos=1)
text(1.7, 4.5, "Weight", pos=1)
text(3.2, 4.5, "Rel. Risk [95% CI]", pos=1)
text(-2.0, -0.8, "(heterogeneity I2 =5.21%, p=0.50)", pos=1)
## to save use 845 x 677
## margin fix
par(mar=c(4,4,1,2))

## funnel plot 
funnel(res1)
