library(data.table)
library(wbstats)
library(countrycode)
library(OECD)
library(tidyverse)
library(ggplot2)
library(knitr)
library(fBasics)
library(stargazer)
library(lme4)
library(arm)
library(car)
library(readxl)
library(ggthemes)
library(lmtest)
library(sandwich)
library(jtools)
library(sjPlot)
library(lme4)
library(huxtable)
library(lattice)


#### Functions ####

# We get in  data from worldbank, rename the value column and set it to a data.table
# It defaults to using data from 2015. it backfills missing data!

wb_data <- function(db = "", name="", date=2015, history=0){
  temp <- wb(indicator = db, startdate = date-history, enddate = date, mrv=10, gapfill=T)
  setDT(temp)
  setnames(temp, "value", name)
  if (history>0){
    temp2 <- spread(temp, date, name)
    setDT(temp2)
    temp2[, (paste0(name, "_change")) := as.integer(get(as.character(date)) - get(as.character(date-history)))]
    temp2
  } else{temp}
}


# Make residual plot:
make_residual_plot <- function(data, model, outcome_variable){
  full_data <- data[complete.cases(data)]
  original <- full_data[, outcome_variable, with=FALSE][[1]]
  print(original)
  resid <-  residuals(model)
  return(residual.plot(original, resid))
}

rmse <- function(fitted, y){
  return(sqrt(mean((y-fitted)^2)))
}


theme_set(theme_economist())
#### Data preparation ####
prepare_fresh_data <- function(){ 
  oecd_available <- get_datasets()

  # Prepare  dl data afresh
  # dl per country data generated from 10.21942/uva.12330959
  #temp <- tempfile()
  #download.file("https://uvaauas.figshare.com/ndownloader/files/22735070",temp)
  #downloads_latlong <- fread(unz(temp, "all_dl_latlong.csv"))
  #unlink(temp)
  #country_dl<-downloads_latlong %>%
  #            dplyr::count(country)%>%
  #            as.data.frame() %>%
  #            dplyr::rename(country_name=country, dls=n)%>%
  #            drop_na(country_name) %>%
  #            dplyr::filter(country_name!="")%>%
  #            mutate(iso2c = countrycode(country_name, origin = "country.name", destination = "iso2c"))%>%
  #            mutate(iso3c = countrycode(country_name, origin = "country.name", destination = "iso3c")) %>%
  #            drop_na()
  #write.csv(country_dl, file="data-raw/country_download.csv")
  
  #or load the prepared version
  count_country_day <- read.csv("data-raw/country_download.csv")
  
  # Loading the different datasets. 
  which_date <- 2015
  population <- wb_data("SP.POP.TOTL", "population")%>%dplyr::filter(date==which_date)
  gdp <- wb_data("NY.GDP.PCAP.PP.CD", "gdp")%>%dplyr::filter(date==which_date)
  internet <- wb_data("IT.NET.BBND", "internet")%>%dplyr::filter(date==which_date)
  literacy <- wb_data("SE.ADT.LITR.ZS", "literacy")%>%dplyr::filter(date==which_date)
  rd <- wb_data("GB.XPD.RSDV.GD.ZS", "rd")%>%dplyr::filter(date==which_date)
  tertiary <- wb_data("SE.TER.ENRR", "tertiary")%>%dplyr::filter(date==which_date)
  #save(population, gdp, internet, literacy, rd, tertiary, file="data-raw/worldbank_data_20200525.rda")
  
  
  # Add different variables to a new data.table, don't pollute the first one
  complete_df <- merge(count_country_day, population[, c("population", "iso3c")], all.x=T)
  complete_df <- merge(complete_df, gdp[, c("gdp", "iso3c")], all.x=T)
  complete_df <- merge(complete_df, internet[, c("internet", "iso3c")], all.x=T)
  complete_df <- merge(complete_df, tertiary[, c("tertiary", "iso3c")], all.x=T) # Number of enrolled
  complete_df <- merge(complete_df, rd[, c("rd", "iso3c")], all.x=T)
  complete_df <- merge(complete_df, literacy[, c("literacy", "iso3c")], all.x=T)
  
  # I use population in Ms, because then the DL/POP will be a bigger number --> better to see effects in  model
  complete_df <-complete_df%>%
    dplyr::mutate(pop_per_mil = population/1000000)%>%
    dplyr::mutate(dl_per_pop = dls/pop_per_mil)%>%
    dplyr::mutate(dl_per_pop_round=round(dl_per_pop))%>%
    dplyr::mutate(internet_per_pop = internet/population)%>%
    dplyr::mutate(continent =  countrycode(iso3c, 'iso3c', 'continent'))%>%
    dplyr::mutate(continent = ifelse((iso3c == "CAN" | iso3c == "USA"),  "North America",continent))
  
  
  #gov spending on education
  expenditure <- read_csv("data-raw/gov_spending_edu.csv")

  complete_df <- merge(complete_df, expenditure[, c("exp_tertiary_pstudent", "iso3c")], all.x=T) # Expennditure per student
  
  
  spending_education <- fread("data-raw/public_spending_education_oecd.csv") %>%
    dplyr::rename(spending_education=Value, iso3c=LOCATION) %>%
    dplyr::filter(SUBJECT=="TRY")
  
  complete_df <- merge(complete_df, spending_education[, c("spending_education", "iso3c")], all.x=T)   
  
  
  #add citation index
  citation <- read_excel("data-raw/scimagojr_2015.xlsx") %>%
    dplyr::rename(h_index="H index") %>%
    dplyr::mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))%>%
    dplyr::rename_all(tolower) 
    
  
  complete_df<- merge(complete_df, citation, all.x=T)
  
  #save(complete_df, file = "data/global_data.rda")
  
}

#to prepare data fresh, uncomment the next line:
#prepare_fresh_data()

#read data file
load(file = "data/global_data_20200525.rda")


# Plot the density of the dl/pop with built in functions
plot(density(complete_df$dl_per_pop, na.rm=T)) 

# Use ggplot for the same
density_plot <- ggplot() + geom_density(aes(complete_df$dl_per_pop), data=complete_df) + labs(x="Downloads per population(in millions)", title="Density")
density_plot



# Density plot with region

count_country_plot <- transform(complete_df, iso2c = reorder(iso2c, -dl_per_pop))
barchart_plot_continent <- ggplot() + geom_bar(aes(iso2c, dl_per_pop, fill=continent),
                                              data=count_country_plot[count_country_plot$dl_per_pop > 1000 & count_country_plot$continent!="",], stat="identity") +
  labs(x="Countries", y="Downloads per population (in millions)", title="Downloads") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))

barchart_plot_continent


# Boxplot
bplot <- ggplot() + geom_boxplot(aes(x=continent, y=dl_per_pop), data=complete_df)
bplot

#### First model: DL/capita ~ population + gdp + internet_per_pop ####

m1 <- glm(formula = dl_per_pop ~ log(pop_per_mil) + log(gdp) + internet_per_pop, data=complete_df)

cor(complete_df$gdp, complete_df$internet_per_pop, use="complete.obs" )
sqrt(vif(m1))
summary(m1)

par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(1,1))


m1.pois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) + log(gdp) + internet_per_pop, data=complete_df, family = "poisson")
summary(m1.pois)
sqrt(vif(m1.pois))
coeftest(m1.pois, vcov = sandwich)

m1.qpois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) + log(gdp) + internet_per_pop, data=complete_df, family = "quasipoisson")


export_summs(m1, m1.pois,m1.qpois,digits=3, number_format="%.3g",
             statistics = c(N = "nobs", "Null deviance"="null.deviance", "res.deviance"="deviance")) 



# R&D, tertiery education, h_index models

#without interaction
m2.qpois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) + log(gdp) +
                  internet_per_pop + tertiary + exp_tertiary_pstudent + rd + h_index,
                data=complete_df, family = "quasipoisson")
summary(m2.qpois)
#without  gpd

m2.b.qpois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) +
                  tertiary + exp_tertiary_pstudent + rd + h_index,
                data=complete_df, family = "quasipoisson")

m2.b.qpois_small <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) +
                    exp_tertiary_pstudent *rd ,
                  data=complete_df, family = "quasipoisson")
summary(m2.b.qpois)

#with interaction
m2.c.qpois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) +
                    tertiary + exp_tertiary_pstudent + rd + h_index + tertiary*rd,
                  data=complete_df, family = "quasipoisson")
summary(m2.c.qpois)
m2.d.qpois <- glm(formula = dl_per_pop_round ~ log(pop_per_mil) +
                    tertiary*rd+ log(gdp),
                  data=complete_df, family = "quasipoisson")

summary(m2.d.qpois)
plot(m2.d.qpois)

m2.qpois_small <- glm(formula = dl_per_pop_round ~  log(pop_per_mil) + exp_tertiary_pstudent*rd, data=complete_df, family = "quasipoisson")
summary(m2.qpois_small)

export_summs(m2.qpois, m2.b.qpois,m2.c.qpois,m2.qpois_small, digits=3, number_format="%.3g",
             statistics = c(N = "nobs", "Null deviance"="null.deviance", "res.deviance"="deviance"))


# Check for changing effect sizes in different regions

# Random slope:
line <- c(1:35000)
m5 <- lmer(formula = dl_per_pop ~ pop_per_mil + log(gdp) + internet_per_pop + exp_tertiary_pstudent + h_index + rd+(1+log(gdp)+exp_tertiary_pstudent|continent), data=complete_df)
isSingular(m5)
summary(m5)

# Generalized model with poisson dist
complete_df$continent<-as.factor(complete_df$continent)
m5.poiss <- glmer(formula = dl_per_pop_round ~ pop_per_mil + internet_per_pop + (1+log(gdp) |continent),
                  data=complete_df, family=poisson)
m5.poiss.allvar <- glmer(formula = dl_per_pop_round ~ pop_per_mil + internet_per_pop + (1+log(gdp)+tertiary+rd |continent), data=complete_df, family=poisson)

m5.poiss
coefficients(m5.poiss)
coefficients(m5.poiss.allvar)

#pplot the effet of income
xyplot(dl_per_pop ~ gdp, 
       data = complete_df, 
       pch = 16, type = c("p", "g", "r"), groups = continent, auto.key = TRUE)


fitted_rows <- as.integer(names(fitted(m5.poiss)))
fitted_rows_allvar <- as.integer(names(fitted(m5.poiss.allvar)))
plot(fitted(m5.poiss) ~ complete_df[fitted_rows, "dl_per_pop"])
lines(line, line)

rmse(fitted(m5.poiss),complete_df[fitted_rows, "dl_per_pop"])
rmse(fitted(m5.poiss.allvar),complete_df[fitted_rows_allvar, "dl_per_pop"])

# Rescale variables:
cols_to_scale <- c("dl_per_pop", "internet_per_pop"  ,"pop_per_mil", "gdp", "tertiary", "exp_tertiary_pstudent", 'h_index','rd')
scaled_data <- complete_df %>%
  dplyr::mutate_at( dplyr::vars(cols_to_scale), scale ) %>%
  rename_at(vars(cols_to_scale), funs(str_replace(.,"$","_scaled")))

cor(scaled_data$rd, scaled_data$exp_tertiary_pstudent, use="complete.obs" )


#run m5 with scaled

m5.poiss.scaled <- glmer(formula = dl_per_pop_round ~ pop_per_mil_scaled + internet_per_pop_scaled + (log(1+gdp_scaled) |continent), data=scaled_data, family=poisson)
m5.poiss.allvar <- glmer(formula = dl_per_pop_round ~ pop_per_mil_scaled + internet_per_pop_scaled + (log(1+gdp_scaled)+tertiary_scaled+rd_scaled |continent), data=scaled_data, family=poisson)

coefficients(m5.poiss.scaled)
coefficients(m5.poiss.allvar)
rmse(fitted(m5.poiss.scaled),complete_df[as.integer(names(fitted(m5.poiss.scaled))), "dl_per_pop_round"])


#add income category
income_cat<-fread("data-raw/worldbank_income_category.csv")
income_cat$income<-as.factor(income_cat$income)

complete_df<-merge(complete_df, income_cat, by="iso3c")
scaled_data<-merge(scaled_data, income_cat, by="iso3c")
names(scaled_data)



#### run variable slope and intercept model on this data #### 

m6.poiss <- glmer(formula = dl_per_pop_round ~ pop_per_mil + internet_per_pop + (1+log(gdp) |income), data=complete_df, family=poisson)
m6.poiss.allvar <- glmer(formula = dl_per_pop_round ~ pop_per_mil + internet_per_pop + (1+log(gdp)+tertiary+rd |income), data=complete_df, family=poisson)
# Based on: https://stats.stackexchange.com/questions/164457/r-glmer-warnings-model-fails-to-converge-model-is-nearly-unidentifiable
m6.poiss.scaled <- glmer(formula = (1+dl_per_pop_scaled) ~ pop_per_mil_scaled + internet_per_pop_scaled + (log(1+gdp_scaled) |income),
                         data=scaled_data, family=poisson, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
m6.poiss.scaled.allvar <- glmer(formula = dl_per_pop_round ~ pop_per_mil_scaled + internet_per_pop_scaled +(tertiary_scaled+rd_scaled |income), 
                                data=scaled_data, family=poisson, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


m6.poiss
coefficients(m6.poiss)

coefficients(m6.poiss.scaled)
coefficients(m6.poiss.scaled.allvar)
# m6.poiss.scaled
# names(scaled_data)

