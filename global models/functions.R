# First function to get the data

wb_data <- function(db = "", name="", date=2015, history=0){
  temp <- wb(indicator = db, startdate = date-history, enddate = date)
  setDT(temp)
  setnames(temp, "value", name)
  if (history>0){
    temp2 <- spread(temp, date, name)
    setDT(temp2)
    temp2[, (paste0(name, "_change")) := as.integer(get(as.character(date)) - get(as.character(date-history)))]
    temp2
  } else{temp}
}

load_data <- function(location, which="download", source="offline"){
  # Load in data
  data <- fread(location)
  if (which=="download"){
    data <- data[iso3c !=  ""]
    # Get iso2c codes, so we can merge automatically
    data[, iso2c := countrycode(country_name, origin = "country.name", destination = "iso2c")]
    
    # Get region
    data[, continent :=  countrycode(iso2c, 'iso2c', 'continent')]
    data[iso2c == "CA" | iso2c == "US", continent := "North America"]
  } else if (which=="population" & source=="offline"){
    data <- fread(location, colClasses = list("character"="population"))
    data[, population:= as.double(population)]
  } else if (which=="population" & source=="online"){
    data <- wb_data("SP.POP.TOTL", "population")
  } else if ((which=="gdp" |which=="internet")& source=="offline"){
    data <- fread(location)
  } else if (which=="gdp" & source=="online"){
    data <- wb_data("NY.GDP.PCAP.PP.CD", "gdp")
  } else if (which=="internet" & source == "online"){
    data <- wb_data("IT.NET.BBND", "internet")
  }
  data
}

prepare_data <- function(download, population, gdp, internet){
    internet_pop <- merge(population, internet, by = c("country", "date", "iso3c"))
  # Get fixed broadbansd subscriptions per population
  internet_pop[, internet_per_pop := internet/population]
  # Add different variables to a new data.table, don't pollute the first one
  data <- merge(download, population[, .(population, iso3c)], by="iso3c", all.x=T)
  data <- merge(data, gdp[, .(gdp, iso3c)],   by="iso3c", all.x=T)
  data <- merge(data, internet_pop[, .(internet_per_pop, iso3c)],  by="iso3c", all.x=T)
  
  # I use population in Ms, because then the DL/POP will be a bigger number --> better to see effects in  model
  data[, pop_per_mil := population/1000000]
  
  # Calculate the downloads per population. 
  data[, dl_per_pop := dls/pop_per_mil]
  #data <- data[!is.na(iso2c)]
  
  which_date <- 2015
  
  literacy <- wb_data("SE.ADT.LITR.ZS", "literacy", date=which_date)
  rd <- wb_data("GB.XPD.RSDV.GD.ZS", "rd", date=which_date)
  tertiary <- wb_data("SE.TER.ENRR", "tertiary", date=which_date)
  
  expenditure <- read_tsv("~/GitHub/bookpiracy_final/global models/proba.csv", na='-')
  setDT(expenditure)
  expenditure[, c(14:29):=NULL]
  setnames(expenditure, names(expenditure), c("country", "exp_gdp",
                                              "exp_gov", "exp_pre_primary_pp",
                                              "exp_pre_primary_pp_gdp_pc",
                                              "exp_primary_pp",
                                              "exp_primary_pp_gdp_pc",
                                              "exp_secondary_pp",
                                              "exp_secondary_pp_gdp_pc",
                                              "exp_tertiary_pstudent",
                                              "exp_tertiary_pstudent_gdp_pc",
                                              "exp_textbook",
                                              "teaching_compensation"))
  
  for (col in names(expenditure)){
    if (col == "country"){next}
    temp <- expenditure[, get(col)]
    expenditure[, (col):= unlist(lapply(temp, function (x) as.double(gsub(" ", "", x)))), with=FALSE]
  } 
  expenditure[, iso3c:=  countrycode(country, origin = "country.name", destination = "iso3c")]
  # 
  spending_education <- fread("~/GitHub/bookpiracy_final/global models/public_spending_education_oecd.csv")
  setnames(spending_education, "Value", "spending_education")
  setnames(spending_education, "LOCATION", "iso3c")
  data <- merge(data, expenditure[, .(exp_tertiary_pstudent, iso3c)], by="iso3c", all.x=T) # Expennditure per student
  data <- merge(data, tertiary[, .(tertiary, iso3c)], by="iso3c", all.x=T) # Number of enrolled
  data <- merge(data, rd[, .(rd, iso3c)], by="iso3c", all.x=T)

  # Citation
  citation <- fread("~/GitHub/bookpiracy_final/global models/citation.csv")
  citation[, iso3c:=  countrycode(iso2c, origin = "iso2c", destination = "iso3c")]
  citation$iso2c <- NULL
  data <- merge(data, citation, by="iso3c", all.x=T)
  
  data[, region := countrycode(iso3c, 'iso3c', 'region')]
  data
}

estimate_model <- function(data, x, y,family=NULL,scale=F){
  cleared_colnames <- gsub("\\d", "", c(x,y))
  cleared_colnames <- gsub(".*\\|", "", cleared_colnames)
  cleared_colnames <- gsub(".*\\*.*", "", cleared_colnames)
  cleared_colnames <- gsub("log\\(|\\)|\\||\\(", "", cleared_colnames)
  cleared_colnames <- cleared_colnames[cleared_colnames !=""]
  print(cleared_colnames)
  data <- na.omit(p.data[, .SD, .SDcols=c("iso2c","iso3c",cleared_colnames)])
  if (scale){
    numeric_cols <- colnames(data)[unlist(lapply(data, is.numeric))]
    scaled_data <- scale(data[, .SD,.SDcols=numeric_cols])
    data <- cbind(scaled_data, data[, .SD, .SDcols=colnames(data)[!unlist(lapply(data, is.numeric))]])
  }
  if (is.null(family)){
    formula <- as.formula(paste0(y,"~", paste(x, collapse = "+")))
    m <- glm(formula = formula, data=data)
  } else if (family=="poisson"|family=="quasipoisson"){
    data[, paste0(y,".round") := round(get(y))]
    formula <- as.formula(paste0(y,".round~", paste(x, collapse = "+")))
    m <- glm(formula = formula, data=data, family=family)
  } else if (family =="nb"|family=="negative-binomial"){
    data[, paste0(y,".round") := round(get(y))]
    formula <- as.formula(paste0(y,".round~", paste(x, collapse = "+")))
    m <- glm.nb(formula = formula, data=data)
  } else if (family=="varying"){
    formula <- as.formula(paste0(y,"~", paste(x, collapse = "+")))
    m <- lmer(formula, data = data)
  }
  m
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

# citation <- read_excel('citation/scimagojr_2015.xlsx')
# setDT(citation)
# setnames(citation, 'H index', 'h_index')
# # cor(citation[, .(Documents, h_index)])
# 
# citation[, iso2c := countrycode(Country, origin = "country.name", destination = "iso2c")]
# setnames(citation, names(citation), tolower(names(citation)))
# setnames(citation, names(citation), gsub(" ", "_", names(citation)))
