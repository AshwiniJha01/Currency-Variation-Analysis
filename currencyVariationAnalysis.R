# The idea I want to test is if all the currencies react to changes in USD in the same manner or some currencies react differently. How can we do this? If I use a currency's exhange rate with US, what do I compare it with?
# The idea I am going to test involves comparing changes in currency's exchange rate with how much gold can US dollar buy. The amount of gold that US dollar can buy will act as a proxy metric for benchmarking the value of USD and the exchange rate of all the currencies in USD will benchmark the value of these currencies
# In the analysis, I am going to look at the correlation of the currencies with Gold value and also build a linear regression model. I believe if I build a linear regression model where I model the value of a currency in USD on the Gold value in USD, the coefficients and the std error in the coefficients can indicate the reaction behavior. If the std error in the coefficients is high, it would mean higher unpredictability on a currency's value when based on USD. Also, the error in the model for a given currency, when significantly different from other currencies can imply that currency is not directly linked to USD value. Hence, when we model a currency's exchange rate in USD on Gold value in USD, we are essentially explaining variation in a currency's value based on variation in US currency
# I am not a practitioner in the area of economics or have a formal educaion in this area in college years (I graduated in Mechanical Engineering), in best case my self-taught understading is at basic level. With this basic understanding I have set out to explore this topic and tease out insights that are interesting or thought provoking, there are going to be some debatable implications or assumptions that I have used in my work, like there are factors like a country's GDP, fiscal deficit and political stability, to name a few, that affect the exchange rate and I am not using them in the analysis in this version. But, a joureny of a thousand miles begins with one step......

# Let's read in the libraries we will need:
lapply(c("caret","dplyr", "e1071", "openxlsx"), require, character.only = T)

# Let's read in the curency exchange values from the last 24 odd years, its a report that can be downloaded from IMF website, https://www.imf.org/external/np/fin/ert/GUI/Pages/ReportOptions.aspx. I am reading the data from a local file
df <- openxlsx::read.xlsx("A:\\Projects\\Currency variation analysis\\Exchange_Rate_Report_USDunit.xlsx",rows = 3:6149)

# Correcting the date format:
df$Date <- as.Date(df$Date, origin = "1899-12-30")

# Confirming I have read all the rows:
summary(df$Date)

# Checking the frequency of missing data, so that we can eliminate those currencies where lot of data points are missing
indexMissing <- sapply(df, function(x) sum(is.na(x))/length(x))

# Now I have the exchange values from more than 20 years back, at a daily level, that's good enough spread I believe. Looking at the distribution of missing values across currencies, I see Euro has about 14% missing, because Euro started around 1999. There could be other reasons why there are missing values. I think if I even have half the data (data has over 6k rows) available, it would be sufficient for my first iteration. Also, presence of missing values makes it difficult for me to do a time series or panel regression.

# Removing those currencies which have more than 50% data missing
df <- df[,names(indexMissing[indexMissing < 0.5])]

# Now as all currencies are reported in USD, the column for USD has all 1s
unique(df$`U.S..dollar.(USD)`)
# So we drop this column as well
df$`U.S..dollar.(USD)` <- NULL

# I have download the gold prices data from FRED (Federal Reserve Bank of St. Louis), https://fred.stlouisfed.org/series/GOLDAMGBD228NLBM, The unit of price is U.S. Dollars per Troy Ounce (Not Seasonally Adjusted).

# I am going to read this data now:
goldDf <- openxlsx::read.xlsx("A:\\Projects\\Currency variation analysis\\GOLDAMGBD228NLBM.xlsx",rows = 11:13471)
# Correcting the date format:
goldDf$observation_date <- as.Date(goldDf$observation_date, origin = "1899-12-30")
# Confirming all the rows have been read in;
summary(goldDf$observation_date)

# I will bring in the gold prices into the currency data frame, only for the range of dates I have exchange rates for
df <- merge(x = df, y = goldDf, by.x = "Date", by.y = "observation_date", all.x = T, all.y = F)

# This is how the data looks:
head(df, 5)

# These are the properties of the columns
str(df)
summary(df)

# I am going to do some basic eda:

# Let's look at the correlation values
corMat <- cor(x = df[,2:35], y = df[,36], use = "pairwise.complete.obs")

summary(corMat)

# If we look at the correlation distribution, we see that is varies wildly, with one quartile of currencies showing negative correlation while less than quartile show positive correlation

# These are the top five positively correlated currencies
top_n(data.frame(Currency = rownames(corMat), corr = corMat), n = 5)

# Top 5 negatively correlated currencies
top_n(data.frame(Currency = rownames(corMat), corr = corMat), n = -5)

# As wealth is not a conserved quantity like mass or energy, I am guessing the negative correlation is also not a casuation. It is surprising (for me, at least) to see a European currency Swiss Franc is part of the negative correlation list. The positive correlation list is not so surprising. Another point to note is that the magnitude of negative correlations are higher than the magnitude of positive correlations (max negative correlation = -0.96, max positive correlation = 0.74)

# What about the currencies with low correlations? Maybe their economy has been independent of the US economy

# Linear Regression

# Creating a sample of what I want to do with each currency, I will then extend the idea to all the currencies. Let me pick Aud to model on Gold value

iDf <- df[,c(2, 36)]
iDf <- iDf[complete.cases(iDf),]
clNms <- colnames(iDf)
clNms[1] <- gsub(")", "_", gsub("(", "_",clNms[1], fixed = T), fixed = T)
clNms[2] <- gsub(")", "_", gsub("(", "_",clNms[2], fixed = T), fixed = T)
colnames(iDf) <- clNms
frmla = paste0(clNms[1], " ~ ", clNms[2])
frmla <- as.formula(frmla)
iModel <- lm(formula = frmla, iDf, model = TRUE)
iModelSummaryObj <- summary(iModel)

iModelSummary <- data.frame(Currency = colnames(df)[2])
iModelSummary <- iModelSummary %>% 
  mutate(
    Residual_Min = min(iModel$residuals),
    Residual_1Q = quantile(iModel$residuals, 0.25),
    Residual_Median = quantile(iModel$residuals, 0.5),
    Residual_Mean = mean(iModel$residuals),
    Residual_3Q = quantile(iModel$residuals, 0.75),
    Residual_Max = max(iModel$residuals),
    Coeff_Intercept_Estimate = iModelSummaryObj$coefficients["(Intercept)","Estimate"],
    Coeff_Intercept_StdError = iModelSummaryObj$coefficients["(Intercept)","Std. Error"],
    Coeff_Intercept_tValue = iModelSummaryObj$coefficients["(Intercept)","t value"],
    Coeff_Intercept_PrT = iModelSummaryObj$coefficients["(Intercept)","Pr(>|t|)"], # t prob of intercept
    Coeff_Gold_Estimate = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Estimate"],
    Coeff_Gold_StdError = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Std. Error"],
    Coeff_Gold_tValue = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","t value"],
    Coeff_Gold_PrT = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Pr(>|t|)"], # t prob of Gold variable
    ResidualStdError = iModelSummaryObj$sigma,
    RSquared = iModelSummaryObj$r.squared,
    FStatistic = iModelSummaryObj$fstatistic['value'],
    DoF = iModelSummaryObj$fstatistic['dendf'] #Degrees of freedom
  )
modelSummary <- data.frame()
modelSummary <- rbind(modelSummary, iModelSummary)


# Going to create a for loop to execute the above and gather linear regression summary for all the currencies:
modelSummary <- data.frame()
for(i in 2:(ncol(df)-1)){
  # i = 7
  iDf <- df[,c(i, 36)]
  iDf <- iDf[complete.cases(iDf),]
  clNms <- colnames(iDf)
  clNms[1] <- gsub(")", "_", gsub("(", "_",clNms[1], fixed = T), fixed = T)
  clNms[2] <- gsub(")", "_", gsub("(", "_",clNms[2], fixed = T), fixed = T)
  colnames(iDf) <- clNms
  frmla = paste0(clNms[1], " ~ ", clNms[2])
  frmla <- as.formula(frmla)
  iModel <- lm(formula = frmla, iDf, model = TRUE)
  iModelSummaryObj <- summary(iModel)
  
  iModelSummary <- data.frame(Currency = colnames(df)[i])
  iModelSummary <- iModelSummary %>% 
    mutate(
      Residual_Min = min(iModel$residuals),
      Residual_1Q = quantile(iModel$residuals, 0.25),
      Residual_Median = quantile(iModel$residuals, 0.5),
      Residual_Mean = mean(iModel$residuals),
      Residual_3Q = quantile(iModel$residuals, 0.75),
      Residual_Max = max(iModel$residuals),
      Coeff_Intercept_Estimate = iModelSummaryObj$coefficients["(Intercept)","Estimate"],
      Coeff_Intercept_StdError = iModelSummaryObj$coefficients["(Intercept)","Std. Error"],
      Coeff_Intercept_tValue = iModelSummaryObj$coefficients["(Intercept)","t value"],
      Coeff_Intercept_PrT = iModelSummaryObj$coefficients["(Intercept)","Pr(>|t|)"], # t prob of intercept
      Coeff_Gold_Estimate = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Estimate"],
      Coeff_Gold_StdError = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Std. Error"],
      Coeff_Gold_tValue = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","t value"],
      Coeff_Gold_PrT = iModelSummaryObj$coefficients["GOLDAMGBD228NLBM","Pr(>|t|)"], # t prob of Gold variable
      ResidualStdError = iModelSummaryObj$sigma,
      RSquared = iModelSummaryObj$r.squared,
      FStatistic = iModelSummaryObj$fstatistic['value'],
      DoF = iModelSummaryObj$fstatistic['dendf'] #Degrees of freedom
    )
  modelSummary <- rbind(modelSummary, iModelSummary)

}


# In the data frame modelSummary, we have pretty much all the linear regression output that lm returns

# Let us summarize the output one by one and see if something interesitng comes out

# RSquared
summary(modelSummary$RSquared)
# On an average, any currency's valuation is only 33% explained by the variation in value of USD and only a quarter of the currencies have more than 50% variation explained by USD

# These are the currencies whose variation can be explained with variation in USD:
top_n(data.frame(Currency = modelSummary$Currency, RSquared = modelSummary$RSquared), n = 5)
# Well, Chinese Yuan is a part of the top 5 linearly related currency, but as we had seen earlier, the correlation is negative in magnitude, hence there may be justification behing US' claim of currency manipulation by China
# (note: in case of a simple linear regression, the RSquared = correlation coefficient^2, hence the similarity of result with earlier correlation analysis)

# These are the currencies with little linear relationship with the USD
top_n(data.frame(Currency = modelSummary$Currency, RSquared = modelSummary$RSquared), n = -5)

#Beta Coefficient

# Another way to look at the relationship between USD and a currency is to look at the beta estimate of the coefficient along with the standard error in it. When the standard error in the coefficient is a high percentage of the coefficient, then it implies that the coefficient itself is not a good estimate value (coefficient is a mean value of the coefficients from the linear regression analysis)

tbd <- modelSummary %>%
  mutate(
    coeffDispersion = abs(Coeff_Gold_StdError)/abs(Coeff_Gold_Estimate) #As I am interested in only the ratio, not the sign, I am using absolute values
  )
summary(tbd$coeffDispersion)

# Currencies with least coefficient of dispersion - these are the currencies where the linear regression model can be trusted better than other models with lower coefficient of dispersion (as we have only one independent variable, we are not worried about multicollinearity increasing the variance inflation factor, a.k.a VIF)
top_n(data.frame(Currency = tbd$Currency, coeffDispersion = tbd$coeffDispersion), n = -5)
# As expected, these are the same currencies where we have maximum RSquared from the model as well

# Currencies with maximum coefficient of dispersion
top_n(data.frame(Currency = tbd$Currency, coeffDispersion = tbd$coeffDispersion), n = 5)
# Again, currencies with least reliable coefficients are the ones where RSqaured were among the least
rm(tbd)

# Intercept

# In a simple linear regression, the intercept implies an average value of the dependent variable given the independent variable is zero. This is applicable only in cases where independent variable can take meaningful zero values. In our case, independent variable is the value of gold in USD, which can never be zero, as gold will never be free in United States (or anywhere in the world). But, I feel like indulging myself here. I am going to do a mind experiment, I will think what happens when US currency is so valuable that infinitessimally small amount of USD can buy gold, look at the hypothetical average values of the currencies. I am guessing the interpretation of the intercept can then be, the smaller the intercept values the more inherent value of the currency. Let us check if that appears intuitive from the currencies that have lowest intercept value

# Simple summary of the distribution of intercepts
summary(modelSummary$Coeff_Intercept_Estimate)

# Top 5 currencies where average value is high for almost zero USD gold
top_n(data.frame(Currency = modelSummary$Currency, Coeff_Intercept_Estimate = modelSummary$Coeff_Intercept_Estimate), n = 5)

# Bottom five such currencies
top_n(data.frame(Currency = modelSummary$Currency, Coeff_Intercept_Estimate = modelSummary$Coeff_Intercept_Estimate), n = -5)


# Seeing Botwana currency in this list, I recall we had seen this currency in the list of least RSquared models too. So I am going to revise the list based on the RSqaured and StdError in Intercept - I am going to place filter for RSqaured greater than 40% and where the std error in coefficient is less than 75% of the intercept estimate itself
modelSummary %>%
  filter(RSquared > 0.4) %>%
  filter(Coeff_Intercept_StdError/Coeff_Intercept_Estimate < 0.75) %>%
  select(Currency, Coeff_Intercept_Estimate) %>%
  top_n(-5)
# I think this list makes more sense, these are all amongst the strongest currencies in the world, although Euro is missing from this list as the RSquared is less than 40%