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

# Now I have the exchange values from more than 20 years back, at a daily level, that's good enough spread I believe. Looking at the distribution of missing values across currencies, I see Euro has about 14% missing, because Euro started around 1999. There could be other reasons why there are missing values. I think if I even have half the data (data has over 6k rows) available, it would be sufficient for my first iteration

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

# Now onto the modeling

# Creating a sample of what I want to do with each currency, I will then extend the idea to all the currencies. Let me pick Euro to model on Gold value

sampleDf <- df[,c("Euro.(EUR)","GOLDAMGBD228NLBM")]
sampleDf <- sampleDf[complete.cases(sampleDf),]
colnames(sampleDf) <- c('EuroInUSD','GoldInUSD')
modelSampleEuro <- lm(formula = EuroInUSD ~ GoldInUSD, data = sampleDf, model = TRUE)
summary(modelSampleEuro)

sampleDf2 <- df[,c("Chinese.yuan.(CNY)","GOLDAMGBD228NLBM")]
sampleDf2 <- sampleDf2[complete.cases(sampleDf2),]
colnames(sampleDf2) <- c('CHNYuanInUSD','GoldInUSD')
modelSampleYuan <- lm(formula = CHNYuanInUSD ~ GoldInUSD, data = sampleDf2, model = TRUE)
summary(modelSampleYuan)


summary(modelSampleEuro)
summary(modelSampleYuan)