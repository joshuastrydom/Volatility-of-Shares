# Volatility-of-Shares

This README is here to describe my analysis with the accompanying code. The aim of the paper is to compare of the volatility of returns for large, mid and small cap shares in South Africa. The analysis will be conducted using many packages, with particular use of the PerformanceAnalytics package. Indexes for Large Cap, Mid Cap and Small Cap shares will be created by calculating the product of each constituent shares return and its weight in the respective index. Return, standard deviation and correlation calculations will be used to compare the indexes. 

# Packages
```{r setup, include=FALSE}
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(fmxdat)
library(data.table)
library(tbl2xts)
library(tidyr)
library(PerformanceAnalytics)
library(rvest)
library(corrplot)
library(ggpubr)
```
# Load the data
 ```{r Load data}
Alsi <- readRDS("data/Alsi_Returns.rds")
Alsi <- as.data.frame(Alsi)
```
# Create functions

```{r Functions}
#Create functions that create the data frames for each size
contribution <- function(x) {
  index1 <- x[,c(1:3,7)] |> 
      mutate(Contribution = Return * J203) #All share index
  index1[is.na(index1)] = 0
  wide_index1 <- spread(index1[,c(1:2,5)], Tickers, Contribution)
  wide_index1[is.na(wide_index1)] = 0
  daily_contribution <- wide_index1 |> 
      mutate(Daily_return = select(wide_index1, 2:ncol(wide_index1)) |> rowSums(na.rm = TRUE)) #Sum of daily returns of each stock for index
  first <- daily_contribution[,1]
  last <- daily_contribution[,ncol(daily_contribution)]
  Total <- data.frame(first,last)
  colnames(Total)[1] <- "date"
  colnames(Total)[2] <- "Daily_return"
  return(Total)
} #Use output for rolling and daily functions

contributionmid <- function(x) {
  index1 <- x[,c(1:3,9)] |> 
      mutate(Contribution = Return * J201) #Mid cap index
  index1[is.na(index1)] = 0
  wide_index1 <- spread(index1[,c(1:2,5)], Tickers, Contribution)
  wide_index1[is.na(wide_index1)] = 0
  daily_contribution <- wide_index1 |> 
      mutate(J201Daily_return = select(wide_index1, 2:ncol(wide_index1)) |> rowSums(na.rm = TRUE)) #Sum of daily returns of each stock for index
  Total <- daily_contribution[,c(1,177)]
  return(Total)
} #Use output for rolling and daily functions

contributionsmall <- function(x) {
  index1 <- x[,c(1:3,10)] |> 
      mutate(Contribution = Return * J202) #Small cap index
  index1[is.na(index1)] = 0
  wide_index1 <- spread(index1[,c(1:2,5)], Tickers, Contribution)
  wide_index1[is.na(wide_index1)] = 0
  daily_contribution <- wide_index1 |> 
      mutate(J202Daily_return = select(wide_index1, 2:ncol(wide_index1)) |> rowSums(na.rm = TRUE)) #Sum of daily returns of each stock for index
  Total <- daily_contribution[,c(1,225)]
  return(Total)
}
#Use output for rolling and daily functions

daily_contri <- function(x){
    daily <- x |> 
        mutate(Cum_Ret = cumprod(1 + Daily_return))
    xts <- tbl_xts(daily_contri_large1)
    return(xts)
}

rolling_contri <- function(x){
    rolling <- x |> 
        gather(Ticker, Contribution, -date) |> 
        group_by(Ticker)
    xts <- tbl_xts(tblData = rolling, cols_to_xts = Contribution, spread_by = Ticker)
    return(xts)
}

rolling_SDcum <- function(x){
    rolling <- x |> #Use large_caps, mid_caps or small_caps
        mutate(YM = format(date, "%Y%B")) |> 
        arrange(date) |> 
        group_by(Tickers, YM) |> 
        filter(date == last(date)) |> 
        group_by(Tickers) |> 
        select(date, Tickers, Return) |> 
        mutate(RollSD = RcppRoll::roll_sd(1 + Return, 36, fill = NA, align = "right") * sqrt(12)) |> 
        filter(!is.na(RollSD))
    return(rolling)
}
```
# Dataframe creation 

The following code chunk was used to find the daily returns for each index given the constituent shares' respective weight in each appropriate index. Large Cap shares are part of the J200, Mid Cap shares are part of the J201 and Small Cap shares are part of the J202. 

```{r Dataframes}
# Wrote function to find daily returns for each index given the constituent shares. 
index <- function(x){
    index <- x
    index <- index[,-c(5:10)]
    index$Tickers <- gsub(" SJ|Equity","", index$Tickers)
    wide <- spread(index[,c(1,2,7)], Tickers, Contribution)
    Total <- wide |> 
      mutate(Contribution = select(wide, 2:ncol(wide)) |> rowSums(na.rm = TRUE)) #Sum of daily returns of all stocks of the index
    first <- Total[,1] #Kept the date column
    last <- Total[,ncol(Total)] #Kept the contribution column which ended last due to the mutate
    Final <- data.frame(first,last) #Merged the dateframes
    colnames(Final)[1] <- "date"
    colnames(Final)[2] <- "Daily_return"
    Cum <- Final |> 
        mutate(Cumulative_returns = cumprod(1 + Daily_return)) |>#Get cumulative returns for the index
        filter(Daily_return != 0) #Filtered out rows that did not contribute. 
    return(Cum)
}
J200 <- Alsi |> 
    filter(Index_Name == "Large_Caps") |> 
    mutate(Contribution = Return*J200)
l_caps <- index(J200) #Large Cap Index

J201 <- Alsi |> 
    filter(Index_Name == "Mid_Caps") |> 
    mutate(Contribution = Return*J201)
m_caps <- index(J201) #Mid Cap Index
J202 <- Alsi |> 
    filter(Index_Name == "Small_Caps") |> 
    mutate(Contribution = Return*J202)
s_caps <- index(J202) #Small Cap Index
```

# Index Analysis

The analysis of volatility is based on three indexes of the Johannesburg Securities Exchange (JSE). Data is attained by filtering out the FTSE All Share Index (ALSI). The large cap index is based on the FTSE/JSE Top 40 Index (J200), the mid cap index is based on the FTSE/JSE Mid Cap Index (J201) and the small cap index is based on the FTSE/JSE Small Cap Index (J202). The derived returns for each of the separate indexes are calculated as the product of a stocks daily return and the assigned weight in the respective index. For the large and small cap indexes, data from 2005 to 2022 is available. Unfortunately for the mid cap index, data is only available from 2016 onward.

## Drawdown calculations

```{r Drawdowns}
#Had to convert the previously composed indexes into an xts format
daily_contri <- function(x){
    xts <- tbl_xts(x)
    return(xts)
}
#Created spearte dataframes for each index
drawdown_large <- daily_contri(l_caps)
drawdown_mid <- daily_contri(m_caps)
drawdown_small <- daily_contri(s_caps)
```
Below is the Large Cap Index drawdown graph. 
```{r Figure1}
chart.Drawdown(drawdown_large$Daily_return,main = "Drawdowns: Large Caps", 
               col = "steelblue")
```
Below is the Mid Cap Index drawdown graph. 
```{r Figure2}
chart.Drawdown(drawdown_mid$Daily_return,main = "Drawdowns: Mid Caps", 
               col = "steelblue")
```
Below is the Small Cap Index drawdown graph. 
```{r Figure3}
chart.Drawdown(drawdown_small$Daily_return,main = "Drawdowns: Small Caps", 
               col = "steelblue")
```
## Sector weightings

Sector weightings calculations were used to identify and anaylse the impct of index composition on the return profile, standard deviation and degree of correlation in the index. 

```{r Weightings, include=FALSE}
#Calcualtions for the weight comparision chart for sectors of each index. New dataframes were created so that the accompanying function could operate effectively. 
J200sectors <- J200[,c(1,11,8)] |> 
    group_by(Sector) |> 
    rename(Weight = J200)
J201sectors <- J201[,c(1,11,9)] |> 
    group_by(Sector) |> 
    rename(Weight = J201)
J202sectors <- J202[,c(1,11,10)] |> 
    group_by(Sector) |> 
    rename(Weight = J202)
#Created a function to return the data used to construct the sector weightings within each index.     
sector <- function(x){
    names = c("Financials","Industrials","Resources","Property")
    filtered <- x
    weights <- aggregate(Weight~., x,FUN=sum) |> 
        group_by(date) |> 
        ggplot() +
        geom_bar(aes(x=date, y=Weight, fill=factor(Sector, names)),
                 stat = 'identity', width = 1) +
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        labs(x = NULL, y = NULL, title = "Sector Weighting", fill = "Sector") +
        theme_bw() +
        theme(legend.position="bottom")
        return(weights)
}
J200plot <- sector(J200sectors) #Large Cap Index
J201plot <- sector(J201sectors) #Mid Cap Index
J202plot <- sector(J202sectors) #Small Cap Index
```
The following code chunk was used to determine the mean return for each sector irrespective of their weight in any index. 
```{r sectors, include=FALSE}
sectors <- Alsi[,c(1,3,11)] |> 
    group_by(date, Sector) |> 
    summarise(Sum = sum(Return)) |> 
    ungroup() |> 
    filter(!is.na(Sector)) |> 
    na.omit() |> 
    spread(Sector, Sum)
summary(sectors)
```
The results of the mean return calcualtion were manually tabulated as follows:
\begin{table}[h]
\begin{center}
    \begin{tabular}{| c | c |}
    \hline
        Sector & Mean \\
        \hline
        Financials & 0.16530 \\
        Industrials & 1.342 \\
        Property & 0.009539 \\
        Resources & 0.07961 \\
        \hline
    \end{tabular}
    \caption{Mean daily returns}
    \label{tab:Mean}
\end{center}
\end{table}

Now, the weighting plots are displayed in order to analyse each separate index in detail. 
```{r Figure4}
J200plot
```
```{r Figure5}
J201plot
```
```{r Figure6}
J202plot
```
# Annualised returns

At first, the rolling annualised returns of the indexes were calculated using various functions. The resultant dataframes produced by the functions were used to plot the rolling annualised return graph. 

```{r Annual returns}
#Rolling annualised returns of indices
roll_annualreturn <- function(x,y,z){
    large <- x[,-c(3)]
    colnames(large)[2] <- "Large_Cap"
    mid <- y[,-c(3)]
    colnames(mid)[2] <- "Mid_Cap"
    small <- z[,-c(3)]
    colnames(small)[2] <- "Small_Cap"
    joined <- list(large,mid,small) |> 
        reduce(full_join, by='date') |> 
        gather(Index, Return, -date) |> 
        arrange(date) |> 
        filter(!is.na(Return)) |> 
        mutate(YM = format(date, "%Y%B")) |> 
        arrange(date) |> 
        group_by(Index, YM) |> 
        filter(date == last(date))
    return(joined)
}
#Joined the separate datasets crated. 
full_return <- roll_annualreturn(l_caps, m_caps, s_caps)
plotreturn <- full_return |> 
    group_by(Index) |> 
    mutate(RollRets = RcppRoll::roll_prod(1 + Return, 36, fill = NA, align = "right")^(12/36) - 1) |> 
    group_by(date) |> 
    filter(any(!is.na(RollRets))) |> 
    ungroup() |> 
    ggplot() +
    geom_line(aes(date, RollRets, color = Index), alpha = 0.7, size = 1.25) + 
    labs(title = "Rolling 3 Year Annualized Returns with differing start dates", 
    subtitle = "Large, Mid and Small cap Indexes", x = "", y = "Rolling 3 year Returns (Ann.)", 
    caption = "Note:\nDistortions are not evident now.") + theme_fmx(title.size = ggpts(30), 
    subtitle.size = ggpts(20), caption.size = ggpts(25), CustomCaption = T) + 
    fmx_cols()

```
I then plotted the joined dataset of the annualised returns for the three separate indexes. 

```{r Figure7}
finplot(plotreturn, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```
A density plot of the distribution of annualised returns seemed necessary. The code used to create such a plot follows below. 
```{r ret compare}
b <- full_return |> 
    group_by(Index) |> 
    mutate(RollRets = RcppRoll::roll_prod(1 + Return, 12, fill = NA, align = "right")^(12/12) - 1) |> #Annualised rolling returns
    group_by(date) |> 
    filter(any(!is.na(RollRets))) |> 
    ungroup() 

# c is used as the input for each of the density plots.
c <- b[-c(1:146),-c(3,4)] |> #Filtered out for the past 10 years and got rid of unnecessary columns
    spread(Index, RollRets)
return_density <- full_return[,-4] |>
    spread(Index, Return)
#Large Cap Index density plot
return_density_large <- ggplot(c[,c(1,2)], aes(x = Large_Cap)) +
    geom_density(color = "darkred", fill = "red") +
    geom_vline(aes(xintercept = mean(Large_Cap)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(-0.05, 0.1), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")

#Mid Cap Index density plot
mid_density <- c[,c(1,3)] |> 
    na.omit() #Had to omit NAs for the geom_vline to function. 
return_density_mid <- ggplot(mid_density, aes(x = Mid_Cap)) +
    geom_density(color = "darkblue", fill = "lightblue") +
    geom_vline(aes(xintercept = mean(Mid_Cap)), 
             linetype = "dashed", size = 0.6, na.rm = FALSE) +
    coord_flip() +
    scale_x_continuous(limits = c(-0.05, 0.1), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")
    
#Small Cap Index density plot
return_density_small <- ggplot(c[,c(1,4)], aes(x = Small_Cap)) +
    geom_density(color = "darkgreen", fill = "lightgreen") +
    geom_vline(aes(xintercept = mean(Small_Cap)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(-0.05, 0.1), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")
```
I used ggarange to combine the density plots. 
```{r Figure8}
retplot <- ggarrange(return_density_large, return_density_mid, return_density_small + rremove("x.text"), 
          labels = c("Large Cap", "Mid Cap", "Small Cap"), align = "h", common.legend = TRUE, font.label = list(size = 10, color = "black"),
          ncol = 3, nrow = 1)
annotate_figure(retplot, top = text_grob("Rolling 12-Month Return Comparison (past 10 Years)", 
               color = "black", face = "bold", size = 11))
```
# Rolling Standard deviation 

The rolling standard deviation calculations follow. The RcppRoll function was employed to determine the rolling standard deviations. 

```{r Join}
#Filter out the necessary columns of the respective indexes 
ljoin <- l_caps[,c(1,2)]
colnames(ljoin)[2] <- "large_dailyreturn"
mjoin <- m_caps[,c(1,2)]
colnames(mjoin)[2] <- "mid_dailyreturn"
sjoin <- s_caps[,c(1,2)]
colnames(sjoin)[2] <- "small_dailyreturn"

#Wrote a function to calculate the rolling stanard deviations
roll_SD <- function(x,y,z){
    large <- x |>
        mutate(Large_Cap = RcppRoll::roll_sd(1 + Daily_return, 36, fill = NA, align = "right") * sqrt(12)) |> 
        filter(!is.na(Large_Cap))
    l <- large[,c(1,4)]
    mid <- y |> 
        mutate(Mid_Cap = RcppRoll::roll_sd(1 + Daily_return, 36, fill = NA, align = "right") * sqrt(12)) |> 
        filter(!is.na(Mid_Cap))
    m <- mid[,c(1,4)]
    small <- z |> 
        mutate(Small_Cap = RcppRoll::roll_sd(1 + Daily_return, 36, fill = NA, align = "right") * sqrt(12)) |> 
        filter(!is.na(Small_Cap))
    s <- small[,c(1,4)]
    joined <- list(l,m,s) |> 
        reduce(full_join, by='date') |> 
        gather(Index, SD, -date) |> 
        arrange(date)
    return(joined)
}

#Used the function to create a graph of the rolling standard deviations. 
full_SD <- roll_SD(l_caps, m_caps, s_caps) |> 
    ggplot() +
        geom_line(aes(x = date, y = SD, color = Index), alpha = 0.7, size = 1) +
        labs(title = "Rolling 3 Year Annualized SD with differing starting dates", subtitle = "Large, Mid and Small cap Indexes", x = "", y = "Rolling 3 year Returns (Ann.)", caption = "Note:\nDistortions are not evident now.") + 
        theme_fmx(title.size = ggpts(30), subtitle.size = ggpts(20), caption.size = ggpts(25), CustomCaption = T) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    theme(legend.position = "bottom") +
    fmx_cols()
```
I then used the resulting dataframes/gg dataset to plot the results. 

```{r Figure9}
finplot(full_SD, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, 
    y.pct = T, y.pct_acc = 1)
```
Now, I needed to data for the rolling 12 month standard deviation of each index. This data would eventually be used to plot density graphs. 

```{r SD compare}
#Rolling 12-month standard deviation for the indexes
full <- full_return |> 
        group_by(Index) |> 
        select(date, Index, Return) |> 
        mutate(RollSD = RcppRoll::roll_sd(1 + Return, 12, fill = NA, align = "right") * sqrt(12)) |> 
        filter(!is.na(RollSD))
        
#Filter out columns that are not necessary. 
SD_density <- full[,-3] |>
    spread(Index, RollSD)

#Create density plot for the Large Cap Index
sd_density_large <- ggplot(SD_density[-c(1:73),c(1,2)], aes(x = Large_Cap)) +
    geom_density(color = "darkred", fill = "red") +
    geom_vline(aes(xintercept = mean(Large_Cap)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(0.01, 0.04),labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")

#Create density plot for the Mid Cap Index
sd_mid <- SD_density[,c(1,3)] |> 
    na.omit()
sd_density_mid <- ggplot(sd_mid, aes(x = Mid_Cap)) +
    geom_density(color = "darkblue", fill = "lightblue") +
    geom_vline(aes(xintercept = mean(Mid_Cap)), 
             linetype = "dashed", size = 0.6, na.rm = FALSE) +
    coord_flip() +
    scale_x_continuous(limits = c(0.01, 0.04), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")

#Create density plot for the Small Cap Index
sd_density_small <- ggplot(SD_density[-c(1:73),c(1,4)], aes(x = Small_Cap)) +
    geom_density(color = "darkgreen", fill = "lightgreen") +
    geom_vline(aes(xintercept = mean(Small_Cap)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(0.01, 0.04), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")
```

I used ggarrange to combine the respective ggplots for a nice figure to be used in the paper. 

```{r Figure10}
sdplot <- ggarrange(sd_density_large, sd_density_mid, sd_density_small + rremove("x.text"), 
          labels = c("Large Cap", "Mid Cap", "Small Cap"), font.label = list(size = 10, color = "black"),
          ncol = 3, nrow = 1)
annotate_figure(sdplot, top = text_grob("Rolling 12-Month Risk Comparison (past 10 Years)", 
               color = "black", face = "bold", size = 11))
```

The 'average' volatility for each index was then calculated to compare to the rolling standard deviation calculation. 

```{r STD}
largevol <- StdDev(l_caps$Daily_return)
midvol <- StdDev(m_caps$Daily_return)
smallvol <- StdDev(s_caps$Daily_return)

cat("Large volatility: ", largevol, "\n")
cat("Mid volatility: ", midvol, "\n")
cat("Small volatility: ", smallvol)
```
The results were manually tabulated as follows. 

\begin{table}[h]
\begin{center}
    \begin{tabular}{| c | c |}
    \hline
         & Standard deviation \\
        \hline
        Large cap & 0.01298498 \\
        Mid cap & 0.01039368 \\
        Small cap & 0.00801369 \\
        \hline
    \end{tabular}
    \caption{Standard deviation}
    \label{tab:SD}
\end{center}
\end{table}

Now, the volatility skewness of each index was calculated. This is a ratio of upside variance to downside variance. 

```{r vol}
vol_returns <- list(ljoin, mjoin, sjoin) |>
    reduce(full_join, by='date') |> 
    tbl_xts()
VolatilitySkewness(vol_returns, MAR = 0, stat = c("volatility"))
```
The results were again manually tabulated. 

\begin{table}[h]
\begin{center}
    \begin{tabular}{| c | c | c | c |}
    \hline
         & Large cap & Mid cap & Small cap \\
        \hline
        Volatility skewness & 1.069731 & 0.9739739 & 1.379238 \\
        \hline
    \end{tabular}
    \caption{Volatility skewness}
    \label{tab:VS}
\end{center}
\end{table}

# Correlation

The correlation calculation was rather involved. I first created an index to determine the degree of correlation between all stocks within each respective index. After those datafiles were obtained, separate density plots were produced using ggplot2. The resulting graphs were then combined and were used in the paper. 

```{r Correlation}
#Function to determine the correlation between constituent shares within each index. 
correlationreturn <- function(x){
    index <- x
    index <- index[,-c(5:10)]
    index$Tickers <- gsub(" SJ|Equity","", index$Tickers)
    Total <- index |> 
        mutate(Contribution = coalesce(Contribution, 0)) |> 
        group_by(Tickers) |> 
        mutate(Cumulative_contribution = cumprod(1 + Contribution))
    Final <- Total[,c(1,2,3)]
    wide <- spread(Final, Tickers, Return)
    wide1 <- as.data.frame(wide)
    wide2 <- wide1[-c(1:1750),]
    #wide1[is.na(wide1)] = 0 #Made NAs equal to zero so that rowSums function would work
    xts <- wide2[,c(2:86)] |> 
        xts(order.by = as.Date(wide2[ , 1], "%Y-%m-%d"))
    y <- rollapply(xts, width=365, cor, fill = NA, by.column=FALSE)
    paste_ <- function(...) paste(..., sep = "_")
    names_mat <- do.call("outer", list(names(xts), names(xts), paste_))
    names(y) <- names_mat
    #y$Average_correlation <- rowMeans(y, na.rm=TRUE)
    e <- y |> 
        xts_tbl()
    f <- rbind(e, sapply(e, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else ""))
    last <- f[nrow(f),]
    long <- last |> 
        gather(Stocks, Correlation, -date)
    long1 <- long[,-1]
    long2 <- arrange(long1, Correlation)
    long3 <- long2[!duplicated(long2$Correlation), ]
    long3$Correlation <- as.numeric(as.character(long3$Correlation))
    #long4 <- long3[!is.nan(long3)] |> 
    #    filter(long3$Correlation != 1)
    #long3 = as.numeric(long2[,2]) 
    return(long3)
}

#Large Cap Index correalation dataframe
large_corr <- correlationreturn(J200)
#Large Cap Index density plot of correlations
large_corr1 <- large_corr[-c(1493:1494),]
plot_largecorr <- ggplot(large_corr1, aes(x = Correlation)) +
    geom_density(color="darkred", fill="red") +
    geom_vline(aes(xintercept = mean(Correlation)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(0, 0.5), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")

#Mid Cap Index correalation dataframe
mid_corr <- correlationreturn(J201)
#Mid Cap Index density plot of correlations
mid_corr1 <- mid_corr[-c(1044:1045),]
plot_midcorr <- ggplot(mid_corr1, aes(x = Correlation)) +
    geom_density(color="darkblue", fill="lightblue") +
    geom_vline(aes(xintercept = mean(Correlation)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(0, 0.5), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")

#Small Cap Index correalation dataframe
small_corr <- correlationreturn(J202)
#Small Cap Index density plot of correlations
small_corr1 <- small_corr[-c(721:722),]
plot_smallcorr <- ggplot(small_corr1, aes(x = Correlation)) +
    geom_density(color="darkgreen", fill="lightgreen") +
    geom_vline(aes(xintercept = mean(Correlation)), 
             linetype = "dashed", size = 0.6) +
    coord_flip() +
    scale_x_continuous(limits = c(0, 0.5), labels = scales::percent_format(scale = 100)) +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
    theme(legend.position="none")
```

The graphs were combined and plotted. 

```{r Figure11}
corrplot <- ggarrange(plot_largecorr, plot_midcorr, plot_smallcorr + rremove("x.text"), 
          labels = c("Large Cap", "Mid Cap", "Small Cap"), font.label = list(size = 10, color = "black"),
          ncol = 3, nrow = 1)
annotate_figure(corrplot, top = text_grob("Rolling 12-Month Correlation Comparison (past 10 Years)", 
               color = "black", face = "bold", size = 11))
```