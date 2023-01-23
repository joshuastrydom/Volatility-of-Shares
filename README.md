# Volatility-of-Shares
 A comparison of the volatility of returns for large, mid and small cap shares in South Africa
 
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
# Return calculations

As no J205 column was available, daily returns for each size of stock were calculated by multiplying the individual returns with the weight assigned to the J203 index. Subsets of the Alsi data frame were thus made for each of the large, mid and small cap stocks. The daily returns are the contribution to the return of the J203 by each of the large, mid and small cap stock groupings.

# Large cap shares

The large cap share data was acquired from the Alsi dataset. The daily returns were calculated by multiplying the returns column with the J203 column.

```{r Large cap shares, include=FALSE}
large_caps <- Alsi |> 
    filter(Index_Name == "Large_Caps")
large_caps$Tickers <- gsub(" SJ|Equity","", large_caps$Tickers)
large <- contribution(large_caps)
daily_large <- daily_contri(large)
#colnames(daily_large)[1] <- "large_daily_return"
rolling_large <- rolling_contri(large)
rolling_large1 <- rolling_SDcum(large_caps) |> 
    ggplot() +
    geom_line(aes(x = date, y = RollSD, color = Tickers), alpha = 0.7, size = 1.25) +
    labs(title = "Rolling 3 Year Annualized SD of various large cap stocks with differing start dates", subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", caption = "Note:\nDistortions are not evident now.") + 
    theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) +
    theme(legend.position="none") #+
    #theme(text=element_text(family="Garamond", size=14))
```
I then plotted the rolling 3 year annualised standard deviation of the large cap stocks. 

```r 
finplot(rolling_large1, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```
A drawdown chart for large cap shares was then attained. 
```r 
chart.Drawdown(daily_large$Daily_return,main = "Drawdowns: Large caps in J203", 
               col = "steelblue")
```
A cumulative returns graph was formulated to ascertain whether the risk is worth the reward. My line of thinking is if large cap shares have a greater volatility of returns then investors need to be rewarded with higher cumulative returns. 
```r 
ggplot(daily_large, aes(x = Index, y = Cum_Ret)) + 
    geom_line() +
    theme_bw() +
    labs(title = "Large cap shares in J203 cumulative Return", y = "Growth of R1 invested in 2005.")
```
# Mid cap shares

The mid cap share data was acquired from the Alsi dataset. The daily returns were calculated by multiplying the returns column with the J203 column.

```{r Mid cap shares, include=FALSE}
mid_caps <- Alsi |> 
    filter(Index_Name == "Mid_Caps")
mid_caps$Tickers <- gsub(" SJ|Equity","", mid_caps$Tickers)
mid <- contribution(mid_caps)
mid1 <- contributionmid(mid_caps)
daily_mid <- daily_contri(mid)
#colnames(daily_mid)[1] <- "mid_daily_return"
rolling_mid <- rolling_contri(mid)
rolling_mid1 <- rolling_SDcum(mid_caps) |> 
    ggplot() +
    geom_line(aes(x = date, y = RollSD, color = Tickers), alpha = 0.7, size = 1.25) +
    labs(title = "Rolling 3 Year Annualized SD of various mid cap stocks with differing start dates", subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", caption = "Note:\nDistortions are not evident now.") + 
    theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) +
    theme(legend.position="none") #+
    #theme(text=element_text(family="Garamond", size=14))
```
I then plotted the rolling 3 year annualised standard deviation of the mid cap stocks. 
```r 
finplot(rolling_mid1, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```
A drawdown chart for mid cap shares was then attained. 
```r
chart.Drawdown(daily_mid$Daily_return,main = "Drawdowns: Mid caps in J203", 
               col = "steelblue")
```
A cumulative returns graph was formulated to ascertain whether the risk is worth the reward. My line of thinking was the same as in the large cap share example. 
```r 
ggplot(daily_mid, aes(x = Index, y = Cum_Ret)) + 
    geom_line() +
    theme_bw() +
    labs(title = "Mid cap shares in J203 cumulative Return", y = "Growth of R1 invested in 2005.")
```
# Small cap shares

The small cap share data was acquired from the Alsi dataset. The daily returns were calculated by multiplying the returns column with the J203 column.

```{r Small cap shares, include=FALSE}
small_caps <- Alsi |> 
    filter(Index_Name == "Small_Caps")
small_caps$Tickers <- gsub(" SJ|Equity","", small_caps$Tickers)
small <- contribution(small_caps)
small1 <- contributionsmall(small_caps)
daily_small <- daily_contri(small)
#colnames(daily_small)[1] <- "small_daily_return"
rolling_small <- rolling_contri(small)
roll <- rolling_SDcum(small_caps)
rolling_small1 <- rolling_SDcum(small_caps) |> 
    filter(RollSD < 0.4) |> 
    ggplot() +
    geom_line(aes(x = date, y = RollSD, color = Tickers), alpha = 0.7, size = 1.25) +
    labs(title = "Rolling 3 Year Annualized SD of various small cap stocks with differing start dates", subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", caption = "Note:\nDistortions are not evident now.") + 
    theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) +
    theme(legend.position="none") #+
    #theme(text=element_text(family="Garamond", size=14))
```
I then plotted the rolling 3 year annualised standard deviation of the small cap stocks. 

```r
finplot(rolling_small1, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```
A drawdown chart for mid cap shares was then attained. 
```r
chart.Drawdown(daily_small$Daily_return,main = "Drawdowns: Small caps in J203", 
               col = "steelblue")
```
A cumulative returns graph was formulated to ascertain whether the risk is worth the reward. My line of thinking was the same as in the large cap share example. 
```r
ggplot(daily_small, aes(x = Index, y = Cum_Ret)) + 
    geom_line() +
    theme_bw() +
    labs(title = "Small cap shares in J203 cumulative Return", y = "Growth of R1 invested in 2005.")
```

# Rolling Standard deviation

The datasets had to be manipulated so that they could be joined and later analysed. The "join" dataframe merged the large, mid and small cap data frames so that the rolling standard deviation of their daily return contribution to the J203 could be compared. 
```{r Join, include=FALSE}
ljoin <- large
colnames(ljoin)[2] <- "large_dailyreturn"
mjoin <- mid
colnames(mjoin)[2] <- "mid_dailyreturn"
sjoin <- small
colnames(sjoin)[2] <- "small_dailyreturn"
join <- list(ljoin, mjoin, sjoin) |>
    reduce(full_join, by='date') |> 
    gather(Index, Return, -date) |> 
    mutate(YM = format(date, "%Y%B")) |> 
    arrange(date) |> 
    group_by(Index, YM) |> 
    filter(date == last(date)) |> 
    group_by(Index) |> 
    select(date, Index, Return) |> 
    mutate(RollSD = RcppRoll::roll_sd(1 + Return, 36, fill = NA, align = "right") * sqrt(12)) |> 
    filter(!is.na(RollSD)) |> 
    ggplot() +
    geom_line(aes(x = date, y = RollSD, color = Index), alpha = 0.7, size = 1.25) +
    labs(title = "Illustration of Rolling 3 Year Annualized SD of various Indices with differing start dates", subtitle = "", x = "", y = "Rolling 3 year Returns (Ann.)", caption = "Note:\nDistortions are not evident now.") + 
    theme_fmx(title.size = ggpts(25), subtitle.size = ggpts(5), caption.size = ggpts(25), CustomCaption = T) #+
    #theme(text=element_text(family="Garamond", size=14))
```
It is evident that small cap stock returns have the lowest rolling standard deviation, followed by mid cap stocks. 
```r
finplot(join, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```
Static standard deviation for each of the stock sizes was deemed necessary and thus calculated. It is again evident that large cap stocks embody a higher standard deviation than mid and small cap stocks. 

```{r STD, include=FALSE}
largevol <- StdDev(large$large_daily_return)
midvol <- StdDev(mid$mid_daily_return)
smallvol <- StdDev(small$small_daily_return)

cat("Large volatility: ", largevol, "\n")
cat("Mid volatility: ", midvol, "\n")
cat("Small volatility: ", smallvol)
```
The results were then put into a table. 

\begin{table}[h]
\begin{center}
    \begin{tabular}{| c | c |}
    \hline
         & Standard deviation \\
        \hline
        Large cap & 0.01118614 \\
        Mid cap & 0.001418264 \\
        Small cap & 0.0002078111 \\
        \hline
    \end{tabular}
    \caption{Standard deviation}
    \label{tab:SD}
\end{center}
\end{table}

Volatility also needed to be analysed. This analysis was conducted with the help of the performance analytics package. 
```{r vol,include=FALSE}
vol_returns <- list(ljoin, mjoin, sjoin) |>
    reduce(full_join, by='date') |> 
    tbl_xts()
VolatilitySkewness(vol_returns, MAR = 0, stat = c("volatility"))
```
Again, a table was created to display the results. 

\begin{table}[h]
\begin{center}
    \begin{tabular}{| c | c | c | c |}
    \hline
         & Large cap & Mid cap & Small cap \\
        \hline
        Volatility skewness & 1.067183 & 0.9504131 & 0.9286573 \\
        \hline
    \end{tabular}
    \caption{Volatility skewness}
    \label{tab:VS}
\end{center}
\end{table}