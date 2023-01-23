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

```{r Figure1,  warning =  FALSE, fig.align = 'center', fig.cap = "Large rolling SD \\label{Figure1}", fig.ext = 'png', fig.height = 3, fig.width = 6}
finplot(rolling_large1, x.date.dist = "1 year", x.date.type = "%Y", x.vert = T, y.pct = T, y.pct_acc = 1)
```

```{r Figure2, warning =  FALSE, fig.align = 'center', fig.cap = "Large cap drawdown chart \\label{Figure2}", fig.height = 3, fig.width = 6, fig.ext = 'png'}
chart.Drawdown(daily_large$Daily_return,main = "Drawdowns: Large caps in J203", 
               col = "steelblue")
```

```{r Figure3, warning =  FALSE, fig.align = 'center', fig.cap = "Caption Here \\label{Figure3}", fig.height = 3, fig.width = 6, fig.ext = 'png'}
ggplot(daily_large, aes(x = Index, y = Cum_Ret)) + 
    geom_line() +
    theme_bw() +
    labs(title = "Large cap shares in J203 cumulative Return", y = "Growth of R1 invested in 2005.")
```
