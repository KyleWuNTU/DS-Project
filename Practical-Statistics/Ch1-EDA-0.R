#Ch1

state <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/state.csv")
print(state)
print(mean(state[["Population"]], trim = 0.1))
print(mean(state[["Population"]]))
print(weighted.mean(state[["Murder.Rate"]], w = state[["Population"]]))
print(sd(state[["Population"]]))
print(mad(state[["Population"]]))
print(quantile(state[["Murder.Rate"]], p = c(.05, .25, .5, .75, .95)))
print(boxplot(state[["Population"]]/1000000, ylab = "Population (millions)"))

#table of frequency
breaks <- seq(from = min(state[["Population"]], to =max(state[["Population"]], length = 11)))
pop_freq <- cut(state[["Population"]], breaks = breaks, right = TRUE, include.lowest = TRUE)
table(pop_freq)

#histogram
print(hist(state[["Population"]]))

#Density
print(hist(state[["Murder.Rate"]], freq = FALSE)) #freq = FALSE said that we want percent
print(lines(density(state[["Murder.Rate"]]), lwd =3, col = "blue"))

#Correlation
#two variables
sp500_px <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/sp500_data.csv.gz")
sp500_sym <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/sp500_sectors.csv")

etfs <- sp500_px[row.names(sp500_px) > '2012-07-01', sp500_sym[sp500_sym$sector == "etf","symbol"]]
library(corrplot)
print(corrplot(cor(etfs), method = "ellipse")) #heatmap

#multiple variable
kc_tax <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/kc_tax.csv.gz")
kx_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving >100 & SqFtToTLiving <3500)
nrow(kc_tax0)
