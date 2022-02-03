#norm
norm_samp <- rnorm(100)
print(qqnorm(norm_samp))
print(abline( a= 0 , b =1, col= "blue"))


#endregion
sp500_px <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/sp500_data.csv.gz")

nflx <- sp500_px[,'NFLX']
nflx <- diff(log(nflx[nflx>0]))
print(qqnorm(nflx))
print(abline(a=0, b=1, col='grey'))

print(dbinom(x=2, size = 5, p =0.1))