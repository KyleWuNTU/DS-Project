session_times <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/web_page_data.csv")
print(ggplot(session_times, aes(x = Page, y = Time)) + geom_boxplot())
mean_a <- mean(session_times[session_times["Page"] =="Page A", "Time"])
mean_b <- mean(session_times[session_times["Page"] =="Page B", "Time"])
print(mean_b - mean_a)

#Permutation Testing
perm_fun <- function(x, nA, nB){
  n <- nA + nB
  idx_b <- sample(1:n, nB)
  idx_a <- setdiff(1:n, idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return (mean_diff)
}

perm_diffs <- rep(0,1000)
for (i in 1:1000){
  perm_diffs[i] = perm_fun(session_times[,"Time"], 21, 15)
}

print(hist(perm_diffs, xlab = "Session time differences (in seconds)"))
print(abline(v = mean_b - mean_a))
print(mean(perm_diffs > (mean_b - mean_a)))

#以上結果表示，頁面 A 和頁面 B 之間觀察到的工作階段時間差異恰好落在機遇變異的範圍內，所以無法拒絕虛無假設
#唯有在測量便變異值位在分佈以外才能代表有統計顯著性，不然就有可能只是因為機率使然。
print(t.test(Time ~Page, data = session_times, alternative= "less"))

four_sessions <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/four_sessions.csv")
print(summary(aovp(Time ~ Page, data = four_sessions)))

click_rate <- read.csv("/Users/kylewu/Downloads/practical-statistics-for-data-scientists/data/click_rates.csv")
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=TRUE)
#卡方檢定（獨立性檢定）如果得到的 p-value 比 significance level 小，則不具有獨立性，反之則說迷結果是由隨機性所獲得的。
print(chisq.test(clicks))

#Fisher 精確性檢定
print(fisher.test(clicks))

#檢定力
effect_size = ES.h(p1 = 0.0121, p2 =0.011)
print(pwr.2p.test(h = effect_size, sig.level = 0.05, power = 0.8, alternative = "greater"))
