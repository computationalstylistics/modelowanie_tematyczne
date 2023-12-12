



``` R
library(topicmodels)
library(wordcloud)
library(mgcv)

load("topic_model_k-100.RData")
```



``` R
model_weights = posterior(topic_model)
topic_words = model_weights$terms
doc_topics = model_weights$topics

chunk_IDs = readLines("chunk_IDs.txt")
book_IDs = unique(gsub("_[0-9]{3}$", "", chunk_IDs))
```


``` R
png(filename = "rys-1_slowozbiory.png", width = 6, height = 10, res = 600, units = "in")

no_of_words = 50

op = par(mfrow = c(3, 2),
          oma = c(0, 0, 0, 0),
          #oma = c(4, 4, 0, 0),
          #mar = c(1, 1, 1, 1) )
          mar = c(0, 0, 0, 0) )

#10, 72, 73, 80, 100, 53, 62, 21

for(topic_id in c(10, 72, 80, 100, 53, 62)) {
    current_topic = sort(topic_words[topic_id,], decreasing = T)[1 : no_of_words]
    #png(file = paste("topic_", topic_id, ".png", sep = ""))
    wordcloud(names(current_topic), current_topic, scale=c(4, 0.5), random.order = FALSE, rot.per = 0)
    #dev.off()
}
par(op)
dev.off()
```



``` R
png(filename = "rys-2_proporcje_topikow.png", width = 6, height = 4, res = 600, units = "in")

no_of_words = 50

op = par(mfrow = c(1, 2),
          oma = c(2, 2, 2, 2),
          mar = c(1, 1, 1, 1) )

x = doc_topics[916,]
x[x < 0.005] = NA
plot(x, type = "h", ylim = c(0, 0.6), col = rgb(0, 0, 0, 0.6), lwd = 3, xlab = "numer słowozbioru", ylab = "udział danego słowozbioru")

x = doc_topics[2732,]
x[x < 0.005] = NA
plot(x, type = "h", ylim = c(0, 0.6), col = rgb(0, 0, 0, 0.5), lwd = 3, axes = FALSE)
axis(1)
box()
par(op)
dev.off()
```




``` R
png(filename = "rys-3_simpson.png", width = 6, height = 5, res = 600, units = "in")
# Simpson's index:
#plot(rowSums(doc_topics ^2))

simpson_index = rowSums(doc_topics ^2)
groups = gsub("_.*", "", chunk_IDs)
simpson_index_per_book = split(x = simpson_index, f = groups)

mean_simpson_index_per_book = sapply(simpson_index_per_book, mean)
top_and_bottom = names(sort(mean_simpson_index_per_book))[c(1:5,95:100)]
top_and_bottom = as.numeric(gsub("POL", "", top_and_bottom))
titles = gsub("POL[0-9]+_", "", book_IDs[top_and_bottom])

titles_with_gap = c("A", "B", "C", "D", "E", "...", "...", "U", "V", "W", "X", "Y", "Z")

j = 0
short_list = list()
for(i in top_and_bottom) {
    j = j + 1
    if(j == 6) {j = 8}
    short_list[[j]] = simpson_index_per_book[[i]]
}

boxplot(short_list, axes = FALSE, ylab = "koherencja tematyczna", xlab = "badane powieści")
axis(2)
axis(1, at = 1:13, labels = titles_with_gap, las = 2)
box()

dev.off()
```




``` R
png(filename = "rys-4.png", width = 6, height = 5, res = 600, units = "in")
novels = c(9, 25, 79, 50, 54, 97)
op <- par(mfrow = c(3, 2),
          oma = c(4, 4, 0, 0),
          mar = c(1, 1, 1, 1) )
for(i in novels) {
    current_book = as.numeric(simpson_index_per_book[[i]])
    plot(current_book)
    model_gam = gam((current_book) ~ s(c(1:length(current_book)), bs = "cr") )
    lines(model_gam$fitted.values, col = rgb(1, 0, 0, 0.6), lwd = 3)
}

title(xlab = "",
      ylab = "koherencja tematyczna",
      outer = TRUE, line = 3)

par(op)
dev.off()
```



``` R
png(filename = "rys-5.png", width = 6, height = 5, res = 600, units = "in")
novels = c(61, 39, 64, 66, 71, 93)
op <- par(mfrow = c(3, 2),
          oma = c(4, 4, 0, 0),
          mar = c(1, 1, 1, 1) )

for(i in novels) {
    current_book = as.numeric(simpson_index_per_book[[i]])
    plot(current_book)
    model_gam = gam((current_book) ~ s(c(1:length(current_book)), bs = "cr") )
    lines(model_gam$fitted.values, col = rgb(1, 0, 0, 0.6), lwd = 3)
}
par(op)
dev.off()
```

