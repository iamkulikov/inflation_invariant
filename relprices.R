#library(rJava)
#library(xlsx)
#install.packages(ggplot2)
#install.packages(reshape2)
#install.packages(igraph)
#install.packages(RColorBrewer)
library(ggplot2)
library(reshape2)
library(igraph)
library(RColorBrewer)

###### Задаем положение исходных файлов
setwd("D:/Dropbox/Projects/Economics_blog/inflation_invariant")
#fname="30years_small.csv"
fname="20years_big_average.csv"

###### Импортируем исходные данные
A = read.csv(fname, header = TRUE, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA"))
#colnames(A) <- c(1991:2020)
colnames(A) <- c(2000:2021)

###### Задаем служебные переменные и массив результатов
nprices <- dim(A)[[1]]
nyears <- dim(A)[[2]]
results <- array(dim=c(nprices,nprices,6))
drelprice <- array(dim=c(nprices,nprices,(nyears-1)))
#intersect_min <- 20
#intersect_min <- 15
#top <- 80

###### Определяем функции волатильности

maxmin <- function (x,y) {
	if ( mean(as.matrix(x/y), na.rm=T) > 1  & sum(!is.na(x*y)) >= intersect_min ) {
		r <- x/y
		output <- (max(r, na.rm=T) - min(r, na.rm=T))/mean(as.matrix(r), na.rm=T) } 
	else {
		output <- NA
	}
	return(output)
}

coefvar <- function (x,y) {
	if ( mean(as.matrix(x/y), na.rm=T) > 1  & sum(!is.na(x*y)) >= intersect_min ) {
		r <- x/y
		output <- sd(as.matrix(r), na.rm=T) / mean(as.matrix(r), na.rm=T) } 
	else {
		output <- NA
	}
	return(output)
}

dpricemod <- function (x,y) {
	if ( is.na(mean(as.matrix(x/y), na.rm=T)) ) { output <- rep(NA,nyears-1) } else {
	if ( mean(as.matrix(x/y), na.rm=T) > 1 ) {
		r <- x/y
		output <- as.matrix(abs(r[-1]/r[-length(r)]-1))[1,] } 
	else {
		output <- rep(NA,nyears-1)
	}
	}
	return(output)
}


###### Считаем волатильность относительных цен для всех возможных пар (во времени и суммарно)

for (i in 1:nprices) {
	for (j in 1:nprices) {
		#results[i,j,1] <- maxmin(A[i,], A[j,])
		#results[i,j,2] <- coefvar(A[i,], A[j,])
		#results[i,j,6] <- sum(!is.na(A[i,]*A[j,]))
		drelprice[i,j,] <- as.matrix(dpricemod(A[i,], A[j,]))
	}
}

###### Записываем результаты в файлы
#write.table(results[,,1], file="1_maxmin.csv", sep=";", row.names=F, col.names=F)
#write.table(results[,,2], file="2_coefvar.csv", sep=";", row.names=F, col.names=F)
#write.table(results[,,6], file="6_intersect.csv", sep=";", row.names=F, col.names=F)
for (i in 1:(nyears-1)) { 
	fnameout <- paste(colnames(A)[i+1],"_dpriceav.csv",sep="")
	write.table(drelprice[,,i], file=fnameout, sep=";", row.names=F, col.names=F)
	}

###### Выгружаем результаты из файлов
results <- array(dim=c(nprices,nprices,6))
results[,,1] = as.matrix(read.csv("1_maxmin.csv", header = F, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))
results[,,2] = as.matrix(read.csv("2_coefvar.csv", header = F, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))
results[,,6] = as.matrix(read.csv("6_intersect.csv", header = F, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))

###### Заполняем быстро вычисляемые листы
results[,,3] <- rank(results[,,1])
results[,,4] <- rank(results[,,2])
results[,,5] <- (results[,,3]+results[,,4])/2
#dim(results)
#dim(as.matrix(read.csv("1_maxmin.csv", header = F, sep = ";", quote = "\"",
#         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA"))))

###### Смотрим топ наименее волатильных относительных цен

# по разбросу
for (i in 1:top) {
	#print(which(results[,,1]==sort(results[,,1])[i], arr.ind=T))
	print(which(results[,,3]==i, arr.ind=T))
	print(results[,,3][results[,,3]==i])
	print(results[,,4][results[,,3]==i])
	print(results[,,5][results[,,3]==i])
}

# по коэффициенту вариации
for (i in 1:top) {
	#print(which(results[,,2]==sort(results[,,2])[i], arr.ind=T))
	print(which(results[,,4]==i, arr.ind=T))
	print(results[,,3][results[,,4]==i])
	print(results[,,4][results[,,4]==i])
	print(results[,,5][results[,,4]==i])
}

# по среднему рангу
for (i in 1:top) {
	print(which(results[,,5]==sort(results[,,5])[i], arr.ind=T))
	print(results[,,3][results[,,5]==sort(results[,,5])[i]])
	print(results[,,4][results[,,5]==sort(results[,,5])[i]])
	print(results[,,5][results[,,5]==sort(results[,,5])[i]])
}

###### Смотрим распределение коэффициента вариации

jpeg(filename = "coefvar_distr.jpg",
     width = 800, height = 800, units = "px", pointsize = 12,
     quality = 600,
     bg = "white")

hist(results[,,2],
     freq = NULL, breaks=80,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = "lightgray", border = NULL,
     main = "", ylim = NULL, xlim=c(0,1.2),
     xlab = "Коэффициент вариации относительной цены за 2000-2021 годы", ylab="Частота (одно наблюдение соответствует одной паре товаров)",
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, warn.unused = TRUE)

dev.off()

###### Фильтруем топ, чтобы пары товаров были интересные (из разных групп)

groups <- as.vector(read.csv("groups.csv", header = T, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))[[1]]
diff_out <- matrix(nrow=nprices, ncol=nprices)

for (i in 1:nprices) { for (j in 1:nprices) {
	if (groups[i] != groups[j]) {diff_out[i,j] = 1} else {diff_out[i,j] = NA} 
}}

results_out <- array(dim=c(nprices,nprices,6))

for (i in c(1,2,6)) {
	results_out[,,i] <- results[,,i]*diff_out
}

results_out[,,3] <- rank(results_out[,,1])
results_out[,,4] <- rank(results_out[,,2])
results_out[,,5] <- (results_out[,,3]+results_out[,,4])/2

###### Смотрим фильтрованный топ наименее волатильных межгрупповых относительных цен

# по разбросу
for (i in 1:top) {
	#print(which(results_out[,,1]==sort(results_out[,,1])[i], arr.ind=T))
	print(which(results_out[,,3]==i, arr.ind=T))
	print(results_out[,,3][results_out[,,3]==i])
	print(results_out[,,4][results_out[,,3]==i])
	print(results_out[,,5][results_out[,,3]==i])
}

# по коэффициенту вариации
for (i in 1:top) {
	#print(which(results_out[,,2]==sort(results_out[,,2])[i], arr.ind=T))
	print(which(results_out[,,4]==i, arr.ind=T))
	print(results_out[,,3][results_out[,,4]==i])
	print(results_out[,,4][results_out[,,4]==i])
	print(results_out[,,5][results_out[,,4]==i])
}

# по среднему рангу
for (i in 101:140) {
	print(which(results_out[,,5]==sort(results_out[,,5])[i], arr.ind=T))
	print(results_out[,,3][results_out[,,5]==sort(results_out[,,5])[i]])
	print(results_out[,,4][results_out[,,5]==sort(results_out[,,5])[i]])
	print(results_out[,,5][results_out[,,5]==sort(results_out[,,5])[i]])
}

###### Смотрим наименее волатильные относительные цены для конкретных товаров
# пиво 120-121, гроб 398, могила 686-687, кино 467, автомобиль 326-327, обруч кольцо 325,
# футб мяч 308, велосипед 293, сигареты 242-243, книга детектив 291, кирпич 317, валидол 346
# плацкарт 424-425

good = 425
for (i in 1:5) {
	print(which(results_out[good,,5]==sort(results_out[good,,5])[i], arr.ind=T))
	print(results_out[good,,1][results_out[good,,5]==sort(results_out[good,,5])[i]])
	print(results_out[good,,2][results_out[good,,5]==sort(results_out[good,,5])[i]])
	print(results_out[good,,5][results_out[good,,5]==sort(results_out[good,,5])[i]])
}

for (i in 1:5) {
	print(which(results_out[,good,5]==sort(results_out[,good,5])[i], arr.ind=T))
	print(results_out[,good,1][results_out[,good,5]==sort(results_out[,good,5])[i]])
	print(results_out[,good,2][results_out[,good,5]==sort(results_out[,good,5])[i]])
	print(results_out[,good,5][results_out[,good,5]==sort(results_out[,good,5])[i]])
}


###### Делаем множество вгутригрупповых относительных цен

groups <- as.matrix(read.csv("groups.csv", header = T, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))[,2]
diff_in <- matrix(nrow=nprices, ncol=nprices)

for (i in 1:nprices) { for (j in 1:nprices) {
	if (groups[i] == groups[j]) {diff_in[i,j] = 1} else {diff_in[i,j] = NA} 
}}

results_in <- array(dim=c(nprices,nprices,6))

for (i in c(1,2,6)) {
	results_in[,,i] <- results[,,i]*diff_in
}

results_in[,,3] <- rank(results_in[,,1])
results_in[,,4] <- rank(results_in[,,2])
results_in[,,5] <- (results_in[,,3]+results_in[,,4])/2


###### Проверяем гипотезу, что внутригрупповые относительные цены меняются слабее
## Два распределения - внутигрупповой коэффициент вариации vs межгрупповой

dens_out <- density(results_out[,,2], na.rm=T)
dens_in <- density(results_in[,,2], na.rm=T)

jpeg(filename = "coefvar_distr_groups.jpg",
     width = 800, height = 800, units = "px", pointsize = 12,
     quality = 600,
     bg = "white")

plot(dens_out, col="green", ylim=c(0,5.5), xlim=c(0,1), lwd = 3,
	 xlab="Коэффициент вариации относительной цены за 2000-2021 годы", ylab="Плотность распределения",
	main="")
lines(dens_in, col="red", lwd = 3)
legend("topright", legend = c("межгрупповые (mean=0,234)", "внутригрупповые (mean=0,177)"), lwd = 3, col = c("green", "red"),
	inset = 0.05, bty="n")

dev.off()

###### Рассчитываем средний прирост относительных цен по годам и сравниваем с инфляцией
dpriceav = rep(NA,nyears-1)
dpricequant = matrix(nrow = 9, ncol = nyears-1, dimnames=list(seq(0.1,0.9,0.1),c(2001:2021)) )

for (i in 1:(nyears-1)) {
	dpriceav[i] = mean(as.matrix(drelprice[,,i]), na.rm=T)
	dpricequant[,i] = quantile(as.matrix(drelprice[,,i]), probs = seq(0.1,0.9,0.1), na.rm=T)
}

write.table(dpriceav, file="dpriceav_big.csv", sep=";", row.names=F, col.names=F)

inflation = as.vector(read.csv("cpi.csv", header = F, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))

jpeg(filename = "dpriceav_inflation.jpg",
     width = 800, height = 800, units = "px", pointsize = 12,
     quality = 600,
     bg = "white")

plot(inflation, type="l", ylim=c(0,0.2), xaxt = "n", col= "black", lty="solid", lwd = 3,
	 xlab="", ylab="")
axis(1, at = c(1:21), labels=c(2001:2021))
lines(dpriceav, type="l", col= "blue", lty="dashed", lwd = 3)
legend("topright", legend = c("Среднегодовая инфляция", "Средний модуль прироста относительных цен, г/г"), lwd = 3, col = c("black", "blue"),
	inset = 0.05, bty="n")

dev.off()

###### График квантилей распределения приростов относительной цены во времени

jpeg(filename = "quantile_dprice.jpg",
     width = 800, height = 800, units = "px", pointsize = 12,
     quality = 600,
     bg = "white")

plot(dpriceav, type="l", ylim=c(0,0.3), xaxt = "n", col= "blue", lty="dashed", lwd = 3,
	 xlab="", ylab="Квантили годового прироста относительных цен (по модулю)")
axis(1, at = c(1:21), labels=c(2001:2021))

palette <- colorRampPalette(colors=c("#444444", "#FFFFFF"))
cols <- c(rev(palette(6)[2:5]),palette(6)[2:5])

for (j in 1:8) {
	polygon(x = c(1:21,21:1), 
	y = c(dpricequant[j+1,], rev(dpricequant[j,])),
	col=cols[j], border=F)
}

for (j in 1:9) {
	lines(dpricequant[j,], lwd = 5-abs(j-5) )
}

lines(dpriceav, type="l", col= "blue", lty="dashed", lwd = 3)

dev.off()

###### График распределения приростов относительной цены

plot(density(drelprice[,,1], na.rm=T), xlim=c(0,0.5), ylim=c(0,15)) 
for (i in 2:(nyears-1)) {
	lines(density(drelprice[,,i], na.rm=T))
	Sys.sleep(0.2)
}


####### График сети относительных цен

nodes = data.frame(read.csv("groups.csv", header = T, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))

colnames(nodes)[1] <- "id"

edges <- data.frame(melt(results[,,2]))
colnames(edges)=c("from","to","weight")
edges <- edges[!is.na(edges$weight),]
edges <- edges[edges$weight<=0.3,]

nodes <- nodes[nodes$id %in% edges$from | nodes$id %in% edges$to,]

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F) 

cols <- c(brewer.pal(12, "Set3"),brewer.pal(9, "Set1"))

V(net)$size <- 2
V(net)$frame.color <- "white"
#V(net)$color <- "orange"
V(net)$color <- cols[V(net)$type]
V(net)$label <- "" 
E(net)$arrow.mode <- 0

#lw <- layout_with_fr(net, weights=edges$weight)
lw <- layout_with_kk(net, weights=edges$weight)

jpeg(filename = "network_kernel.jpg",
     width = 800, height = 800, units = "px", pointsize = 12,
     quality = 600,
     bg = "white")

plot(net, layout=lw)

dev.off()
