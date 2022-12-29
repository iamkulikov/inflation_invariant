#install.packages(ggplot2)
#install.packages(reshape2)
#install.packages(igraph)
#install.packages(RColorBrewer)

library(ggplot2)
library(reshape2)
library(igraph)
library(RColorBrewer)

# Хотим во множестве всех веторов (приросты цен г/г за 21 лет) выбрать минимальное 
# подмножество векторов (5-6), линейные комбинации которых дадут остальные вектора
# с преемлемой точностью. То есть 21-мерную информацию можно заменить 5-6 мерной
# при условии, что "базисные" вектора заданы.

# Первый вариант. Проецируем 

###### Задаем положение исходных файлов
#setwd("D:/Dropbox/Projects/Economics_blog/inflation_invariant")
setwd("C:/Projects/inflation_invariant")
#fname="30years_small.csv"
fname="20years_big_average.csv"

###### Импортируем исходные данные
A = read.csv(fname, header = TRUE, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA"))
#colnames(A) <- c(1991:2020)
colnames(A) <- c(2000:2021)

###### Рассчитываем приросты 
dprice <- A[,-1]/A[,-dim(A)[2]]-1
#dprice <- as.data.frame(t(as.matrix(dpricet)))
#str(dprice)

#plot(density(dprice[,1], na.rm=T), xlim=c(-0.5, 0.9), ylim=c(0, 10))
#for (i in 2:dim(dprice)[2]) {
#	lines(density(dprice[,i], na.rm=T))
#}

###### Минус вектора с NA
dprice_narrow <- na.omit(dprice)
#dprice_narrow <- dprice[ , colSums(is.na(dprice)) == 0]
#str(dprice_narrow)
#dprice[1,]

###### Главные компоненты
P <- prcomp(dprice_narrow, center = TRUE, scale = TRUE)
summary(P)
#print(P)
#P$x[1,]
#P$center
#P$scale
#P$rotation

###### Нормируем исходные вектора, чтобы можно было искать
###### среди них наиболее близкие к главным компонентам

dprice_narrow_c <- dprice_narrow - rep(P$center, each = nrow(dprice_narrow))
dprice_narrow_cn <- dprice_narrow_c / rep(P$scale, each = nrow(dprice_narrow_c))
# P$x[1,] - это то же самое, что и
# sum(dprice_narrow_cn[1,]*P$rotation[,1])

###### Ищем среди dprice_narrow_cn приблизительно коллинеарные с P$rotation[,i]

bestprice = matrix(nrow = length(dprice), ncol = 2)
err = rep(NA, dim(dprice_narrow_cn)[1])
err1 = rep(NA, dim(dprice_narrow_cn)[1])
err2 = rep(NA, dim(dprice_narrow_cn)[1])

for (j in 1:dim(bestprice)[1]) {
	for (i in 1:length(err) ) {
		err1[i] = sum((P$rotation[,j]/norm(P$rotation[,j], type="2") - dprice_narrow_cn[i,]/norm(dprice_narrow_cn[i,], type="2"))^2)
		err2[i] = sum((P$rotation[,j]/norm(P$rotation[,j], type="2") + dprice_narrow_cn[i,]/norm(dprice_narrow_cn[i,], type="2"))^2)
		err[i] = min(err1[i],err2[i])
	}
	bestprice[j,1] = as.numeric(rownames(dprice_narrow[which(err==min(err)),]))
	bestprice[j,2] = min(err)
}

bestprice


# Невзвешенное среднее приростов с инфляцией как соотносится?
inflation <- as.vector(read.table("cpi.csv", header = F, sep = ";", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"...","","NA")))$V1
inflation[1] <- 0.216882926
inflation <- as.numeric(inflation)

plot(P$center, type="l", col="blue")
lines(inflation, col="red")


