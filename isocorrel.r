setwd('/home/wilar/Documents/Treerings/Isotopes/Crossdating/')
stell <- read.csv('Stella.master.csv',row.names = 1)
tail(stell)
## Importing data sets
ccodes <- read.csv('cover_core_codes.csv',header = TRUE)
allcr <- read.csv('Isocoords.csv') 
Allcr <- read.csv('Allcoords.csv',sep = '\t')
Allst <- subset(Allcr,sampled == 's')[,2:5]
pinsr <- with(allcr,grep('P',site))
Pinsr <- with(Allst,grep('P',site))
Alst <- Allst[Pinsr,]
alst <- allcr[pinsr,]
Alst[,'site'] <- with(Alst,paste('m_',site,sep = ''))
rownames(alst) <- alst[,'site']
rownames(Alst) <- Alst[,'site']
alst1 <- rbind(alst,Alst)[,-1L]
alst1 <- alst1*10^(-3)
d <- dist(alst1,method = 'euclidean')
fit <- hclust(d,method = 'ward.D')
plot(fit,
     main = '', ylab = 'Distance (km)',
     xlab='Sample plot codes',)
abline(1500,0,lty = 2,col = 'black')
abline(80,0,lty = 2,col = 'red')
dend1 <- as.dendrogram(fit)

