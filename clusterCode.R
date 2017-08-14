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
alst1 <- alst1*10^(-4) #Changing from m to km
d <- dist(alst1,method = 'euclidean')
fit <- hclust(d,method = 'ward.D')
plot(fit,
     main = '', ylab = 'Relative Distance (km)',
     xlab='Plot codes',)
## abline(1500,0,lty = 2,col = 'black')
abline(8,0,lty = 2,col = 'red')
nod <- data.frame(x = c(2.05,6.35,8.95,16.7), y = rep(10.5,4))
lab <- as.character(1:4)
text(nod, lab, pos = 2, cex = 0.8, col = 'red')
## circular cluster
## library('ape')
## plot(as.phylo(hclust(d, method = 'ward.D')),type="fan")
path.. <- '/home/wilar/Documents/isoPinaster'
dev.copy(pdf, paste(path..,'clust.pdf', sep = '/'))
dev.off()

