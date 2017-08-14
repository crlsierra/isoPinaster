require('R.utils')
## packs<-c('tiff','png','pastecs','dplR')
## path. <- '~/Documents/Treerings/BIOdevel/BIOdry/R'
sourceDirectory(path., modifiedOnly=FALSE)
packs<-c('measuRing','dplR','BIOdry')
sapply(packs,require,character.only=TRUE)
## loadImage <- '/home/wilar/Documents/Treerings/Isotopes/Crossdating/count2.RData'
## ## ## save.image(loadImage)
## load(loadImage) ## contains allcounts1 object: list of TRW data
loadMaster <- '/home/wilar/Documents/Treerings/Isotopes/Crossdating/Stella.master.csv'
stell <- read.csv(loadMaster,row.names = 1)
loadCodes <- '/home/wilar/Documents/Treerings/Isotopes/Crossdating/cover_core_codes.csv'
ccodes <- read.csv(loadCodes) 
loadSpi <- '/home/wilar/Documents/Treerings/Isotopes/__allcompleteresultsagain/spiFrame.csv'
loadIso <- '/home/wilar/Documents/Treerings/Isotopes/__allcompleteresultsagain/isoFrame.csv'
isoes <- read.csv(loadIso)
spies <- read.csv(loadSpi)

lapply(spies, class)


shiftFrame(isoes)

rd <- spies
dsp <- slitFrame(rd)
which.x = NULL
un = NULL
nmx <- names(rd)
n. <- sapply(rd,is.numeric)
nux <- names(rd[,nmx[n.]])
nfx <- names(rd[,!nmx%in%nux])
nux1 <- nux[1]
if(!is.null(which.x))nux1 <- which.x
rd <- rd[,c(nux1,'year',nfx)]
names(rd) <- c('x','year',nfx)

dsp <- lapply(dsp,function(x)x[,c('year','x')])
for(i in 1:length(dsp))
    names(dsp[[i]]) <- c('year',names(dsp[i]))
fmatch <- function(tomatch.){
    Reduce(function(x,y){
        merge(x,y,all = TRUE)},tomatch.)}
rP <- fmatch(dsp)
rownames(rP) <- rP[,'year']
dt <- rP[,names(rP)[-1L]]
dt <- dt[,order(names(dt))]
if(lu == 2)
    dt <- chun(un[1],un[2]) * dt
attributes(dt)[['nmLong']] <- rev(nfx)
  

?BIOdry




tail(stell)
data(Prings05)


rd <- spies

        rd <- rd[,cClass(rd, 'all')] #MEDS ordering
        nfx <- cClass(rd,'factor')
        nux <- cClass(rd, 'numeric')
        dsp <- split(rd, rd[,rev(nfx)], drop = TRUE)
        length(dsp)
        dsp <- lapply(dsp,function(x)x[,c('year','x')])
        for(i in 1:length(dsp))
            names(dsp[[i]]) <- c('year',names(dsp[i]))
        fmatch <- function(tomatch.){
            Reduce(function(x,y){
                merge(x,y, by = 'year', all = TRUE)},tomatch.)}
        rP <- fmatch(dsp)
        rownames(rP) <- rP[,'year']
        dt <- rP[,names(rP)[-1L]]
        dt <- dt[,order(names(dt))]



spiesfr <- shiftFrame(spies) 
formals(shiftFrame)

sapply(spies, class)
cClass(spies)


data(Prings05)
lev.nm <- 'plot'
which.x <- NULL
un <- NULL

rd <- Prings05

loadP <- '/home/wilar/Documents/Treerings/Isotopes/Resilience/mpr.RData'
loadT <- '/home/wilar/Documents/Treerings/Isotopes/Resilience/mtm.RData'
loadC <- '/home/wilar/Documents/Treerings/Isotopes/Resilience/Pclimate.RData'
load(loadC)



## load(loadT);load(loadP)
## head(mpr)
## head(mtm)

## names(mpr) <- c('prec', names(mpr)[-1L])
## names(mtm) <- c('temp', names(mpr)[-1L])
## dir()
## getwd()

## head(mpr)
## lclp <- split(mpr, mpr[,c('plot','year')])
## lclp1 <- lapply(lclp, function(x)x[,c('year','month','x')])
## lclt <- split(mtm, mtm[,c('plot','year')])
## lclt1 <- lapply(lclt, function(x)data.frame(temp = x[,'x']))
## lclt2 <- lapply(lclt, function(x)x[,c('year','month','x')])
## lclp1 <- lclp1[names(lclt)]
## lcpt <- Map(function(x,y)cbind(x,y),lclp1,lclt1)
## lcpt1 <- Map(function(x,y)merge(x,y,by = c('year','month'), all.x = TRUE), lclp1, lclt2)
## lcpto <- lapply(lcpt1, function(x)
##     x[order(as.numeric(x$'month')),])
## Pclimate <- do.call('rbind',lcpto)
## names(Pclimate) <- c(names(Pclimate)[1:2],'prec','temp')
## Pclimate[,'plot'] <- substr(rownames(Pclimate),1,6)
## rownames(Pclimate) <- NULL

## cdss <- names(lclp1)[sample(1:length(lclp1),1)]
## lclp1[cdss]
## lclt2[cdss]
## lcpto[cdss]

## subi <- subset(ccodes, !icode%in%'')
## subi1 <- as.character(subi[,'code'])
## subi2 <- as.character(subi[,'icode'])

isoCounts <- allcounts1[subi1] 
## names(isoCounts) <- subi2

setwd('/home/wilar/Documents/Treerings/Isotopes/Newpics/')

subdt <- function(lst, patt, from.to = c(1,3))
{
    ## this function subsets the 'lst' list by matching pattern in
    ## 'patt' argument; from.to is used for matching subpatterns
    if(!is.null(from.to))
        patt <- substr(patt, from.to[1], from.to[2])
    nml <- names(lst)[grep(patt, names(lst))]
    return(lst[nml])
    }

str(p42.rwi)

names(isoCounts)

tocmp. <- 'P42_03'
yrs <- max(isoCounts[[tocmp.]][['colNames']][,1])
inclu.dats <- isoCounts[[tocmp.]][['colNames']]
## yrs <- -1
ctmp <- Map(function(x)
    multiDetect(x,
                auto.det = FALSE,
                last.yr = yrs,
                ## from = tocmp.,
                ## to = tocmp.,
                plot = TRUE,
                segs = 4,
                inclu.dat = inclu.dats,
                rgb = c(0,0,1), marker = 6),tocmp.)
graphics.off()

isoCounts_back <- isoCounts
## isoCounts <- isoCounts_back
isoCounts[tocmp.] <- ctmp

## tocmp. <- 'P42_04'
fun. <- 'ccf'
fun. <- 'spag'
fun. <- 'corr'
plotDesk <- F
crossRings(isoCounts[names(subdt(isoCounts, tocmp.))],
           tocmp = tocmp.,
           match.str = c(1,3),
           from.to = NULL,
           ## from.to = c(1:4,9:17),
           fun = fun.,
           seg.length = 8,
           bin.floor = 0,
           lag.max = 3,
           pcrit = 0.15,
           master = stell[names(subdt(stell, tocmp., from.to = c(1,3)))] 
           )

dir()
tofi <- '/home/wilar/Desktop/Pccf.pdf'
if(plotDesk){
dev.copy(pdf,tofi)
dev.off()
graphics.off()}

## dplR statisticsf and plots
mst <- stell[names(subdt(stell, tocmp.))]
p16 <- reduceList(isoCounts[names(subdt(isoCounts, 'P16'))])
p42 <- reduceList(isoCounts[names(subdt(isoCounts, 'P42'))])

## require('BIOdry')
## path. <- '~/Documents/Treerings/BIOdevel/BIOdry/R'
## sourceDirectory(path., modifiedOnly=FALSE)

iP <- shiftFrame(isoes)
row.names(iP) <- as.numeric(row.names(iP)) - 2014
ip16 <- iP[, grep('P16', names(iP))]
ip42 <- iP[, grep('P42', names(iP))]

p16.rwi <- detrend(rwl = p16, method = 'ModNegExp')
p42.rwi <- detrend(rwl = p42, method = 'ModNegExp')
ip16.rwi <- detrend(rwl = ip16, method = 'Friedman')
ip42.rwi <- detrend(rwl = ip42, method = 'Friedman')

p16ch <- chron(p16.rwi)
p42ch <- chron(p42.rwi)
ip16ch <- chron(ip16.rwi)
ip42ch <- chron(ip42.rwi)

## par(mfrow = c(1,2))
## plot(p42ch,
##      xlab = 'Time (from 2013)',
##      ## ylab = expression(paste(delta^{13}, "C")),
##      main = 'North',
##      add.spline = TRUE)
## par(new = TRUE)
## plot(p16ch,
##      xlab = 'Time (from 2013)',
##      ## ylab = expression(paste(delta^{13}, "C")),
##      main = 'South',
##      add.spline = TRUE)

## tois <- '/home/wilar/Desktop/IsoPaper/RWINorthCenter.pdf'
## dev.copy(pdf,tois)
## dev.off()
## graphics.off()


indyr <- function(p16ch, last.yr = 2014){
nm    <- last.yr + as.numeric(rownames(p16ch))
rownames(p16ch) <- nm
return(p16ch)
}

p161 <- indyr(p16)
p421 <- indyr(p42)
p16ch1 <- indyr(p16ch)
p42ch1 <- indyr(p42ch)

ip161 <- indyr(ip16)
ip421 <- indyr(ip42)

ip16ch1 <- indyr(ip16ch)
ip42ch1 <- indyr(ip42ch)

rd <- shiftFrame(spies)
rd <- shiftFrame(Prings05)
## tmp <- head(shiftFrame(rd, from = 'mm', to = 'cm'))




## dplR statisticsf and plots

spies16 <- dt[3:nrow(dt),grep('P16', names(dt))][,c(1:3)]
spies42 <- dt[3:nrow(dt),grep('P42', names(dt))][,c(1:2)]
spies16.rwl <- chron(spies16, prefix = 'SPI')
spies42.rwl <- chron(spies42, prefix = 'SPI')
## spies16.rwl <- detrend(spies16, method = 'Spline')
## spies42.rwl <- detrend(spies42, method = 'Spline')
spies16.ch <- chron(spies16.rwl, prefix = 'SPI')
spies42.ch <- chron(spies42.rwl, prefix = 'SPI')

p161 <- chron(p161)[rownames(p161)%in%rownames(spies16),]
p421 <- chron(p421)[rownames(p421)%in%rownames(spies42),]

as.numeric(row.names(spies16))
dt4coh <- function(chr){
    as.matrix(data.frame(year = as.numeric(row.names(chr)),chr[,1]))
}
require('biwavelet')
coher16 <- wtc(dt4coh(chron(p161)),dt4coh(spies16.ch), nrands = 100) 
coher42 <- wtc(dt4coh(chron(p421)),dt4coh(spies42.ch), nrands = 100)
par(mfrow = c(2,1),oma = c(0,0,0,1), mar = c(5,4,4,5) + 0.1)
plot(coher42, plot.cb = TRUE, main = 'north-cental Spain')
plot(coher16, plot.cb = TRUE, main = 'east Spain')

path.. <- '/home/wilar/Documents/isoPinaster'
dev.copy(pdf, paste(path..,'coherence1.pdf', sep = '/'))
dev.off()

## icoher16 <- wtc(dt4coh(chron(ip161)),dt4coh(spies16.ch), nrands = 100) 
## icoher42 <- wtc(dt4coh(chron(ip421)),dt4coh(spies42.ch), nrands = 100)
## par(mfrow = c(2,1),oma = c(0,0,0,1), mar = c(5,4,4,5) + 0.1)
## plot(icoher42, plot.cb = TRUE, main = 'north-cental Spain')
## plot(icoher16, plot.cb = TRUE, main = 'east Spain')





wtc.sig(coher16)

tois <- '/home/wilar/Desktop/IsoPaper/coherence1.pdf'
dev.copy(pdf,tois)
dev.off()
graphics.off()


## cr16 <- cor(t(spies16.rwl), method = 'pearson')
## cr42 <- cor(t(spies42.rwl), method = 'pearson')

## cr16[lower.tri(cr16, diag = TRUE)] <- NA
## cr42[lower.tri(cr42, diag = TRUE)] <- NA
## cr16 <- as.table(cr16)
## cr42 <- as.table(cr42)




par(mfrow = c(2,2))
plot(chron(ip421),
     add.spline = TRUE,
     ylab = expression(paste(delta^{13}, "C")),
     xlab = 'year')
par(mfg=c(1,2))
plot(ip42ch1,
     ylab = expression(paste(delta^{13}, "C index")),
     add.spline = TRUE,
     xlab = 'year')
par(mfg=c(2,1))
## par(new = TRUE)
plot(chron(ip161),
     ylab = expression(paste(delta^{13}, "C")),
     add.spline = TRUE,
     xlab = 'year')
par(mfg = c(2,2))
plot(ip16ch1,
     ylab = expression(paste(delta^{13}, "C index")),
     add.spline = TRUE,
     xlab = 'year')

par(mfrow = c(2,2))
plot(chron(p421),
     add.spline = TRUE,
     ylab = 'Tree-ring width (mm)',
     xlab = 'year')
par(mfg=c(1,2))
plot(p42ch1,
     add.spline = TRUE,
     xlab = 'year')
par(mfg=c(2,1))
## par(new = TRUE)
plot(chron(p161),
     ylab = 'Tree-ring width (mm)',
     add.spline = TRUE,
     xlab = 'year')
par(mfg = c(2,2))
plot(p16ch1,
     add.spline = TRUE,
     xlab = 'year')


tois <- '/home/wilar/Desktop/IsoPaper/ideltas.pdf'
dev.copy(pdf,tois)
dev.off()
graphics.off()


require('bootRes')
## needs monthly climatic data
Pclimate. <- '/home/wilar/Documents/Treerings/Isotopes/Resilience/Pclimate.RData'
load(Pclimate.)

head(Pclimate)
unique(Pclimate$'plot')
tmp <- Pclimate[Pclimate$'plot'%in%'P42206', -5L]
na.omit(tmp)
spani <- 1971:2007
p111 <- p42ch1[rownames(ip42ch1)%in%spani,]
pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
head(pttt)
p16dcc <- dcc(p111, pttt, start = -6) ## error
par(mfrow = c(2,2))
dcplot(p16dcc)
par(mfg=c(1,1))
title('TRW (north)', line = 2)
## par(new = TRUE)
%%
unique(Pclimate$'plot')
tmp <- Pclimate[Pclimate$'plot'%in%'P16008', -5L]
na.omit(tmp)
spani <- 1971:2007
p111 <- p16ch1[rownames(p16ch1)%in%spani,]
pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
head(pttt)
p16dcc <- dcc(p111, pttt, start = -6) ## error
par(mfg=c(2,1))
dcplot(p16dcc)
par(mfg=c(2,1))
title('TRW (east)', line = 2)
%%
## tois <- '/home/wilar/Documents/isoPinaster/GrowthFunRes.pdf'
## dev.copy(pdf,tois)
## dev.off()
## graphics.off()
%%
unique(Pclimate$'plot')
tmp <- Pclimate[Pclimate$'plot'%in%'P42206', -5L]
na.omit(tmp)
spani <- 1971:2007
p111 <- ip42ch1[rownames(ip42ch1)%in%spani,]
pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
head(pttt)
p16dcc <- dcc(p111, pttt, start = -6) ## error
par(mfg=c(1,2))
dcplot(p16dcc)
par(mfg=c(1,2))
title('LWSC (north)', line = 2)
%%
unique(Pclimate$'plot')
tmp <- Pclimate[Pclimate$'plot'%in%'P16008', -5L]
na.omit(tmp)
spani <- 1971:2007
p111 <- ip16ch1[rownames(p16ch1)%in%spani,]
pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
head(pttt)
p16dcc <- dcc(p111, pttt, start = -6) ## error
par(mfg=c(2,2))
dcplot(p16dcc)
title('LWSC (east)', line = 2)
%%
tois <- '/home/wilar/Documents/isoPinaster/FunRes.pdf'
dev.copy(pdf,tois)
dev.off()
graphics.off()



## head(Pclimate)
## unique(Pclimate$'plot')
## tmp <- Pclimate[Pclimate$'plot'%in%'P42206', -5L]
## na.omit(tmp)
## spani <- 1971:2007
## p111 <- p42ch1[rownames(ip42ch1)%in%spani,]
## pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
## head(pttt)
## p16dcc <- dcc(p111, pttt, start = -6) ## error
## par(mfrow = c(2,1))
## dcplot(p16dcc)
## title('TRW (east Spain)')
## par(new = TRUE)

## unique(Pclimate$'plot')
## tmp <- Pclimate[Pclimate$'plot'%in%'P16008', -5L]
## na.omit(tmp)
## spani <- 1971:2007
## p111 <- p16ch1[rownames(p16ch1)%in%spani,]
## pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
## head(pttt)
## p16dcc <- dcc(p111, pttt, start = -6) ## error
## dcplot(p16dcc)
## par(mfg=c(1,1))
## title('TRW (north-central Spain)')

## tois <- '/home/wilar/Documents/isoPinaster/GrowthFunRes.pdf'
## dev.copy(pdf,tois)
## dev.off()
## graphics.off()

## unique(Pclimate$'plot')
## tmp <- Pclimate[Pclimate$'plot'%in%'P42206', -5L]
## na.omit(tmp)
## spani <- 1971:2007
## p111 <- ip42ch1[rownames(ip42ch1)%in%spani,]
## pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
## head(pttt)
## p16dcc <- dcc(p111, pttt, start = -6) ## error
## par(mfrow = c(2,1))
## dcplot(p16dcc)
## title('LWSC (east Spain)')
## par(new = TRUE)

## unique(Pclimate$'plot')
## tmp <- Pclimate[Pclimate$'plot'%in%'P16008', -5L]
## na.omit(tmp)
## spani <- 1971:2007
## p111 <- ip16ch1[rownames(p16ch1)%in%spani,]
## pttt <- tmp[as.numeric(tmp$'year')%in%spani,]
## head(pttt)
## p16dcc <- dcc(p111, pttt, start = -6) ## error
## dcplot(p16dcc)
## par(mfg=c(1,1))
## title('LWSC (north-central Spain)')

## tois <- '/home/wilar/Documents/isoPinaster/IsoFunRes.pdf'
## dev.copy(pdf,tois)
## dev.off()
## graphics.off()




## p16dcc <- dcc(p16ch1, ip16ch1, start = -10) ## error

## cp16 <- ts(p16ch1[4:nrow(p16ch1),1], start = -40)
## cp42 <- ts(p42ch1[3:nrow(p42ch1),1], start = -40)
## icp16 <- ts(ip16ch1[,1], start = -40)
## icp42 <- ts(ip42ch1[,1], start = -40)
## par(mfrow = c(2,1))
## ccip42 <- ccf(cp42, icp42, main = 'north', xlab = '', ylab = 'CCF', ylim = c(-0.6, 0.6))
## ccip16 <- ccf(cp16, icp16, main = 'south', xlab = 'Lag (years)',ylab = 'CCF', ylim = c(-0.6, 0.6))

## tois <- '/home/wilar/Desktop/IsoPaper/CCFFun.pdf'
## dev.copy(pdf,tois)
## dev.off()
## graphics.off()


## cp16 <- ts(p16ch[4:nrow(p16ch),1], start = -40)
## cp42 <- ts(p42ch[3:nrow(p42ch),1], start = -40)
## icp16 <- ts(ip16ch[,1], start = -40)
## icp42 <- ts(ip42ch[,1], start = -40)
## par(mfrow = c(2,1))
## ccip42 <- ccf(cp42, icp42, main = 'north', xlab = '', ylim = c(-0.6, 0.6))
## ccip16 <- ccf(cp16, icp16, main = 'south', xlab = 'Lag (years)',ylim = c(-0.6, 0.6))

## par(mfrow = c(2,1),
##     oma = c(2,2,0,0),
##     mar = c(1,1,0,0),
##     mgp = c(2,1,0))
## ccip42 <- ccf(cp42, icp42, main = 'north', xaxst = 'n', ylim = c(-0.6, 0.6))
## ccip16 <- ccf(cp16, icp16, main = 'east', ylim = c(-0.6, 0.6))

## significance <- level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(icp16)))

## tmp <- ccf(cp42, icp42, lag.max = 30)
## tmp <- ccf(cp16, icp16, lag.max = 30)

## cosineDist <- function(x){
##     as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
    
## }

## cosine <- function(x) {
##     y <- t(x) %*% x
##     res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
##     return(res)
    
## }

require('lsa')
pr16.rwi <- p16.rwi[4:nrow(p16.rwi),]
p16s <- cosine(t(as.matrix(pr16.rwi)))
pr42.rwi <- p42.rwi[3:nrow(p42.rwi),
                    names(p42.rwi)[!names(p42.rwi)%in%c('P42_06','P42_06a','P42_13','P42_16')]]
nrpr42 <- pr42.rwi[nrow(pr42.rwi),]
add42 <- rowMeans(nrpr42, na.rm = TRUE)
pr42.rwi[nrow(pr42.rwi),c('P42_03','P42_04')] <- add42
p42s <- cosine(t(as.matrix(pr42.rwi)))
ip16s <- cosine(t(as.matrix(ip16.rwi)))
ip42s <- cosine(t(as.matrix(ip42.rwi)))



## p16s[upper.tri(p16s, diag = TRUE)] <- NA  
## ip16s[upper.tri(ip16s, diag = TRUE)] <- NA  
## p42s[upper.tri(p42s, diag = TRUE)] <- NA

euP16s <- dist(as.matrix(pr16.rwi), diag = T, upper = T)
euP42s <- dist(as.matrix(pr42.rwi), diag = T, upper = T)
ieuP16s <- dist(as.matrix(ip16.rwi), diag = T, upper = T)
ieuP42s <- dist(as.matrix(ip42.rwi), diag = T, upper = T)
    
plot(mgram(euP16s,ieuP16s,nperm = 1E4))
plot(mgram(euP42s,ieuP42s,nperm = 1E4))

mantel.correlog(euP16s, ieuP16s, increment = 2)

diag(p16s)
corM16 <- mantel.correlog(p16s, ip16s, increment = 2)
corM42 <- mantel.correlog(p42s, ip42s, increment = 2)

plot(corM16)
plot(corM42)

corM16$correlation
corM16$p

corM42$correlation
corM42$p


pzcor16 <- cor(t(as.matrix(pr16.rwi)))
pzcor42 <- cor(t(as.matrix(pr42.rwi)))



require('ncf')



?mgram
mgram(p16s, p42s)


tail(p42s)

head(p16s)
head(p42s)

tail(p42.rwi)





head(tmp)
head(p16.rwi)



par(mfrow = c(1,2))
plot(p16ch, add.spline = TRUE)
par(new = TRUE)
plot(ts(rowMeans(ip16), start = -40), ylab = expression(delta))
abline(c(mean(ts(rowMeans(ip16))),0))

par(mfrow = c(1,2))
plot(p42ch, add.spline = TRUE, main = 'north', xlab = 'Time (from 2013)')
par(new = TRUE)
plot(p16ch, add.spline=TRUE, main = 'east', xlab = 'Time (from 2013)')

par(mfrow = c(1,2))
plot(ip42ch, add.spline = TRUE,
     xlab = 'Time (from 2013)',
     ylab = expression(paste(delta^{13}, "C index")),
     main = 'north')
par(new = TRUE)
plot(ip16ch, add.spline=TRUE,
     xlab = 'Time (from 2013)',
     ylab = expression(paste(delta^{13}, "C index")),
     main = 'east')

rwi.stats(mst)
rwi.stats(p16)
rwi.stats(p42)

interseries.cor(mst)
interseries.cor(p16)
interseries.cor(p42)

mstch <- chron(mst)
p16ch <- chron(p16)
p42ch <- chron(p42)

tofi <- '/home/wilar/Desktop/deltaI_north_center.pdf'
dev.copy(pdf,tofi)
dev.off()
graphics.off()

head(p16)

recode <- function(p16){
vn <- as.character(subset(ccodes, code%in%names(p16))[,'code'])
nvn <- as.character(subset(ccodes, code%in%names(p16))[,'icode'])
nvn. <- gsub('_','.', nvn)
names(p16) <- nvn.
return(p16)}

ccodes
p16cd <- recode(p16)
p42cd <- recode(p42)

p16b <- shiftFrame(recode(p16),
                   lev.nm = c('plot','sample'))
p42b <- shiftFrame(recode(p42),
                   lev.nm = c('plot','sample'))




    ## Modeling BIOdry fluctuations
p16f <- modelFrame(p16b,
                   form = 'tdForm',
                   on.time = TRUE,
                   log.t = TRUE,
                   MoreArgs = list(only.dup = TRUE,
                                   ## log.t = TRUE,
                                   mp = c(1,1),
                                   un = c('mm','cm')))

p4216b <- rbind(p16b,p42b)
p4216f <- modelFrame(p4216b,
                   form = 'tdForm',
                   on.time = TRUE,
                   log.t = TRUE,
                   MoreArgs = list(only.dup = TRUE,
                                   ## log.t = TRUE,
                                   mp = c(1,1),
                                   un = c('mm','cm')))
head(p4216f$resid)
    summary(p4216f$model)

p42f <- modelFrame(p42b,
                   form = 'tdForm',
                   on.time = TRUE,
                   ## log.t = TRUE,
                   MoreArgs = list(only.dup = TRUE,
                                   ## log.t = TRUE,
                                   mp = c(1,1),
                                   un = c('mm','cm')))
    head(p42f$resid)
    summary(p42f$model)



head(p42b)

head(subi)
pnas <- subset(subi, is.na(tree))
nnas <- paste('na',1:nrow(pnas),sep='_')

subi[with(subi,is.na(tree)), 'tree'] <- nnas

