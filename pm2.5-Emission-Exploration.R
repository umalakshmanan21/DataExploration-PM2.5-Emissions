###Emission in each year###
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
head(SCC)
Emissions <- aggregate(NEI[, 'Emissions'], by=list(NEI$year), FUN=sum)
head(Emissions)
Emissions$PM <- round(Emissions[,2]/1000,2)
barplot(Emissions$PM, names.arg=Emissions$Group.1, main=expression('Total Emission of PM'[2.5]),xlab='Year', ylab=expression(paste('PM', ''[2.5], ' in Kilotons')))

### emission in baltimore area ####
baltEmissions <-  subset(NEI,NEI$fips == '24510')
Emissions <- aggregate(baltEmissions[, 'Emissions'], by=list(baltEmissions$year), FUN=sum)
head(Emissions)
Emissions$PM <- round(Emissions[,2]/1000,2)
barplot(Emissions$PM, names.arg=Emissions$Group.1, main=expression('Total Emission of PM'[2.5]),xlab='Year', ylab=expression(paste('PM', ''[2.5], ' in Kilotons')))

#####baltimore emission by type#############
library(ggplot2)
baltEmissions <-  subset(NEI,NEI$fips == '24510')
head(baltEmissions)
baltEmissions$year <-  factor(baltEmissions$year,levels = c('1999','2002','2005','2008'))
ggplot(data=baltEmissions, aes(x=year, y=log(Emissions))) + facet_grid(. ~ type) + guides(fill=F) +geom_boxplot(aes(fill=type)) + stat_boxplot(geom ='errorbar') +ylab(expression(paste('Log', ' of PM'[2.5], ' Emissions'))) + xlab('Year') + ggtitle('Emissions per Type in Baltimore City, Maryland')
#########################################

#### PLOT 4 - emission due to coal ###########################
Coal_SCC = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]
head(Coal_SCC)
summary(Coal_SCC)
dim(Coal_SCC)
merge <- merge(NEI,Coal_SCC,by = 'SCC')
merge_emit <- aggregate(merge[, 'Emissions'], by=list(merge$year), FUN=sum)
merge_emit$x <- round(merge_emit[,2]/1000,2)
plot(merge_emit$Group.1,merge_emit$x,type='o',xlab = 'Year' , ylab = (expression(paste('PM', ''[2.5], ' in kilotons'))),col = "blue",lwd = 2)

#### PLOT 5 - emmision by motor vehicles in baltimore area ###########################
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
head(SCC)
SCC_MV = SCC[grepl("veh", SCC$Short.Name, ignore.case=TRUE),]
head(SCC_MV)
summary(SCC_MV)
dim(SCC_MV)
baltEmissions <-  subset(NEI,NEI$fips == '24510')
merge <- merge(baltEmissions,SCC_MV,by = 'SCC')
merge_emit <- aggregate(merge[, 'Emissions'], by=list(merge$year), FUN=sum)
merge_emit$x <- round(merge_emit[,2]/1000,2)
plot(merge_emit$Group.1,merge_emit$x,type='o',xlab = 'Year' , ylab = (expression(paste('PM', ''[2.5], ' in kilotons'))),col = "blue",lwd = 2)


#### PLOT 6- comparison of emissions in baltimore and Los Angeles due to motor vehicles  ###########################
library(ggplot2)
SCC_MV = SCC[grepl("veh", SCC$Short.Name, ignore.case=TRUE),]
baltLAEmissions <-subset(NEI,NEI$fips == '24510' | NEI$fips == '06037' )
merge1 <- merge(baltLAEmissions,SCC_MV,by = 'SCC')
merge1_emit <- aggregate(merge1[, 'Emissions'], by=list(merge1$year), FUN=sum)
merge1_emit$x <- round(merge1_emit[,2]/1000,2)
baltLAEmissions$year <- factor(baltLAEmissions$year,levels = c('1999','2002','2005','2008'))
ggplot(data=baltLAEmissions, aes(x=year, y=log(Emissions))) + facet_grid(. ~ fips) + guides(fill=F) +geom_boxplot(aes(fill=fips)) + stat_boxplot(geom ='errorbar') +ylab(expression(paste('Log', ' of PM'[2.5], ' Emissions'))) + xlab('Year') + ggtitle('Emissions per Type in Los Angeles County, CA (06037) and Baltimore City, Maryland (24510)')








