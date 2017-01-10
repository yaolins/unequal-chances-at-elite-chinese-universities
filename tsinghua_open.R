setwd("~/Dropbox/data/tsinghua/tsinghua_open")

library(data.table)
library(ggplot2)
library(ggrepel)

#READ ANNONYMIZED TSINGHUA STUDENT ROSTER
master <- read.csv("master.csv", header = TRUE)

#CALCULATE THE NUMBER OF TSINGHUA STUDENTS BY YEAR AND PROVINCE
master = as.data.table(master)
summarized.prov = master[, list(n=.N), by=c("year", "province")]
summarized.prov$province <- as.character(summarized.prov$province)

#READ PROVINCIAL TOTALS
prov <- read.csv("province.csv", header = TRUE, check.names = FALSE)

#RESHAPE PROVINCIAL TOTAL DATA TO LONG FORMAT
melt.prov <- melt(prov, variable.name = "year", value.name = "tot")

#MERGE TSINGHUA DATA WITH PROVINCIAL TOTALS
merged.prov <- merge(melt.prov, summarized.prov, by = c("year", "province"))
merged.prov$pct <- merged.prov$n / merged.prov$tot #calculate acceptance rate
merged.prov[,1] <- as.character(merged.prov[,1])
merged.prov <- merged.prov[order(-merged.prov$pct),] #descending order

#OPTIONAL - EXPORT ACCEPTANCE RATE BY PROVINCE/YEAR IN WIDE FORMAT
wide.prov <- merged.prov
wide.prov$tot <- NULL
wide.prov$n <- NULL
wide.prov <- dcast(wide.prov, province ~ year)
wide.prov$median <- apply(wide.prov[,2:9],1, median, na.rm = TRUE)
wide.prov$pctchange <- (wide.prov[,9] - wide.prov[,2]) / wide.prov[,2] * 100

#CALCULATE THE ACCEPTANCE RATE OF "ALL OTHER PROVINCES"
everyoneelse <- subset(merged.prov, pct < 8)
everyoneelse = as.data.table(everyoneelse)
everyoneelse.new = everyoneelse[, lapply(.SD, sum, na.rm=TRUE), by=year, .SDcols=c("tot", "n")]
everyoneelse.new$pct <- everyoneelse.new$n / everyoneelse.new$tot
everyoneelse.new$province <- "其他"

#MERGE "OTHER" WITH "BEIJING", "SHANGHAI", AND "TIANJIN"
bjetc <- subset(merged.prov, pct > 8)
clean <- rbind(bjetc, everyoneelse.new)
clean$year <- as.numeric(clean$year)
clean$province <- factor(clean$province, levels = c("北京","上海","天津","其他"))


#TIME TREND CHART (CHINESE)

xlabs <- seq(2006, 2013, by=1)
caption <- "注：本统计只含港澳台外 31 个省和直辖市\n新生数据来自清华大学，高考数据来自新浪教育"
annot <- read.table(text=
	"year|pct|just|text
	2007.5|46|0|北京
	2008.2|20|0|上海
	2009.9|15.9|0|天津
	2010.5|5.9|0|其他省/直辖市",
	sep="|", header=TRUE, stringsAsFactors=FALSE)
	annot$text <- gsub("<br>", "\n", annot$text)
timetrend_cn <- ggplot() +
	theme_minimal(base_family="OpenSans-CondensedLight") +
	geom_line(data=clean, aes(x=year, y=pct, group=province, color = province)) +
	theme(text = element_text(family = 'STHeiti'),
		legend.position="none",
		panel.grid.major.x=element_blank(),
		panel.grid.minor.x=element_blank(),
		panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
		panel.grid.minor.y=element_blank(),
		axis.line.x=element_line(color="#2b2b2b", size=0.15),
		axis.ticks=element_line(),
		axis.ticks.x=element_line(color="#2b2b2b", size=0.15),
		axis.ticks.y=element_blank(),
		axis.ticks.length=unit(5, "pt"),
		axis.text.y=element_text(margin=margin(r=-2)),
		plot.margin= unit(rep(0.5,8), "cm"),
		plot.title=element_text(size=21, margin=margin(b=8)),
		plot.subtitle=element_text(size=12, color = "#707070", face = "italic"),
		plot.caption=element_text(size=7, hjust=1, margin=margin(t=15), color = "#707070")) +
	scale_x_continuous(expand=c(0,0),
                              breaks=seq(2006, 2013, by=1),
                              labels=xlabs, limits=c(2006,2013)) +
	labs(x = NULL,
		y = NULL,
		title="不同省份考清华的难度不同吗？",
		subtitle="清华入学人数，每万名高考考生",
		caption = caption) +
	geom_label(aes(x=2006, y=50, label="人"),
                      family="STHeiti",
                      size=3, hjust=0, label.size=0, color="#707070") +
	geom_label(data=annot, aes(x=year, y=pct, label=text, hjust=just),
                       family = 'STHeiti', lineheight=0.95,
                      size=5, label.size=0, color="#2b2b2b")
ggsave(filename="timetrend_cn.png", plot=timetrend_cn)


#TIME TREND CHART (ENGLISH)
caption_en <- "Note: Students are allowed to take the college entrance exam, the Gaokao, only in the province of their household residence (Hukou), and universities have different quotas for different provinces. Tsinghua is one of the two highest-ranked universities in China. Only Tsinghua was included in the analysis due to data availability, but the pattern is generalizable to all elite institutions. Sources: student data from Tsinghua, Gaokao data from Sina."
caption_en <- label_wrap_gen(155)(caption_en)

annot_en <- read.table(text=
"year|pct|just|text
2007.5|46|0|Beijing
2008|19|0|Shanghai
2010|16.6|0|Tianjin
2010.5|5.8|0|Other 28 Provinces",
sep="|", header=TRUE, stringsAsFactors=FALSE)
annot$text <- gsub("<br>", "\n", annot$text)

timetrend_en <- ggplot() +
	theme_minimal(base_family="Avenir") +
	geom_line(data=clean, aes(x=year, y=pct, group=province, colour = province)) +
	theme(text = element_text(family = 'Avenir'),
	legend.position="none",
	panel.grid.major.x=element_blank(),
	panel.grid.minor.x=element_blank(),
	panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
	panel.grid.minor.y=element_blank(),
	axis.line.x=element_line(color="#2b2b2b", size=0.15),
	axis.ticks=element_line(),
	axis.ticks.x=element_line(color="#2b2b2b", size=0.15),
	axis.ticks.y=element_blank(),
	axis.ticks.length=unit(5, "pt"),
	axis.text.y=element_text(margin=margin(r=-2)),
	plot.margin= unit(rep(0.5,10), "cm"),
	plot.title=element_text(size=13, margin=margin(b=10)),
	plot.subtitle=element_text(size=9, color = "#707070", face = "italic"),
	plot.caption=element_text(size=7, hjust=0, margin=margin(t=15), color = "#707070")) +
	scale_x_continuous(expand=c(0,0),
                              breaks=seq(2006, 2013, by=1),
                              labels=xlabs, limits=c(2006,2013)) +
	labs(x = NULL,
	y = NULL,
	title="Where Your Parents Are From Determines Your Chances at Elite Chinese Universities",
	subtitle="Number of students admitted to Tsinghua, per 10,000 college entrance exam takers",
	caption = caption_en) +
	geom_label(aes(x=2006, y=50, label="people"),
                      family="Avenir",
                      size=3, hjust=0, label.size=0, color="#707070") +
	geom_label(data=annot_en, aes(x=year, y=pct, label=text, hjust=just),
                       family = 'Avenir', lineheight=0.95,
                      size=4, label.size=0, color="#2b2b2b")
ggsave(filename="timetrend_en.png", plot=timetrend_en, width=7.5, height=4)

#SCATTERPLOT (X = TSINGHUA ACC, Y = 211 PER CAPITA)
univ211 <- read.csv("univ211.csv", header = TRUE, check.names = FALSE)
tsinghua211 <- merge(univ211, wide.prov, by = "province")

caption_sc <- "注：本统计只含港澳台外 31 个省和直辖市\n“清华入学人数”为 2006 至 2013 年新生数量中位数\n新生数据来自清华大学，高考数据来自新浪教育"

#NORMAL SCALE
scatter_normalscale <- ggplot(tsinghua211, aes(median*100, univ211percapita)) +
		geom_jitter() +
		theme_minimal(base_family="STHeiti") +
		theme(text = element_text(family = 'STHeiti'),
			panel.grid.major.x=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.x=element_blank(),
			panel.grid.major.y=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
			axis.title.x = element_text(size=13, margin=margin(20,0,0,0)),
			axis.title.y = element_text(size=13, margin=margin(0,20,0,0)),
			legend.position="top",
			legend.title=element_blank(),
			plot.caption=element_text(size=7, hjust=1, margin=margin(t=15), color = "#707070")) +
		guides(color = guide_legend(nrow = 1)) +
		labs(x = "清华入学人数，每百万名高考考生",
			y = "211 大学数量，每百万名高考考生",
			title= NULL,
			subtitle= NULL,
			caption = caption_sc) +
		geom_text_repel(aes(label=province), size=3, family = 'STHeiti', segment.size = 0.1)
ggsave(filename="scatter_normalscale.png", plot=scatter_normalscale)

#LOG SCALE
scatter <- ggplot(tsinghua211, aes(median*100, univ211percapita)) +
		geom_jitter() +
		theme_minimal(base_family="STHeiti") +
		theme(text = element_text(family = 'STHeiti'),
			panel.grid.major.x=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.x=element_blank(),
			panel.grid.major.y=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
			axis.title.x = element_text(size=13, margin=margin(20,0,0,0)),
			axis.title.y = element_text(size=13, margin=margin(0,20,0,0)),
			legend.position="top",
			legend.title=element_blank(),
			plot.caption=element_text(size=7, hjust=1, margin=margin(t=15), color = "#707070")) +
		guides(color = guide_legend(nrow = 1)) +
		scale_x_log10(breaks=c(0,200,500,1500,5000)) +
		scale_y_log10() +
		labs(x = "清华入学人数，每百万名高考考生",
			y = "211 大学数量，每百万名高考考生",
			title= NULL,
			subtitle= NULL,
			caption = caption_sc) +
		geom_text_repel(aes(label=province), size=3, family = 'STHeiti', segment.size = 0.1, force = 1)
ggsave(filename="scatter.png", plot=scatter)

#ENGLISH
caption_en_sc <- "Note: Students are allowed to take the college entrance exam, the Gaokao, only in the province of their household residence (Hukou), and universities have different quotas for different provinces. Tsinghua is one of the two highest-ranked universities in China. 'Number of student admitted to Tsinghua' is the median for each province in the years 2006 to 2013. 'Top universities' refer to Project 211 universities, i.e. those that receive significant funding from the national government. Sources: student data from Tsinghua, Gaokao data from Sina."
caption_en_sc <- label_wrap_gen(135)(caption_en_sc)

#NORMAL SCALE

scatter_en_normalscale <- ggplot(tsinghua211, aes(median*100, univ211percapita)) +
		geom_jitter() +
		theme_minimal(base_family="Avenir") +
		theme(text = element_text(family = 'Avenir'),
			panel.grid.major.x=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.x=element_blank(),
			panel.grid.major.y=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
			axis.title.x = element_text(size=13, margin=margin(20,0,0,0)),
			axis.title.y = element_text(size=13, margin=margin(0,20,0,0)),
			legend.position="top",
			legend.title=element_blank(),
			plot.caption=element_text(size=8.5, hjust=0, margin=margin(t=15), color = "#707070"),
			plot.margin= unit(rep(0.5,8), "cm")) +
		guides(color = guide_legend(nrow = 1)) +
		labs(x = "Number of students admitted to Tsinghua, per million college entrance exam takers",
			y = "Number of top universities* per million college entrance exam takers",
			title= NULL,
			subtitle= NULL,
			caption = caption_en_sc) +
		geom_text_repel(aes(label=province_en), size=3, family = 'Avenir', segment.size = 0.1, force = 1)
ggsave(filename="scatter_en_normalscale.png", plot=scatter_en_normalscale,width=8.5,height=8)


scatter_en <- ggplot(tsinghua211, aes(median*100, univ211percapita)) +
		geom_jitter() +
		theme_minimal(base_family="Avenir") +
		theme(text = element_text(family = 'Avenir'),
			panel.grid.major.x=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.x=element_blank(),
			panel.grid.major.y=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
			axis.title.x = element_text(size=13, margin=margin(20,0,0,0)),
			axis.title.y = element_text(size=13, margin=margin(0,20,0,0)),
			legend.position="top",
			legend.title=element_blank(),
			plot.caption=element_text(size=8, hjust=0, margin=margin(t=15), color = "#707070"),
			plot.margin= unit(rep(0.5,8), "cm")) +
		guides(color = guide_legend(nrow = 1)) +
		scale_x_log10(breaks=c(0,200,500,1500,5000)) +
		scale_y_log10() +
		labs(x = "Number of students admitted to Tsinghua, per million college entrance exam takers",
			y = "Number of top universities* per million college entrance exam takers",
			title= NULL,
			subtitle= NULL,
			caption = caption_en_sc) +
		geom_text_repel(aes(label=province_en), size=3, family = 'Avenir', segment.size = 0.1, force = 1)
ggsave(filename="scatter_en.png", plot=scatter_en,width=8, height=7.9)

#BAR CHART - CHANGE

wide.prov[["sign"]] = ifelse(wide.prov[["pctchange"]] >= 0, "positive", "negative")

bar <- ggplot(wide.prov, aes(x=reorder(province, -pctchange), y=pctchange, fill=sign)) +
	  geom_bar(stat="identity") +
	  theme_minimal(base_family="STHeiti") +
	  theme(text = element_text(family = 'STHeiti'),
			panel.grid.major.x=element_blank(),
			panel.grid.minor.x=element_blank(),
			panel.grid.major.y=element_line(color="#0d0d0d", linetype="dotted", size=0.1),
			panel.grid.minor.y=element_line(color="#2b2b2b", linetype="dotted", size=0.1),
			axis.text.x=element_text(size=10, angle=310,hjust=0.5,vjust=1),
			axis.title.y = element_text(angle=0, size=11, margin=margin(0,10,0,0), vjust=0.5),
			legend.position="top",
			legend.title=element_blank(),
			plot.caption=element_text(size=8, hjust=1, margin=margin(t=15), color = "#707070"),
			plot.margin= unit(rep(0.5,8), "cm")) +
	  scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"))+
	  scale_y_continuous(breaks=seq(-25, 150, by=25), limits=c(-25, 150)) +
	  guides(fill=FALSE) +
	  labs(x = NULL,
		  y = "清\n华\n入\n学\n人\n数\n占\n当\n年\n高\n考\n人\n数\n比\n例\n\n'13\n年\n相\n对\n于\n'06\n年\n变\n化",
		  title= NULL,
		  subtitle= NULL,
		  caption = caption)
ggsave(filename="bar.png", plot=bar,width=9)