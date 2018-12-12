library(dplyr)
library(ggplot2)
setwd("C:/Users/root/Documents/SJC_Library/")
data_dir <- '.'

records <- data.frame(read.csv("MCE_Historical_Transactions.csv"), stringsAsFactors=F)
records$CHARGE_LOCATION <- NULL
names(records)[3] <- "TYPE"
names(records)[5] <- "CHECK_OUT"
names(records)[6] <- "RETURN"

records <- transform(records,CHECK_OUT= as.POSIXct(CHECK_OUT, format="%m/%d/%y %I:%M %p"))
records <- transform(records,RETURN= as.POSIXct(RETURN, format="%m/%d/%y %I:%M %p"))

records <- mutate(records, CO_DATE = strftime(CHECK_OUT, format = "%F"),
                     RET_DATE = strftime(CHECK_OUT, format = "%F"),
                     CO_TIME = as.numeric(as.POSIXct(CHECK_OUT, origin = "1970-01-01")),
                     RET_TIME = as.numeric(as.POSIXct(RETURN, origin = "1970-01-01")),
                     DURATION_OUT = RET_TIME - CO_TIME
)

records <- records[as.character(records$CO_DATE) > "2006-01-01" & 
                     as.character(records$CO_DATE) < "2015-01-01" ,]

all_titles <- unique(records[,"TITLE"], drop = F )

types <- unique(records$TYPE)
levels(records$TYPE) <- c("Video", "Reserve 2HR", "Audio Cassette", "Book", "CD", "Laptop", 
                          "Laptop", "Periodical", "Reference Book", "Special Collection", "DVD")

records$TYPE[records$TITLE == "[Headphone]"] <- "Reserve 2HR"
records$TYPE[records$TITLE == "Clickers Set"] <- "Reserve 2HR"

#if TITLE , CHECKIN, and CHECKOUT are duplicates remove that line. 
#This will get rid of duplicates under different TYPES.
#duplicated returns the copies and leaves the originals in the df
#!duplicated gives me back the originals.

records <- records[ !duplicated(records[c("TITLE", "CALL_NO","CHECK_OUT", "RETURN")]), ]


rownames(records) <- NULL
nrow(records) #39213 before subsetting from 2006/1/1 - 2015/1/1

book <- filter(records, records$TYPE == "Book"); nrow(book); #29274
book_titles <- unique(book$TITLE); NROW(book_titles); #10335

laptop <- filter(records, records$TYPE == "Laptop"); nrow(laptop); #4660
laptop_titles <- unique(laptop$TITLE); NROW(laptop_titles); #4

special <- filter(records, records$TYPE == "Special Collection");nrow(special); #65
special_titles <- unique(special$TITLE); NROW(special_titles); #20

dvd <- filter(records, records$TYPE == "DVD"); nrow(dvd); #843
dvd_titles <- unique(dvd$TITLE); NROW(dvd_titles); #302

cd <- filter(records, records$TYPE == "CD"); nrow(cd); #666
cd_titles <- unique(cd$TITLE); NROW(cd_titles); #54

video <- filter(records, records$TYPE == "Video"); nrow(video); #178
video_titles <- unique(video$TITLE); NROW(video_titles); #71

reference <- filter(records, records$TYPE == "Reference Book"); nrow(reference); #350
reference_titles <- unique(reference$TITLE); NROW(reference_titles); #54

reserve <- filter(records, records$TYPE == "Reserve 2HR"); nrow(reserve); #3157
reserve_titles <- unique(reserve$TITLE); NROW(reserve_titles); #19

cassette <- filter(records, records$TYPE == "Audio Cassette"); nrow(cassette); #13
cassette_titles <- unique(cassette$TITLE); NROW(cassette_titles); #6 

periodical <- filter(records, records$TYPE == "Periodical"); nrow(periodical); #7
periodical_titles <- unique(periodical$TITLE); NROW(periodical_titles); #4

#10869 total titles compared to 10844. 25 Titles are in mixed categories.

bc <-intersect( book_titles, cd_titles) #7
dv <-intersect( dvd_titles, video_titles) #11
bs <-intersect( book_titles, special_titles) #2
br <-intersect( book_titles, reference_titles) #5

duplicates <- Reduce(union, list(bc,dv, bs, br))

##For every duplicate item, filter it out of records and count how many times each is checked out.
##Allow easy identification of books that come with CDs or DVDs, and books that changed TYPE when
##a never version was released. 
double_category <- lapply(duplicates, function(x) {
  filter(records, TITLE == x) %>% group_by(TITLE, CALL_NO, TYPE) %>% summarize(NUM = n())
})

double_category <-as.data.frame(do.call(rbind, double_category))

# Now let's look at the top titles in each category
special_top <- arrange(special %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
reference_top <- arrange(reference %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
reserve_top <- arrange(reserve %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
book_top <- arrange(book %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
cassette_top <- arrange(cassette %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
cd_top <- arrange(cd %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
dvd_top <- arrange(dvd %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
video_top <- arrange(video %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
periodical_top <- arrange(periodical %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))
laptop_top <- arrange(laptop %>% group_by(TITLE) %>% summarize(NUM_TRANS = n()), desc(NUM_TRANS))

# #anything with books
# View(book_top) #top books are textbooks as expected
# View(special_top) #books have <20 transactions.
# View(reference_top) #books
# 
# #no books here
# View(reserve_top) #no books only scissors, study rooms, markers, etc.
# View(cassette_top) #only 6 diff tapes
# View(cd_top) #Top cds are for small children. most likely used as teaching aids. 
# View(dvd_top) #Over 302 dvds but only 50 have been checked out more than 5 times. top 3 dvds: 26,23,19
# View(periodical_top) #only 4 items. 7 checkouts total
# View(laptop_top)
# View(video_top) #only 71 vids. Largest checkout rate is 15. 

top_type <- records %>% group_by(TYPE) %>% summarize(NUM = n())
View(arrange(top_type, desc(NUM)))

all_books <- rbind(special, reference, book)
View(all_books)

####################################################
# Questions to work on
####################################################
# Find the day with the most transactions
books_daily <- all_books %>% group_by(DAY = CO_DATE) %>% summarize(TOTAL = n())
View(books_daily)
View(arrange(books_daily, desc(TOTAL)))

mean(books_daily$TOTAL)
median(books_daily$TOTAL)

ggplot(books_daily, aes(x = DAY, y = TOTAL) ) + geom_point()

# Find the month with the most transactions on average
books_monthly <- all_books %>% group_by(MONTH = substring(CO_DATE, 1,7)) %>% summarize(TOTAL = n())
books_monthly <- transform(books_monthly, 
                           SEMESTER = ifelse(
                             as.numeric(substring(MONTH, 6,7)) <=11 & as.numeric(substring(MONTH,6,7)) >= 9, "FALL",
                              ifelse(as.numeric(substring(MONTH, 6,7))==12 | as.numeric(substring(MONTH,6,7)) == 1, "WINTER_BREAK",
                                     ifelse(as.numeric(substring(MONTH, 6,7)) <=8 & as.numeric(substring(MONTH,6,7)) >= 5, "SUMMER","SPRING")))
                           )

View(books_monthly)

p <- ggplot(books_monthly, aes(x = MONTH, y = TOTAL, group = 1) ) + geom_line(aes(color=books_monthly$SEMESTER))
p <- p + geom_point(aes(color=books_monthly$SEMESTER))
p <- p + theme(legend.position = c(.1,.9), 
                 legend.title=element_blank(),
                 axis.text.x=element_text(angle=65, hjust=1), 
                 legend.background = element_rect(fill = "transparent"))
p

# plot the number of book checkouts by day chronologically

# plot the number of book checkouts by day on average across all years
books_daily_average <- all_books %>% 
                        group_by(DAY = strftime(all_books$CHECK_OUT, format = "%m-%d")) %>%
                        summarize(TOTAL = n())
View(books_daily_average)

p <- ggplot(books_daily_average, aes(x = DAY, y = TOTAL, group = 1) ) + geom_line()
p <- p + geom_point()
p <- p + theme(legend.position = c(.1,.9), 
               legend.title=element_blank(),
               axis.text.x=element_text(angle=65, hjust=1), 
               legend.background = element_rect(fill = "transparent"))
p


# Classify the books by ID number. Try to find a library.
# What's the average number of days that students borrow books before returning them?
# How many books are there total in the library? 


nrow(all_books) #29662
NROW(unique(all_books$CO_DATE)) #2396


x <- all_books %>% 
  group_by(DAY = strftime(all_books$CHECK_OUT, format = "%m-%d")) %>%
  summarize(MEAN = n()/9)

a <- ggplot(all_books, aes(x = DAY, y = MEAN, group = 1) ) + geom_line()

View(books_daily_average)
View(x)

sum(x$MEAN)

mean(all_books$DURATION_OUT) / (60*60*24)
median(all_books$DURATION_OUT) / (60*60*24)

locc <- data.frame(read.csv("locc.csv"), stringsAsFactors=F)
names(locc)[1] <- "PREFIX"
names(locc)[2] <- "Subject Area"


dewey <- data.frame(read.csv("dewey.csv", header= F), stringsAsFactors=F)
names(dewey)[1] <- "PREFIX"
names(dewey)[2] <- "Subject Area"



#match books suffix
#View(locc)
all_books <- transform(all_books, PREFIX = substring(CALL_NO, 1,1))
all_books <- arrange(all_books, desc(CO_DATE))


#foo <- all_books %>% group_by(PREFIX) %>% summarize(NUM = n())

dewey_books <- filter(all_books, !is.na(as.numeric(as.character(all_books$PREFIX))))
locc_books <- filter(all_books, is.na(as.numeric(as.character(all_books$PREFIX))))

locc_books <- merge(locc_books, locc, by="PREFIX")
dewey_books <- merge(dewey_books, dewey, by="PREFIX")



#export these
dist_locc <- locc_books %>% group_by("Subject Area") %>% summarize(NUM = n())
dist_dewey <- dewey_books %>% group_by("Subject Area") %>% summarize(NUM = n())

write.csv(dist_locc, file="dist_locc.csv", row.names = FALSE)
write.csv(dist_dewey, file="dist_dewey.csv", row.names = FALSE)

dist_dewey_year <- dewey_books %>% group_by("Subject Area", CO_DATE) %>% summarize(NUM = n())
View(dist_dewey_year)


write.csv(head(book_top,500), file="top_books.csv", row.names = FALSE)
