library(arules)
library(arulesViz)

#data <- read.transactions("/home/johannes/Documents/Ubiqum/Course2/task4/data/ElectronidexTransactions2017.csv", 
                               #format = c("basket"), cols = NULL, sep=",", rm.duplicates = TRUE)

data_df <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task4/data/ElectronidexTransactions2017.csv", header = FALSE, sep = ",")
head(data_df)

data_df[data_df == "iMac"] <- ""
data_df[data_df == "iMac "]<- ""
data_df[data_df == "HP Laptop"] <- ""

write.table(data_df, file = "noiMac.csv", col.names = FALSE, row.names = FALSE, sep = ",")
data <- read.transactions("noiMac.csv", sep =",", format("basket"),  rm.duplicates = TRUE)

transdata <- data[which(size(data) != 0)]

#### Transactions with more than x items ####
transdata_big <- data[which(size(data)>12)]

# Summary transdata_big
summary(transdata_big)
head(size(transdata_big))
hist(size(transdata_big))
# FIRST INSPECTION
inspect (transdata_big[1:3]) # You can view the transactions. Is there a way to see a certain # of transactions?
length (transdata_big) # Number of transactions.
size (transdata_big) # Number of items per transaction
LIST(transdata_big) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transdata_big)# To see the item labels

# insepct by rows
inspect(transdata_big[1:6])

# counts frquency of items
itemFrequency(transdata_big[1:125])

plot(itemFrequency(transdata_big))

plot(size(transdata_big))

itemFrequencyPlot(transdata_big, support=0.02,cex.names =0.6)

# TOP 10 frequency
itemFrequencyPlot(transdata_big,topN=10)


image(transdata_big)

image(sample(transdata_big, 20))


# APRIORI ALGORITHM
rules_big <- apriori(transdata_big, parameter = list(support=0.03, confidence=0.7, minlen=2))
rules_big
inspect(head(rules_big))
plot(rules_big)

# redundancy check
redundant_rules_big <- is.redundant(rules_big)
redundant_rules_big
summary(redundant_rules_big)

#remove redundant rules
rules_big <- rules_big[!redundant_rules_big]
rules_big

summary(rules_big)

plot(rules_big, measure=c("support","confidence"),shading="lift")

inspect(rules_big[1:15])


# sort rules by lift
inspect(head(sort(rules_big,by="lift"),20))

####Transdata####
# Summary transdata
summary(transdata)
head(size(transdata))
hist(size(transdata))
# FIRST INSPECTION
inspect (transdata[1:3]) # You can view the transactions. Is there a way to see a certain # of transactions?
length (transdata) # Number of transactions.
size (transdata) # Number of items per transaction
LIST(transdata) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(transdata)# To see the item labels

# insepct by rows
inspect(transdata[1:6])

# counts frquency of items
itemFrequency(transdata[1:125])

plot(itemFrequency(transdata))

plot(size(transdata))

itemFrequencyPlot(transdata, support=0.02,cex.names =0.6)

# TOP 10 frequency
itemFrequencyPlot(transdata,topN=10)


image(transdata)

image(sample(transdata, 20))

# APRIORI ALGORITHM
rules <- apriori(transdata, parameter = list(support=0.003, confidence=0.3, minlen=2))
rules
inspect(head(rules))

# redundancy check
redundant_rules <- is.redundant(rules)
redundant_rules
summary(redundant_rules)

#remove redundant rules
rules <- rules[!redundant_rules]
rules

summary(rules)

plot(rules, method = "graph")

inspect(rules[1:15])

# sort rules
inspect(sort(rules,by="lift")[1:20])

plot(rules)
head(quality(rules))

plot(rules, measure=c("support","confidence"),shading="lift")
plot(rules, shading="order", cotrol=list(main="Two-key plot"))

####SUBSETS####
####create subset####
rules_sub1 <- subset(rules, items %in% "Apple Earpods")
rules_sub2 <- subset(rules, items %in% "Apple Magic Keyboard")
rules_sub3 <- subset(rules, items %in% "Computer Game")
rules_sub4 <- subset(rules, items %in% "ASUS 2 Monitor")
rules_sub5 <- subset(rules, items %in% "Epson Printer")
rules_sub6 <- subset(rules, items %in% "LG Monitor")
rules_sub7 <- subset(rules, items %in% "AOC Monitor")
rules_sub8 <- subset(rules, items %in% "HP Black & Tri-color Ink")
rules_sub9 <- subset(rules, items %in% "Rii LED Gaming Keyboard & Mouse Combo")
rules_sub10 <- subset(rules, items %in% "iPad Pro")
####Visualize subset####
# sub1
rules_sub1
inspect(rules_sub1)
inspect(sort(rules_sub1,by="lift")[1:12])
inspect(subset(rules_sub1,by="lift" (rhs %in% c("Apple Earpods")[1:12])))
#inspect(subset(rules_sub1, (lhs %in% c("ViewSonic Monitor"))))
plot(rules_sub1, method = "graph", interactive = T)
plot(rules_sub1[1:10], method="graph", control=list(type="items"), interactive = T)

# sub2
rules_sub2
inspect(rules_sub2)
inspect(sort(rules_sub2,by="lift"))
inspect(subset(rules_sub2, (rhs %in% c("Apple Magic Keyboard"))))
#inspect(subset(rules_sub2, (lhs %in% c("Apple Magic Keyboard"))))
plot(rules_sub2, method = "graph", interactive = T)
plot(rules_sub2[1:10], method="graph", control=list(type="items"), interactive = T)

# sub3
rules_sub3
inspect(rules_sub3)
inspect(sort(rules_sub3,by="lift"))
inspect(subset(rules_sub3,(rhs %in% c("Computer Game"))))
#inspect(subset(rules_sub2, (lhs %in% c("HP Laptop"))))
plot(rules_sub3, method = "graph", interactive = T)
plot(rules_sub3[1:10], method="graph", control=list(type="items"), interactive = T)

# sub4
rules_sub4
inspect(rules_sub4)
inspect(sort(rules_sub4,by="lift"))
inspect(subset(rules_sub4, (rhs %in% c("ASUS 2 Monitor"))))
#inspect(subset(rules_sub2, (lhs %in% c("HP Laptop"))))
plot(rules_sub4, method = "graph", interactive = T)
plot(rules_sub4[1:10], method="graph", control=list(type="items"), interactive = T)

# sub5
rules_sub5
inspect(rules_sub5)
inspect(sort(rules_sub5,by="lift"))
inspect(subset(rules_sub5, (rhs %in% c("Epson Printer"))))
#inspect(subset(rules_sub2, (lhs %in% c("HP Laptop"))))
plot(rules_sub5, method = "graph", interactive = T)
plot(rules_sub5[1:10], method="graph", control=list(type="items"), interactive = T)

# sub6
rules_sub6
inspect(rules_sub6)
inspect(sort(rules_sub6,by="lift"))
inspect(subset(rules_sub6, (rhs %in% c("LG Monitor"))))
#inspect(subset(rules_sub2, (lhs %in% c("HP Laptop"))))
plot(rules_sub6, method = "graph", interactive = T)
plot(rules_sub6[1:10], method="graph", control=list(type="items"), interactive = T)


# sub7
rules_sub7
inspect(rules_sub7)
inspect(sort(rules_sub7,by="lift"))
inspect(subset(rules_sub7, (rhs %in% c("AOC Monitor"))))
#inspect(subset(rules_sub7, (lhs %in% c("HP Laptop"))))
plot(rules_sub7, method = "graph", interactive = T)
plot(rules_sub7[1:10], method="graph", control=list(type="items"), interactive = T)

# sub8
rules_sub8
inspect(rules_sub8)
inspect(sort(rules_sub8,by="lift"))
inspect(subset(rules_sub8, (rhs %in% c("HP Black & Tri-color Ink"))))
#inspect(subset(rules_sub8, (lhs %in% c("HP Laptop"))))
plot(rules_sub8, method = "graph", interactive = T)
plot(rules_sub8[1:10], method="graph", control=list(type="items"), interactive = T)

# sub9
rules_sub9
inspect(rules_sub9)
inspect(sort(rules_sub9,by="lift"))
inspect(subset(rules_sub9, (rhs %in% c("Rii LED Gaming Keyboard & Mouse Combo"))))
#inspect(subset(rules_sub9, (lhs %in% c("HP Laptop"))))
plot(rules_sub9, method = "graph", interactive = T)
plot(rules_sub9[1:10], method="graph", control=list(type="items"), interactive = T)

# sub10
rules_sub10
inspect(rules_sub10)
inspect(sort(rules_sub10,by="lift"))
inspect(subset(rules_sub10, (rhs %in% c("iPad Pro"))))
#inspect(subset(rules_sub9, (lhs %in% c("HP Laptop"))))
plot(rules_sub10, method = "graph", interactive = T)
plot(rules_sub10[1:10], method="graph", control=list(type="items"), interactive = T)

