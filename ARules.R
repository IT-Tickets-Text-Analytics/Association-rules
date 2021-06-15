# Import relevant packages

library(tidyverse) #Data Manipulation and Plotting
library(lubridate) #Date Manipulation
library(arulesSequences) #;Running the Sequence mining algorithm
library(ggtext) #Making adding some flair to plots
library(tidygraph)  ## Cre,ating a Graph Structure
library(ggraph) ## Plotting the Network Graph Structure
library(arulesViz)

setwd("C:\\Users\\IT Tickets")
#Import standard transaction data
transactions = read.csv("Ticket-Task.csv")
# Start time of data to be considered
head(transactions)
View(transactions)

transactions<-transactions[,c(1,2,3)]

trans_sequence <- transactions %>%

  group_by(ï..Ticket_ID, Ticket_Type) %>%

  summarize(
    SIZE = n(),
    Subtype = paste(as.character(Subtype), collapse = ';')
  )
View(trans_sequence)

trans_sequence = trans_sequence[,c(2,1,4)]

trans_sequence
names(trans_sequence) = c("sequenceID","eventID", "items")
trans_sequence <- data.frame(lapply(trans_sequence, as.factor))
trans_sequence <- trans_sequence[order(trans_sequence$sequenceID, trans_sequence$eventID),]
View(trans_sequence)
# Convert to transaction matrix data type
write.table(trans_sequence, "mytxtout.txt", sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
trans_matrix <- read_baskets("mytxtout.txt", sep = ";", info = c("sequenceID","eventID", "items"))
str(trans_matrix)


#_____________________________________________________________________________________________
#_____________________________________Apriori algorithm_______________________________________
trans_sequence1<-trans_sequence[,c(3)]
View(trans_sequence1)
write.csv(trans_sequence1, "mytxtout.csv",row.names = FALSE, col.names = FALSE, quote = FALSE)
tr <- read.transactions('mytxtout.csv', format = 'basket', sep=';')
tr
summary(tr)
#_______________
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
#____________________

rules <- apriori(tr, parameter=list(support=0.001, confidence=0.8,minlen=3, maxlen=10))
rules
inspect(rules)
write(rules, file = "data_DML_PAE.csv", sep = ",")
summary(rules)

plot(rules)
# Filter rules with confidence greater than 0.4 or 40%
subRules<-rules[quality(rules)$confidence>0.6]
#Plot SubRules
plot(subRules)
top10subRules <- head(subRules, n = 30, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
plot(top10subRules, method="graph", control=list(type="items"))
plot(top10subRules, method = "grouped")
plot(top10subRules, method = "grouped", control = list(k = 50))
plot(top10subRules, method = "paracoord")

itemsets <- eclat(tr, parameter = list(support = 0.001, minlen=2))
plot(itemsets, method="graph")

oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = tr)

subrules <- subset(rules, lift>1)
subrules
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

plot(subRules2, method="graph", control=list(type="items"))


Update_rules_rhs <- apriori(tr,
                            parameter = list(supp=0.001, conf=0.8,
                                             minlen=2),
                            appearance = list(default="lhs", rhs="Ticket: Update"))

inspect(Update_rules_rhs)
write(Update_rules_rhs, file = "Update_RTSS.csv", sep = ",")
# Filter rules with confidence greater than 0.4 or 40%
subRules<-rules[quality(Update_rules_rhs)$confidence>0.32]
inspect(subRules)
#Plot SubRules
plot(subRules)
top10subRules <- head(Update_rules_rhs, n = 30, by = "confidence")
inspect(top10subRules)
plot(top10subRules, method = "graph",  engine = "htmlwidget")

Delivery_rules_rhs <- apriori(tr,
                              parameter = list(supp=0.001, conf=0.8,

                                               minlen=2),
                              appearance = list(default="lhs", rhs="Ticket: Delivery"))

inspect(Delivery_rules_rhs)
write(Delivery_rules_rhs, file = "Delivery_RTSS.csv", sep = ",")

Decommission_rules_rhs <- apriori(tr,
                                  parameter = list(supp=0.001, conf=0.8,

                                                   minlen=2),
                                  appearance = list(default="lhs", rhs="Ticket: Decommission"))

inspect(Decommission_rules_rhs)
write(Decommission_rules_rhs, file = "Decommission_RTSS.csv", sep = ",")

Installation_rules_rhs <- apriori(tr,
                                  parameter = list(supp=0.001, conf=0.8,
                                                   maxlen=10,
                                                   minlen=2),
                                  appearance = list(default="lhs", rhs="Ticket: Installation"))

inspect(Installation_rules_rhs)
write(Installation_rules_rhs, file = "Installation_RTSS.csv", sep = ",")

Incident_rules_rhs <- apriori(tr,
                              parameter = list(supp=0.001, conf=0.8,
                                               maxlen=10,
                                               minlen=2),
                              appearance = list(default="lhs", rhs="Ticket: Incident"))

inspect(Incident_rules_rhs)
write(Incident_rules_rhs, file = "Incident_RTSS.csv", sep = ",")

Incident_rules_rhs <- apriori(tr,
                              parameter = list(supp=0.001, conf=0.8,
                                               maxlen=10,
                                               minlen=2),
                              appearance = list(default="lhs", rhs="Ticket: Incident"))

inspect(Incident_rules_rhs)
write(Incident_rules_rhs, file = "Incident_RTSS.csv", sep = ",")

Capacity_rules_rhs <- apriori(tr,
                              parameter = list(supp=0.001, conf=0.8,
                                               maxlen=10,
                                               minlen=2),
                              appearance = list(default="lhs", rhs="Ticket: Capacity"))

inspect(Capacity_rules_rhs)
write(Capacity_rules_rhs, file = "Capacity_RTSS.csv", sep = ",")

Migration_rules_rhs <- apriori(tr,
                               parameter = list(supp=0.001, conf=0.8,
                                                maxlen=10,
                                                minlen=2),
                               appearance = list(default="lhs", rhs="Ticket: Migration"))

inspect(Migration_rules_rhs)
write(Migration_rules_rhs, file = "Migration_RTSS.csv", sep = ",")

Support_rules_rhs <- apriori(tr,
                             parameter = list(supp=0.001, conf=0.8,
                                              maxlen=10,
                                              minlen=2),
                             appearance = list(default="lhs", rhs="Ticket: Support"))

inspect(Support_rules_rhs)
write(Support_rules_rhs, file = "Support_RTSS.csv", sep = ",")

Integration_rules_rhs <- apriori(tr,
                                 parameter = list(supp=0.001, conf=0.8,
                                                  maxlen=10,
                                                  minlen=2),
                                 appearance = list(default="lhs", rhs="Ticket: Integration"))

inspect(Integration_rules_rhs)
write(Integration_rules_rhs, file = "Integration_RTSS.csv", sep = ",")

Release_rules_rhs <- apriori(tr,
                             parameter = list(supp=0.001, conf=0.8,
                                              maxlen=10,
                                              minlen=2),
                             appearance = list(default="lhs", rhs="Ticket: Release"))

inspect(Release_rules_rhs)
write(Release_rules_rhs, file = "Release_RTSS.csv", sep = ",")


Configuration_rules_rhs <- apriori(tr,
                                   parameter = list(supp=0.001, conf=0.8,
                                                    maxlen=10,
                                                    minlen=2),
                                   appearance = list(default="lhs", rhs="Ticket: Configuration"))

inspect(Configuration_rules_rhs)
write(Configuration_rules_rhs, file = "Configuration_RTSS.csv", sep = ",")

Problem_rules_rhs <- apriori(tr,
                             parameter = list(supp=0.001, conf=0.8,
                                              maxlen=10,
                                              minlen=2),
                             appearance = list(default="lhs", rhs="Ticket: Problem"))

inspect(Problem_rules_rhs)
write(Problem_rules_rhs, file = "Problem_RTSS.csv", sep = ",")

Upgrade_rules_rhs <- apriori(tr,
                             parameter = list(supp=0.001, conf=0.8,
                                              maxlen=10,
                                              minlen=2),
                             appearance = list(default="lhs", rhs="Ticket: Upgrade"))

inspect(Upgrade_rules_rhs)
write(Upgrade_rules_rhs, file = "Upgrade_RTSS.csv", sep = ",")

Update_rules_rhs <- apriori(tr,
                            parameter = list(supp=0.001, conf=0.8,
                                             maxlen=10,
                                             minlen=2),
                            appearance = list(default="lhs", rhs="Ticket: Update"))

inspect(Update_rules_rhs)
write(Update_rules_rhs, file = "Update_RTSS.csv", sep = ",")
#_________________________________________________________________

shorter.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=3))
subset.rules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 3913
subset.association.rules. <- rules[-subset.rules] # remove subset rules.
inspect(subset.association.rules.)
# Filter rules with confidence greater than 0.4 or 40%
subRules<-rules[quality(rules)$confidence>0.2]
#Plot SubRules
plot(subRules)
top10subRules <- head(subRules, n = 20, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

plot(subRules2, method="graph", control=list(type="items"))
#_________________________________________________________________________
plot(sort(rules, by="lift"), method="graph",control=list(type="items"))
plot(rules)
head(quality(rules))
plot(rules, method="grouped")
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")

#sel <- plot(rules, measure=c("support", "lift"), shading = "confidence",interactive = TRUE)
#rules_df <- data.frame(A = labels(lhs(rules)), B = labels(rhs(rules)), rules@quality)
#subrules2 <- head(rules, n = 10, by = "lift")
#subrules2
plot(rules, method = "graph")
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)


#plot(subrules2, method = "paracoord")

# Plot histograms of quality criteria
par(mfrow = c(2, 2))
hist(rules_df$support, breaks = 100)
hist(rules_df$confidence, breaks = 100)
hist(rules_df$lift, breaks = 100)
hist(rules_df$count, breaks = 100)
par(mfrow = c(1,1))
#__________________________________________________________________________________
