# importing dataset into R
global.ss <-
  read.csv("Global Superstore.csv", stringsAsFactors = FALSE)

##################Checkpoint 1: (Data Understanding & Data Preparation)##########
# convert the data frame into transactions
Global.txns <-
  as(split(global.ss[, "Sub.Category"], global.ss[, "Order.ID"]), "transactions")

inspect(Global.txns) # view the transaction tables
summary(Global.txns)# view the summary

itemFrequencyPlot(Global.txns, topN = 20, type = "absolute")# plot the graph
# Binders, storage,& art are the most sold items
# copiers, appliances, machines, tables are the least sold items. So we will try to 
# evaluvate the association rules to improve the sale of least sold items

#################Checkpoint 2: (Association Rule Mining)#########
# Create transaction rules by trying with possible support values:
txn.rules <-
  apriori(Global.txns, parameter = list(support = 0.001, confidence = .80))

txn.rules <-
  apriori(Global.txns, parameter = list(support = 0.001, confidence = .50))
# lhs                                 rhs       support     confidence lift
# [1] {Appliances,Furnishings,Storage} => {Binders} 0.001118434 0.5957447  2.766036
# [2] {Appliances,Paper,Storage}       => {Binders} 0.001238266 0.6078431  2.822209

txn.rules <-
  apriori(Global.txns, parameter = list(support = 0.001, confidence = .70))
# as the total transaction is huge, zero rules returened for the above support
# hence reducing the support value further

# increasing teh suppport level to 0.0001
txn.rules <-
  apriori(Global.txns, parameter = list(support = 0.0001, confidence = .80))
# writing ... [392 rule(s)] done [0.00s].

#increasing the confidence level to reduce the number of rules
txn.rules <-
  apriori(Global.txns, parameter = list(support = 0.0001, confidence = .95))
# writing ... [281 rule(s)] done [0.00s].

# Increaing the number of itemsets to identify potential rules
txn.rules_1 <-
  apriori(Global.txns,
          parameter = list(
            support = 0.0001,
            confidence = .95,
            maxlen = 1
          ))

txn.rules_2 <-
  apriori(Global.txns,
          parameter = list(
            support = 0.0001,
            confidence = .95,
            maxlen = 2
          ))

txn.rules_3 <-
  apriori(Global.txns,
          parameter = list(
            support = 0.0001,
            confidence = .95,
            maxlen = 3
          ))
# no rules with 1,2&3 itemset rules are not avialble which makes the cut.

#################Checkpoint 3: (Rule Relevance/Evaluation)#############
txn.rules_4 <-
  apriori(Global.txns,
          parameter = list(
            support = 0.0001,
            confidence = .60,
            maxlen = 4
          ))
# writing ... [20 rule(s)] done [0.00s].
plot(txn.rules_4)
plot(txn.rules_4, method = "grouped")

inspect(head((sort(txn.rules_4, by = "confidence")), n = 10))
# lhs                               rhs           support      confidence lift
# [1]  {Appliances,Fasteners,Tables}  => {Furnishings} 0.0001198322 0.7500000  6.332631
# [2]  {Appliances,Fasteners,Tables}  => {Chairs}      0.0001198322 0.7500000  5.891512
# [3]  {Appliances,Fasteners,Tables}  => {Art}         0.0001198322 0.7500000  4.300561
# [4]  {Labels,Supplies,Tables}       => {Envelopes}   0.0001198322 0.7500000  8.128247
# [5]  {Fasteners,Paper,Tables}       => {Binders}     0.0001997204 0.7142857  3.316421
# [6]  {Copiers,Paper,Tables}         => {Art}         0.0002396645 0.6666667  3.822721
# [7]  {Appliances,Chairs,Tables}     => {Art}         0.0002796085 0.6363636  3.648961
# [8]  {Fasteners,Supplies,Tables}    => {Storage}     0.0001997204 0.6250000  3.451009
# [9]  {Accessories,Fasteners,Tables} => {Storage}     0.0001997204 0.6250000  3.451009
# [10] {Accessories,Fasteners,Tables} => {Binders}     0.0001997204 0.6250000  2.901869

plot(subset(txn.rules_4, subset = lift > 6 &
              confidence > 0.60), method = "graph")
# Tables can be suggested near the Envelopes, suppliers, labesls or fasterners,
# appliances, furnishings to imptove the sales of the table

plot(head((sort(txn.rules_6, by = "confidence")), n = 10),
     method = "graph",
     control = list(type = "itemsets"))
# The most significant rule is Appliances, bookcases, copiers, paper -> Binders

txn.rules_5 <-
  apriori(Global.txns,
          parameter = list(support = 0.0001,
                           confidence = .95))
# writing ... [281 rule(s)] done [0.00s].

inspect(head(
  txn.rules_5,
  n = 10,
  by = c("confidence", "lift")
))

# lhs                                                  rhs          support      confidence
# [1]  {Art,Fasteners,Labels,Paper,Phones}               => {Machines}   0.0001198322 1         
# [2]  {Binders,Chairs,Paper,Storage,Tables}             => {Appliances} 0.0001198322 1         
# [3]  {Bookcases,Copiers,Envelopes,Furnishings,Storage} => {Appliances} 0.0001198322 1         
# [4]  {Art,Chairs,Furnishings,Labels,Phones}            => {Appliances} 0.0001198322 1         
# [5]  {Art,Fasteners,Phones,Tables}                     => {Copiers}    0.0001198322 1         
# [6]  {Chairs,Envelopes,Phones,Tables}                  => {Copiers}    0.0001198322 1         
# [7]  {Accessories,Chairs,Envelopes,Phones,Tables}      => {Copiers}    0.0001198322 1         
# [8]  {Envelopes,Fasteners,Labels,Machines,Storage}     => {Copiers}    0.0001198322 1         
# [9]  {Binders,Envelopes,Fasteners,Labels,Machines}     => {Copiers}    0.0001198322 1         
# [10] {Binders,Envelopes,Fasteners,Machines,Storage}    => {Copiers}    0.0001198322 1         
# lift    
# [1]  17.60549
# [2]  14.84875
# [3]  14.84875
# [4]  14.84875
# [5]  11.80896
# [6]  11.80896
# [7]  11.80896
# [8]  11.80896
# [9]  11.80896
# [10] 11.80896

plot(head(subset(
  sort(txn.rules_5, by = "confidence"), subset = lift > 10.5
), n = 10), method = "graph")

plot(head(subset(
  sort(txn.rules_5, by = "confidence"), subset = lift > 10.5
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# significant rule: binders, chairs, paper, storage, tables -> appliances
# like binders fasteners can also be combined with many other products for better sales

txn.rules_6 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .95),
    appearance = list(rhs = c(labels = "Art"), default = "lhs")
  )
# writing ... [48 rule(s)] done [0.00s].

inspect(head((sort(txn.rules_6, by = "confidence")), n = 10))
# lhs                                         rhs   support      confidence lift
# [1]  {Appliances,Furnishings,Labels,Tables}   => {Art} 0.0001198322 1          5.734082
# [2]  {Accessories,Appliances,Paper,Tables}    => {Art} 0.0001198322 1          5.734082
# [3]  {Accessories,Appliances,Binders,Tables}  => {Art} 0.0001198322 1          5.734082
# [4]  {Copiers,Fasteners,Furnishings,Tables}   => {Art} 0.0001198322 1          5.734082
# [5]  {Copiers,Paper,Storage,Tables}           => {Art} 0.0001198322 1          5.734082
# [6]  {Envelopes,Fasteners,Furnishings,Tables} => {Art} 0.0001198322 1          5.734082
# [7]  {Chairs,Fasteners,Paper,Tables}          => {Art} 0.0001198322 1          5.734082
# [8]  {Appliances,Machines,Paper,Storage}      => {Art} 0.0001198322 1          5.734082
# [9]  {Bookcases,Fasteners,Phones,Supplies}    => {Art} 0.0001198322 1          5.734082
# [10] {Chairs,Envelopes,Furnishings,Supplies}  => {Art} 0.0001997204 1          5.734082


inspect(head(
  txn.rules_6,
  n = 10,
  by = c("confidence", "lift")
))
plot(head(subset(
  sort(txn.rules_6, by = "confidence"), subset = lift > 5.70
), n = 10), method = "graph")
# Tables can be suggested along with art items for better sale
plot(head(subset(
  sort(txn.rules_6, by = "confidence"), subset = lift > 5.70
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# Significant rule:  chairs, envelopes, furnishings, supplies -> art

txn.rules_7 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .95),
    appearance = list(rhs = c(labels = "Binders"), default = "lhs")
  )
# writing ... [60 rule(s)] done [0.00s].

inspect(head(
  txn.rules_7,
  n = 10,
  by = c("confidence", "lift")
))
# lhs                                        rhs       support      confidence lift
# [1]  {Fasteners,Paper,Phones,Tables}         => {Binders} 0.0001198322 1          4.64299
# [2]  {Art,Envelopes,Storage,Tables}          => {Binders} 0.0001198322 1          4.64299
# [3]  {Chairs,Paper,Phones,Tables}            => {Binders} 0.0001597763 1          4.64299
# [4]  {Accessories,Appliances,Machines,Paper} => {Binders} 0.0001198322 1          4.64299
# [5]  {Copiers,Envelopes,Machines,Paper}      => {Binders} 0.0001198322 1          4.64299
# [6]  {Bookcases,Fasteners,Labels,Machines}   => {Binders} 0.0001198322 1          4.64299
# [7]  {Accessories,Art,Chairs,Machines}       => {Binders} 0.0001597763 1          4.64299
# [8]  {Accessories,Machines,Paper,Phones}     => {Binders} 0.0001198322 1          4.64299
# [9]  {Appliances,Bookcases,Copiers,Paper}    => {Binders} 0.0001997204 1          4.64299
# [10] {Appliances,Copiers,Paper,Phones}       => {Binders} 0.0001198322 1          4.64299
plot(head(subset(
  sort(txn.rules_7, by = "confidence"), subset = lift > 4.6
), n = 10), method = "graph")
# machines, papers, phones can be suggested along with binders for better sale of the
# former items

plot(head(subset(
  sort(txn.rules_7, by = "confidence"), subset = lift > 4.6
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# significant rule: Appliances, bookcases, copiers, paper -> Binders

txn.rules_8 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .95),
    appearance = list(rhs = c(labels = "Chairs"), default = "lhs")
  )
# writing ... [22 rule(s)] done [0.00s].

inspect(head(
  txn.rules_8,
  n = 10,
  by = c("confidence", "lift")
))
# lhs                                          rhs      support      confidence lift   
# [1]  {Accessories,Appliances,Paper,Tables}     => {Chairs} 0.0001198322 1          7.85535
# [2]  {Appliances,Art,Paper,Tables}             => {Chairs} 0.0001597763 1          7.85535
# [3]  {Bookcases,Copiers,Labels,Tables}         => {Chairs} 0.0001198322 1          7.85535
# [4]  {Copiers,Envelopes,Labels,Tables}         => {Chairs} 0.0001198322 1          7.85535
# [5]  {Accessories,Copiers,Phones,Tables}       => {Chairs} 0.0002396645 1          7.85535
# [6]  {Art,Fasteners,Paper,Tables}              => {Chairs} 0.0001198322 1          7.85535
# [7]  {Envelopes,Labels,Storage,Tables}         => {Chairs} 0.0001198322 1          7.85535
# [8]  {Appliances,Fasteners,Furnishings,Phones} => {Chairs} 0.0001198322 1          7.85535
# [9]  {Accessories,Appliances,Art,Paper,Tables} => {Chairs} 0.0001198322 1          7.85535
# [10] {Appliances,Art,Binders,Paper,Tables}     => {Chairs} 0.0001198322 1          7.85535

plot(head(subset(
  sort(txn.rules_8, by = "confidence"), subset = lift > 7.8
), n = 10), method = "graph")
# tables can be suggested aloing with chairs for beter sales of tables

plot(head(subset(
  sort(txn.rules_8, by = "confidence"), subset = lift > 7.8
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# significant rule: accessores, copiers, phones, tables -> Chairs

txn.rules_9 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .60),
    appearance = list(rhs = c(labels = "Machines"), default = "lhs")
  )
# writing ... [9 rule(s)] done [0.00s].

inspect(head(
  txn.rules_9,
  n = 10,
  by = c("confidence", "lift")
))

# lhs                                             rhs        support      confidence lift
# [1] {Art,Fasteners,Labels,Paper,Phones}          => {Machines} 0.0001198322 1.00       17.60549
# [2] {Fasteners,Labels,Paper,Phones}              => {Machines} 0.0001198322 0.75       13.20411
# [3] {Bookcases,Envelopes,Tables}                 => {Machines} 0.0001198322 0.60       10.56329
# [4] {Accessories,Fasteners,Labels,Phones}        => {Machines} 0.0001198322 0.60       10.56329
# [5] {Binders,Copiers,Envelopes,Fasteners,Labels} => {Machines} 0.0001198322 0.60       10.56329
# [6] {Binders,Bookcases,Paper,Phones,Storage}     => {Machines} 0.0001198322 0.60       10.56329
# [7] {Art,Labels,Paper,Phones,Storage}            => {Machines} 0.0001198322 0.60       10.56329
# [8] {Art,Binders,Furnishings,Paper,Phones}       => {Machines} 0.0001198322 0.60       10.56329
# [9] {Art,Binders,Paper,Phones,Storage}           => {Machines} 0.0001198322 0.60       10.56329

plot(head(subset(
  sort(txn.rules_9, by = "confidence"), subset = lift > 10.5
), n = 10), method = "graph")
# machines can be suggested along with papers & binders for better sales

plot(head(subset(
  sort(txn.rules_9, by = "confidence"), subset = lift > 10.5
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# signifcant rule: arts, fasteners, labels, paper, phones -> machines

txn.rules_10 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .60),
    appearance = list(rhs = c(labels = "Copiers"), default = "lhs")
  )
# writing ... [62 rule(s)] done [0.00s].

inspect(head(
  txn.rules_10,
  n = 10,
  by = c("confidence", "lift")
))
# lhs                                                     rhs       support      confidence
# [1]  {Art,Fasteners,Phones,Tables}                        => {Copiers} 0.0001198322 1         
# [2]  {Chairs,Envelopes,Phones,Tables}                     => {Copiers} 0.0001198322 1         
# [3]  {Accessories,Chairs,Envelopes,Phones,Tables}         => {Copiers} 0.0001198322 1         
# [4]  {Envelopes,Fasteners,Labels,Machines,Storage}        => {Copiers} 0.0001198322 1         
# [5]  {Binders,Envelopes,Fasteners,Labels,Machines}        => {Copiers} 0.0001198322 1         
# [6]  {Binders,Envelopes,Fasteners,Machines,Storage}       => {Copiers} 0.0001198322 1         
# [7]  {Appliances,Bookcases,Envelopes,Furnishings,Storage} => {Copiers} 0.0001198322 1         
# [8]  {Envelopes,Fasteners,Labels,Storage,Supplies}        => {Copiers} 0.0001198322 1         
# [9]  {Art,Binders,Chairs,Furnishings,Supplies}            => {Copiers} 0.0001198322 1         
# [10] {Binders,Bookcases,Chairs,Labels,Paper}              => {Copiers} 0.0001198322 1         
# lift    
# [1]  11.80896
# [2]  11.80896
# [3]  11.80896
# [4]  11.80896
# [5]  11.80896
# [6]  11.80896
# [7]  11.80896
# [8]  11.80896
# [9]  11.80896
# [10] 11.80896

plot(head(subset(
  sort(txn.rules_10, by = "confidence"), subset = lift > 10.5
), n = 10), method = "graph")
# copiers can be suggested near envelopes and binders

plot(head(subset(
  sort(txn.rules_10, by = "confidence"), subset = lift > 10.5
), n = 10),
method = "graph",
control = list(type = "itemsets"))

txn.rules_11 <-
  apriori(
    Global.txns,
    parameter = list(support = 0.0001,
                     confidence = .60),
    appearance = list(rhs = c(labels = "Appliances"), default = "lhs")
  )
# writing ... [25 rule(s)] done [0.00s].

inspect(head(
  txn.rules_11,
  n = 10,
  by = c("confidence", "lift")
))

plot(head(subset(
  sort(txn.rules_11, by = "confidence"), subset = lift > 10.5
), n = 10), method = "graph")
# appliances can be suggested near binders and art

plot(head(subset(
  sort(txn.rules_11, by = "confidence"), subset = lift > 10.5
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# significant rules
# binders, chairs, paper, storage, tools -> appliances
# arts,chairs,furnishings, labels, phones - > appliances

txn.rules_12 <-
  apriori(Global.txns,
          parameter = list(
            support = 0.0001,
            confidence = .95),
          appearance = list(rhs = c(labels = "Storage"), default = "lhs")
  )
# writing ... [59 rule(s)] done [0.00s].

inspect(head(
  txn.rules_12,
  n = 10,
  by = c("confidence", "lift")
))
# lhs                                         rhs       support      confidence lift    
# [1]  {Copiers,Fasteners,Supplies,Tables}      => {Storage} 0.0001198322 1          5.521614
# [2]  {Bookcases,Chairs,Supplies,Tables}       => {Storage} 0.0001198322 1          5.521614
# [3]  {Chairs,Envelopes,Fasteners,Tables}      => {Storage} 0.0001198322 1          5.521614
# [4]  {Binders,Envelopes,Fasteners,Tables}     => {Storage} 0.0001198322 1          5.521614
# [5]  {Fasteners,Furnishings,Paper,Tables}     => {Storage} 0.0001198322 1          5.521614
# [6]  {Art,Binders,Envelopes,Tables}           => {Storage} 0.0001198322 1          5.521614
# [7]  {Copiers,Envelopes,Machines,Supplies}    => {Storage} 0.0001597763 1          5.521614
# [8]  {Bookcases,Copiers,Labels,Machines}      => {Storage} 0.0001597763 1          5.521614
# [9]  {Accessories,Bookcases,Copiers,Machines} => {Storage} 0.0001198322 1          5.521614
# [10] {Chairs,Copiers,Envelopes,Machines}      => {Storage} 0.0001198322 1          5.521614
plot(head(subset(
  sort(txn.rules_12, by = "confidence"), subset = lift > 5.5
), n = 10), method = "graph")

plot(head(subset(
  sort(txn.rules_12, by = "confidence"), subset = lift > 5.5
), n = 10),
method = "graph",
control = list(type = "itemsets"))
# significant rule: bookcase, copiers, labels, machines -> storage

# Conclusion:
  # 1. Binders goes well with most of the items, so binders can be suggested with any itemset
  # as frequently bought items
  # 2. similary fasteners alos goes will with most of the items and hence can be suggested
  # as frequenty bought items with most of the itemset
  # 3. Art items can be suggested along with tables, chairs furnishings and supplies
  # 4. Storage can be suggested along with copiers, chairs and envelopes
  # 3. as our primary focus is to improve the sales of the least frequent items like
  # copiers, appliances, machines & tables, using association rules these items can be combined
  # with the suggested itemset for better sales.
  # copiers can be suggested with machines, arts, fasteners and binders
  # appliances can be suggested with acessories, binders, bookcases and art
  # machine can be suggested with copiers, binders, storage and accessories
  # Tables can be suggested with art, accessories, copiers, phones and chairs
