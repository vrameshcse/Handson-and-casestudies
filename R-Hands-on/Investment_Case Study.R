# ****** Spark Funds Case Study *******#

### Objectives
# To identify Suitable investment Types among Best Countries under Best sector
# $5 To $15 million investments
# Must be English speaking Country
### END


#------ START OF DATA READ  ------
  # To read & assign the
  #   companies file data => comp.details data set
  #   round2  file data => invest.details data set
  #   mapping file data => sector.details data set
  # Using strip.white,  removed the white space & kept empty value
  # using na.string, Empty values are converted to NA
  
  comp.details <-
    read.delim(
      "companies.txt",
      header = TRUE,
      sep = "\t",
      strip.white = TRUE,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  
  invest.details <-
    read.csv(
      "rounds2.csv",
      header = TRUE,
      stringsAsFactors = FALSE,
      strip.white = TRUE,
      na.string = ""
    )
  
  sectors.details <-
    read.csv(
      "mapping_file.csv",
      header = TRUE,
      stringsAsFactors = FALSE,
      strip.white = TRUE,
      na.string = ""
    )
  

  # To verify the Na value present instead of empty value
  
  head(comp.details)
  head(invest.details)
  head(sectors.details)

#-----  END OF DATA READ ----- 



###--------START OF DATA CLEANSING --------------
#Assumptions: 
  #1. We cannot eliminate the closed companies as the analysis focus is to find the number
  #of investments accross sectors.
  #2. We cannot eliminate the duplicate companies from the companies data set as the names
  #are only unique, those companies are different in nature with focus on different sectors
  ##---- START Of Companies Data Set
    #Key columns to upper for merge purpose
      comp.details$permalink <-
        toupper(comp.details$permalink)
    
    #Replacing the country code's na column values with mode value of country code
      comp.details$country_code[is.na(comp.details$country_code)] <-
        names(sort(table(comp.details$country_code), decreasing = TRUE,na.rm=TRUE)[1])
    
    
  ##--- END of Companies Data set
    
  
  ##--- START Of  Investment (round2) 
    #Convering the Key columns cases to upper for comparison purpose
      invest.details$company_permalink <-
        toupper(invest.details$company_permalink)
    
    #To make  funded at format consistent
    # replacing / with - to have consistent date format (mm-dd-yyyy)
        sapply(
          invest.details$funded_at,
          FUN = function(x) {
            gsub(
              fixed = TRUE,
              pattern = "/",
              x = invest.details$funded_at[x],
              replacement = "-"
            )
          }
        )
      
    
  ##--- END of Investment (round2) 

###-----END of DATA CLEANSING -----



#===========================================================================
      # Check Point Questions Answers
#===========================================================================

###----- Table 1.1 --------
  
  #Q1:
    # Upper is needed as the language is case sensitive    
    length(unique(toupper(invest.details$company_permalink)))
  
  #Q2:
    # Upper is needed as the language is case sensitive    
    length(unique(toupper(comp.details$name)))
  
  #Q3:
    #Primary Key should not contain duplicates / null 
    #Length of the primary key value must be equal to number of rows of that data set
    #Below confirm funding round permalink is the primary key
    nrow(comp.details) == 
      length(unique(toupper(comp.details$permalink)))
    
  #Q4:
    #To identify any companies in rounds2 which are not in companies data set
    # Answer Zero shows that all companies in rounds2 present in companies data set
    length(invest.details$company_permalink
           [!invest.details$company_permalink %in% comp.details$permalink])
    
  #Q5:
    # Merge the comp.details data frame with Investment.details data frame
    # by Permalink & company permalink
    
    nrow(invest.details) # To note the count of round 2 before merging

    master.frame <-
      merge(
        invest.details,
        comp.details,
        by.x = "company_permalink",
        by.y = "permalink",
        all.X = TRUE
      )
    
    nrow(master.frame) # to confirm the all.X merge
    
###----- End of Table 1.1 ----- 
  

  
###----- Table 2.1 -----
  
  #Q1:
    sum(is.na(master.frame$raised_amount_usd))
  
  #Q2:
    #summary of raised_amount_usd for our reference before manipulating the NA values 
    #Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    #0   250000  1203000  3064000  4500000 17000000    19990
    summary(master.frame$raised_amount_usd,na.rm=TRUE)
  
    #Considering there are 19990 NA values and with most of outliers of raised_amount_usd
    #lying at the upperside, we thought it does not make sense to replace mean
    #for all NA values, rather 
    #1. Find the mean of the raised_amount_usd after excluding Outliers
    #2. Replace all the raised_amount_usd NA's with derived mean
    
    master.frame.iqr <-
      master.frame[!master.frame$raised_amount_usd %in% boxplot.stats(master.frame$raised_amount_usd)$out, ]
    
    mean.iqr <- mean(master.frame.iqr$raised_amount_usd, na.rm = TRUE)
    
    master.frame$raised_amount_usd[is.na(master.frame$raised_amount_usd)] <-
      mean.iqr
    
    #Just to compare the mean after modification
    mean(master.frame$raised_amount_usd, na.rm = TRUE)
  
###----- End of Table 2.1 ----- 
  
  
  
  
###----- START Of Table 3.1 -----
    
  ##Create mean.amt data set from master.frame 
  ##by aggregating the raised_amount_usd against funding round type
    mean.amt <- setNames(aggregate(
      master.frame$raised_amount_usd ~ master.frame$funding_round_type,
      master.frame,
      mean,
      na.action = na.pass,
      na.rm = TRUE
    ),c("funding_round_type","raised_amount_usd"))
    
  #Q1:
    #mean against Venture
    mean.venture <-
      mean.amt$raised_amount_usd[mean.amt$funding_round_type == "venture"]
  #Q2:
    #mean against Angel
    mean.angel <-
      mean.amt$raised_amount_usd[mean.amt$funding_round_type == "angel"]
  #Q3:
    #mean against seed
    mean.seed <-
      mean.amt$raised_amount_usd[mean.amt$funding_round_type == "seed"]
  
  #Q4:
    #mean against Venture
    mean.private.equity <-
      mean.amt$raised_amount_usd[mean.amt$funding_round_type == "private_equity"]
  
  #Q5:
    #Identifying investment round type which has mean in between 5 to 15           
    mean.5to15 <-
      mean.amt$funding_round_type[mean.amt$raised_amount_usd >= 5000000 &
                                    mean.amt$raised_amount_usd <= 15000000]
  
  #Q5:
    # Ignoring the undisclosed right investment round type as thats not specific round type
    # VENTURE type is the right investment round type to invest.
  
##----- End of Table 3.1 -----        
  
  
  
###----- Table 4.1 -----
  
  ##Sorting the countries based on total raised_amount_usd against Venture round type for each country
    ordered.funding.country <- aggregate(
        master.frame$raised_amount_usd
        [master.frame$funding_round_type ==
          "venture"],
        by = list(master.frame$country_code
                  [master.frame$funding_round_type ==
                    "venture"]),
        FUN = sum
      )
    
  ##Identifying top9 from the above sorted data set
    top9.country <- ordered.funding.country[with(ordered.funding.country,
                                       order(x, decreasing = TRUE)), ][1:9, ]
  ## Assinging Names to the columns
    names(top9.country) <- c("Country","raised_amount_usd")
  
  #Identifying top3 Engligh Speaking countries from Top9    
  
  #Q1:
    #USA" is the top Engligh Speaking Country
  
  #Q2:
    #"GBR" is the second English speaking country
  
  #Q3:
    #"IND" is the third English speaking country  
    

###----- End of Table 4.1 -----   

    
###----- Check point 5 -----

  #???????????????To BE REPLACED WITH BUSINESS RULES ?????????
    
    #To avoid manipulating the base data, new column primary_category is created 
    #First value before the delimiter is considered as primary category 
    # this created value will be used for sector mapping.
    
    master.frame$primary_category <-
      sapply(strsplit(
        master.frame$category_list,
        split = "|",
        fixed = TRUE
      ),
      "[",
      1)

    nrow(master.frame) # Number of Rows before merging
    
    master.frame <-
      merge(
        master.frame,
        sectors.details,
        by.x = "primary_category",
        by.y = "category_list",
        all.x = TRUE,na.rm=TRUE
      )
    
    nrow(master.frame) # Number of Rows after merging
###----- End Of checkpoint 5 -----
    
  
  
###----- Table 6.1 -----
    
  ## Assigning Top3 countries to countryX variable 
    country1 <- "USA"
    country2 <- "GBR"
    country3 <- "IND"
  ## Assigning funding Type, range to variables
    FT <- "venture"
    Lower.range <- 5000000
    Upper.range <- 15000000

  ## Addding plyr library 
  library("plyr")
  
  ## Creating Three Data Frames D1, D2, D3 for the identified top3 countries(USA, GBR & IND) respectively.
  ## These data frame contains funding details against Venture Type and amt in between lower & upper range  
    D1 <-
      subset(
        master.frame,
        (
          country_code == country1 &
            funding_round_type == FT &
            raised_amount_usd > Lower.range & raised_amount_usd < Upper.range
        )
      )
    
    D2 <-
      subset(
        master.frame,
        (
          country_code == country2 &
            funding_round_type == FT &
            raised_amount_usd > Lower.range & raised_amount_usd < Upper.range
        )
      )
    
    D3 <-
      subset(
        master.frame,
        (
          country_code == country3 &
            funding_round_type == FT &
            raised_amount_usd > Lower.range &
            raised_amount_usd < Upper.range
        )
      )
    
  ##Including a seperate column to include the total sum of investments for each main sector
    D1$total.amt.inv <- sapply(
      seq_len(nrow(D1)),
      FUN = function(x)
        sum(D1$raised_amount_usd[D1$main_sector == D1$main_sector[x]], na.rm = T)
    )
  
    D2$total.amt.inv <- sapply(
      seq_len(nrow(D2)),
      FUN = function(x)
        sum(D2$raised_amount_usd[D2$main_sector == D2$main_sector[x]], na.rm = T)
    )
  
    D3$total.amt.inv <- sapply(
      seq_len(nrow(D3)),
      FUN = function(x)
        sum(D3$raised_amount_usd[D3$main_sector == D3$main_sector[x]], na.rm = T)
    )
  
  ##Including a seperate column to include the count of investments for each main sector
    D1$investment_count <- sapply(
      seq_len(nrow(D1)),
      FUN = function(x) {
        length(D1$company_permalink[D1$main_sector  == D1$main_sector[x]]) -
          sum(is.na(D1$main_sector))
      }#To eliminate the NA values from the length count
    )
    
    D2$investment_count <- sapply(
      seq_len(nrow(D2)),
      FUN = function(x) {
        length(D2$company_permalink[D2$main_sector == D2$main_sector[x]]) -
          sum(is.na(D2$main_sector))
      }#To eliminate the NA values from the length count
    )
    
    D3$investment_count <- sapply(
      seq_len(nrow(D3)),
      FUN = function(x) {
        length(D3$company_permalink[D3$main_sector == D3$main_sector[x]]) -
          sum(is.na(D3$main_sector))
      }#To eliminate the NA values from the length count
    )
    
    #Q1:
      top3.investment.count <-
        c(
          c1 = length(D1$funding_round_permalink),
          C2 = length(D2$funding_round_permalink),
          c3 = length(D3$funding_round_permalink)
        )
    
    #Q2:
      top3.investment.total <-
        c(sum(D1$raised_amount_usd),
          sum(D2$raised_amount_usd),
          sum(D3$raised_amount_usd))
      
    ## Sorting the countwise sector for each country
      c1.sector = unique(na.omit(D1$main_sector[order(D1$investment_count, decreasing = TRUE)]))[1:3]
      c2.sector = unique(na.omit(D2$main_sector[order(D2$investment_count, decreasing = TRUE)]))[1:3]
      c3.sector = unique(na.omit(D3$main_sector[order(D3$investment_count, decreasing = TRUE)]))[1:3]

    #Q3:  
      first.investment.sector <-
        c(c1=c1.sector[1],c2=c2.sector[1],c3=c3.sector[1])
      
    #Q4:
      second.investment.sector <-
        c(c1=c1.sector[2],c2=c2.sector[2],c3=c3.sector[2])
      
    #Q5:
      third.investment.sector <-
        c(c1=c1.sector[3],c2=c2.sector[3],c3=c3.sector[3])
      
    #Q6:
      first.sector.count <-
        c(
          unique(na.omit(D1$investment_count[D1$main_sector == c1.sector[1]])),
          unique(na.omit(D2$investment_count[D2$main_sector == c2.sector[1]])),
          unique(na.omit(D3$investment_count[D3$main_sector == c3.sector[1]]))
        )
      
    #Q7:
      second.sector.count <-
        c(
          unique(na.omit(D1$investment_count[D1$main_sector == c1.sector[2]])),
          unique(na.omit(D2$investment_count[D2$main_sector == c2.sector[2]])),
          unique(na.omit(D3$investment_count[D3$main_sector == c3.sector[2]]))
        )
      
    #Q8:
      third.sector.count <-
        c(
          unique(na.omit(D1$investment_count[D1$main_sector == c1.sector[3]])),
          unique(na.omit(D2$investment_count[D2$main_sector == c2.sector[3]])),
          unique(na.omit(D3$investment_count[D3$main_sector == c3.sector[3]]))
        )
      
      #Q9: 
      D1.firsttop.sector <-
        aggregate(D1$raised_amount_usd[D1$main_sector == c1.sector[1]], list(D1$company_permalink[D1$main_sector == c1.sector[1]]), FUN =
                    sum)
      D2.firsttop.sector <-
        aggregate(D2$raised_amount_usd[D2$main_sector == c2.sector[1]], list(D2$company_permalink[D2$main_sector == c2.sector[1]]), FUN =
                    sum)
      D3.firsttop.sector <-
        aggregate(D3$raised_amount_usd[D3$main_sector == c3.sector[1]], list(D3$company_permalink[D3$main_sector == c3.sector[1]]), FUN =
                    sum)
      
      first.comp.firstsector <-
        c(c1 = D1.firsttop.sector[with(D1.firsttop.sector, order(x, decreasing = TRUE)),][1, 1],
          c2 = D2.firsttop.sector[with(D2.firsttop.sector, order(x, decreasing = TRUE)),][1, 1],
          c3 = D3.firsttop.sector[with(D3.firsttop.sector, order(x, decreasing = TRUE)),][1, 1])
      
      #Q10:
      D1.secondtop.sector <-
        aggregate(D1$raised_amount_usd[D1$main_sector == c1.sector[2]], list(D1$company_permalink[D1$main_sector == c1.sector[2]]), FUN =
                    sum)
      D2.secondtop.sector <-
        aggregate(D2$raised_amount_usd[D2$main_sector == c2.sector[2]], list(D2$company_permalink[D2$main_sector == c2.sector[2]]), FUN =
                    sum)
      D3.secondtop.sector <-
        aggregate(D3$raised_amount_usd[D3$main_sector == c3.sector[2]], list(D3$company_permalink[D3$main_sector == c3.sector[2]]), FUN =
                    sum)
      
      first.comp.secondsector <-
        c(c1 = D1.secondtop.sector[with(D1.secondtop.sector, order(x, decreasing = TRUE)),][1, 1],
          c2 = D2.secondtop.sector[with(D2.secondtop.sector, order(x, decreasing = TRUE)),][1, 1],
          c3 = D3.secondtop.sector[with(D3.secondtop.sector, order(x, decreasing = TRUE)),][1, 1])
      
      
###----- END of Check point 6 -----
      
      
###----- Checkpoint 7 -----
  
    library("ggplot2")
    
      top.sector <- setNames(
        data.frame(
          top.sectors.name <- c("venture", "seed", "private_equity"),
          top.sectors.totalinv <- c(
            sum(master.frame$raised_amount_usd[master.frame$funding_round_type == "venture"], na.rm =
                  T),
            sum(master.frame$raised_amount_usd[master.frame$funding_round_type == "seed"], na.rm =
                  T),
            sum(master.frame$raised_amount_usd[master.frame$funding_round_type == "private_equity"], na.rm =
                  T)
          ),
          top.sectors.avginv <- c(
            mean(master.frame$raised_amount_usd[master.frame$funding_round_type == "venture"], na.rm =
                   T),
            mean(master.frame$raised_amount_usd[master.frame$funding_round_type == "seed"], na.rm =
                   T),
            mean(master.frame$raised_amount_usd[master.frame$funding_round_type == "private_equity"], na.rm =
                   T)
          )
        ),
        c("Funding.type", "Total.Investment", "Avg.Investment")
      )
  
    #Q1:
      ggplot(top.sector,
             aes(
               x = "",
               y = Total.Investment,
               fill = factor(Funding.type)
             )) +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start = pi / 3) +
        geom_text(label = paste(
          top.sector$Funding.type,
          "Avg =",
          round(top.sector$Avg.Investment)
        ),
        position = "nudge") +
        xlab('Total Investment') +
        labs(fill = 'Total Investment')
    
    
      #Q2:    
      barplot(
        height = top9.country$raised_amount_usd,
        names(top9.country$Country),
        width = 0.5 ,
        # names.arg = top9.country$Country,
        space = 0.2,
        legend.text = top9.country$Country,
        col = topo.colors(9),
        main = "Top 9 Countries Total Investment in Venture",
        xlab = "country",
        ylab = "Total investments"
      )
      
    #Q3:
      top3.country.details <- setNames(data.frame(
        c(country1,country2,country3),
        c(first.investment.sector,second.investment.sector,third.investment.sector),
        c(first.sector.count,second.sector.count,third.sector.count)
        ),c("Country","Sector","Count"))
      
      ggplot(
        top3.country.details,
        aes(
          x=top3.country.details$Country,
          y = top3.country.details$Count,
          size = top3.country.details$Country,
          col = top3.country.details$Sector
        )
      )+ geom_point(position=position_jitter(width=0.6, height = 0.5)) +
        xlab("Top 3 countries") +
        ylab("# of investments")
      
            
          
###----- End Of Checkpoint 7 -----      
