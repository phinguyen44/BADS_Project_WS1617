################################################################################
# 
#   Feature creation using external information
#
################################################################################
# Description:
# Construct income average for Bundeslaender and age groups
# Group colors
# Group sizes
################################################################################
# Sources
#Income Statistics for Bundeslaender
# Sources:  
#  [1] https://de.statista.com/statistik/daten/studie/73061/umfrage/bundeslaender-im-vergleich---bruttoinlandsprodukt/
#  [2] https://de.statista.com/statistik/daten/studie/296286/umfrage/bruttojahresverdienst-der-arbeitnehmer-in-deutschland/
#  [3] Further income source: https://www.destatis.de/DE/Publikationen/Thematisch/VerdiensteArbeitskosten/Arbeitnehmerverdienste/BroschuereVerdiensteBlick0160013179004.pdf?__blob=publicationFile


################################################################################
################################################################################
### Income Features

load("BADS_WS1718_known_imp1.RData")

#### Construct income average for Bundeslaender
income.bl.dat <- data.frame(
                 matrix(nrow = length(levels(dat.input1$user_state)), 
                        ncol = 3))

# New data frame containing external information on Bundeslaender
colnames(income.bl.dat)  <- c("user_state", "income.bl", "West")
income.bl.dat$user_state <- as.factor(levels(dat.input1$user_state))
income.bl.dat$income.bl  <- c(42.62, 42.95, 35.42, 26.84, 46.75, 60.91,
                       42.73, 32.59, 25.02, 36.54, 33.58, 34.89,
                       27.89, 25.82, 30.48, 27.17)
income.bl.dat$West <- as.factor(c(1,1,0,0,1,1,1,1,0,1,1,1,0,0,1,0))

dat.input1 <- merge(dat.input1, income.bl.dat, by = "user_state" )

#### Construct income average for age groups

# New data frame containing external information on age
income.age.dat <- data.frame(
                  matrix(nrow = length(levels(as.factor(dat.input1$age))), 
                         ncol = 3))
colnames(income.age.dat)  <- c("age", "income.age", "age.group")
income.age.dat$age <- levels(as.factor(dat.input1$age))

# Assign income to groups and define age groups
income.age <- list()
age.group  <- list()
 for (i in 1:length(levels(as.factor(dat.input1$age)))){
     if (i < 20){
         income.age[i] <- 8.51
          age.group[i] <-  "below20"
     } else if (i < 25){ 
         income.age[i] <- 17.90
         age.group[i]  <- "below25"
     } else if (i < 30){
         income.age[i] <- 26.79
         age.group[i]  <- "below30"
     } else if (i < 35){
         income.age[i] <- 33.56
         age.group[i]  <- "below35"
     } else if (i < 40){
         income.age[i] <- 36.30
         age.group[i]  <- "below40"
     } else if (i < 45){
         income.age[i] <- 38.44
          age.group[i] <- "below45"
     } else if (i < 50){
         income.age[i] <- 38.83
          age.group[i] <- "below50"
     } else if (i < 55){
         income.age[i] <- 38.74
          age.group[i]  <- "below55"
     } else if (i < 60){
         income.age[i] <- 36.82
          age.group[i] <- "below60"
     } else if (i < 65){
         income.age[i] <- 34.43
         age.group[i]  <- "below65"
     } else {
         income.age[i] <- 13.68
          age.group[i] <- "above65"
     }
}

income.age.dat$income.age <- as.numeric(income.age)
income.age.dat$age.group <- as.factor(unlist(age.group))

dat.input1 <- merge(dat.input1, income.age.dat, by = "age" )

# Calculate indicator of income based on age and state

dat.input1$income.ind <- (dat.input1$income.bl * dat.input1$income.age)^0.5
dat.input1$price.inc.ratio <- dat.input1$item_price / dat.input1$income.ind

# Some clean up
rm(income.bl.dat, income.age.dat, income.age, age.group)


################################################################################
################################################################################
### Grouping Colors 

# Create dataframe containing groups of colors
colors.dat <- data.frame(matrix(nrow = length(levels(dat.input1$item_color)),
                                ncol = 2))
colnames(colors.dat) <- c("item_color", "item.color.group")
colors.dat$item_color <- c(sort(levels(dat.input1$item_color)))
colors.dat$item_color <- as.factor(unlist(colors.dat$item_color))

# Group colors according to basic color groups
basic.matching <- c(
    "?"  =  "other"  ,          "almond" = "brown" ,       "amethyst" = "purple" , 
    "ancient" = "yellow",       "anthracite" = "black",    "antique pink" = "purple",   
    "apricot" = "yellow",       "aqua" = "blue" ,          "aquamarine" = "blue",    
    "ash" = "grey"   ,          "aubergine" = "purple"  ,  "aviator" = "other" ,     
    "avocado" = "green",        "azure" = "blue",          "baltic blue" = "blue",   
    "basalt" = "grey",          "beige" = "white",         "berry" = "red",          
    "black" = "black",          "blau" = "blue",           "blue" = "blue",          
    "bordeaux" = "red",         "brown" = "brown",         "brwon" = "brown",        
    "caramel" = "yellow",       "champagner" = "white",    "cobalt blue" = "blue",   
    "cognac" = "brown",         "copper coin" = "brown",   "coral" = "red",          
    "creme" = "yellow" ,        "crimson" = "red",         "curled" = "other",       
    "currant purple" = "red",   "curry" = "yellow",        "dark denim" = "blue",   
    "dark garnet" = "black",    "dark grey" = "grey",      "dark navy" = "blue",     
    "dark oliv" = "green",      "darkblue" = "blue" ,      "denim" = "blue" ,       
    "ebony" = "black" ,         "ecru" = "grey"    ,       "floral" = "other",       
    "fuchsia" = "red",          "gold" = "yellow",         "graphite" = "grey",      
    "green" = "green",          "grey" = "grey",           "habana" = "other",         
    "hibiscus" = "red",         "ingwer" = "brown",        "iron" = "grey",          
    "ivory" = "white",          "jade" = "green",          "kanel" = "brown",        
    "khaki" = "brown" ,         "lemon" = "yellow",        "magenta" = "red" ,       
    "mahagoni" = "brown",       "mango" = "yellow",        "mint" = "green",         
    "mocca" = "brown",          "nature" = "other",        "navy" = "blue",          
    "ocher" = "brown",          "oliv" = "green",          "olive" = "green",        
    "opal" = "other",           "orange" = "red",          "pallid" = "grey",        
    "perlmutt" = "other",       "petrol" = "blue",         "pink" = "purple",           
    "purple" = "purple",        "red" = "red",             "silver" = "grey",        
    "stained" = "other",        "striped" = "other",       "terracotta" = "red",   
    "turquoise" = "blue",       "vanille" = "yellow",       "white" = "white",        
    "yellow" = "yellow" )

colors.dat$item.color.group <- as.factor(basic.matching)
    
dat.input1 <- merge(dat.input1, colors.dat, by = "item_color" )

# Some clean up
rm(colors.dat, basic.matching)

################################################################################
################################################################################
### Grouping sizes 
# Source: https://www.net-a-porter.com/am/sizechart.nap
# check out sizes on Zalando
# Tell clothing and shoes apart based on size range

sizes <- list()

# Clothing sizes
sizes$clothing.eu = c("xs", "XS", "s", "S", "m", "M", "l", "L", "xl", "XL", 
                   "xxl", "XXL", "xxxl", "XXXL","34", "36", "38", "40",
                  "42", "44", "46", "48", "50", "52", "54", "56" )
sizes$clothing.uk = c("6", "8", "10", "12", "14", "16", "18", "20", "22", "24")
 
# Shoe sizes(mostly women)
sizes$shoe.eu = c("35", "35+", "36", "36+",	"37", "37+", "38",	"38+",	
              "39",	"39+", "40", "40+",	"41", "41+",
              "42", "42+", "43", "43+", "44", "44+")
sizes$shoe.uk.us = c("3", "3+", "4", "4+", "5", "5+", "6", "6+", 
               "7", "7+", "8", "8+", "9", "9+", "9+",
               "10", "10+", "11", "11+")

# Children sizes
sizes$children.size = c("44",	"50",	"56",	"62",	"68",	"74",	"80",
                  "86",	"92",	"98",	"104",	"110",	"116",	"122",
                  "128","134",	"140",	"146",	"152",	"158",  "164",
                  "170",	"176")

sizes$pants.eu = c("3832", "3432", "3332", "3332", "23", "24", "25", "26",
             "27", "28", "29", "30", "31", "32", "34", "36", "38",
             "40", "42", "44", "48", "50", "52")

# Find unique sizes to determine category
k <- unlist(sizes)
z <- k[duplicated(k)]
sizes$unique <- k[!(k %in% z)]

vec.cloth <- unlist(sizes$unique[1:22])
vec.shoes <- unlist(sizes$unique[23:51])
vec.child <- unlist(sizes$unique[52:71])
vec.pants <- unlist(sizes$unique[72:82])


# Assign category to those items with unique size
# check whether this approach is correct
dat.input1  <- dat.input1 %>% 
    group_by(item_id) %>% 
    dplyr:: mutate(
        item.category = ifelse(item_size %in% vec.cloth, "clothing","unknown"),
        item.category = ifelse(item_size %in% vec.shoes, "shoes", item.category),
        item.category = ifelse(item_size %in% vec.child, "child", item.category),
        item.category = ifelse(item_size %in% vec.pants, "pants", item.category))
        
# Crosscheck by doing the same on grouped sizes
dat.input1  <- dat.input1 %>% 
    group_by(item_size) %>% 
    dplyr:: mutate(
        item.categorysz = ifelse( item_size %in% unlist(sizes$unique) &
                                  item_size %in% vec.cloth, "clothing","unknown"),
        item.categorysz = ifelse( item_size %in% unlist(sizes$unique) &
                              item_size %in% vec.shoes, "shoes", item.categorysz),
        item.categorysz = ifelse( item_size %in% unlist(sizes$unique) &
                              item_size %in% vec.child, "child", item.categorysz),
        item.categorysz = ifelse( item_size %in% unlist(sizes$unique) &
                                      item_size %in% vec.pants, "pants", item.categorysz))

# Classification of unknown sizes can be further refined
# if only even numbers exist, probably no shoes
# look also at range


###################################################################

# Below: Just some Notes 

###################################################################

# Check out distributions of item, item price across sizes and colors
agg =  aggregate(dat.input1$return, list(dat.input1$item.subcategory), 
                 FUN = function(x)c(mn = mean(x), n = length(x)))

item_agg <- do.call(data.frame, agg)

plot(item_agg$Group.1, item_agg$x.mn)

woe.scores <- woe(return ~ ., data = dat.input1, zeroadj = 1)
woe.scores$IV
library("caret") 
library(hmeasure)
library(InformationValue)
library(mlr)
library(klaR)

Score.fisher <- function(var, target){
    classMeans <- tapply(var, target, mean)
    classStds <- tapply(var, target, sd)
    classDiff <- abs(diff(classMeans))
    coefficient <- as.numeric(classDiff / sqrt(sum(classStds^2)))
    return(coefficient)
}

# Extract fisher score for all categorical variables
Scores.fisher <- apply(dat.input1[,sapply(dat.input1, is.numeric)], 
                       2, Score.fisher, dat.input1$return)


