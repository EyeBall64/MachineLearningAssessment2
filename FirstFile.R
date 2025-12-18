#########introduction###########################################################
#this will only use a maximium of 80 charcters across for ease of use
#we are told by the data set that there are some missing values from stalk-root
#I have decided to remove these as there are still several thousand lines of
#data remaining.#
#the colours sometimes refer to cinnamon and buff which R does not recognize as
#colours, I have decided to instead refer to these as sienna and goldenrod4 
#instead as these are recognized by R as colours and will make whatever algrithm
#I must apply to them easier to use.
######library########
install.packages("tidyverse")
library(tidyverse)
install.packages("hnp")
library(hnp)
install.packages("ISLR2")
library(ISLR2)
install.packages("boot")
library(boot)
install.packages("tree")
library(tree)
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
install.packages("dplyr")
library(dplyr)
install.packages("caTools")
library(caTools)
install.packages("rpart")
library(rpart)
colNames <- c("edible", "capShape", "capSurface", "capColor", "bruises", "odor",
              "gillAttachment", "gillSpacing", "gillSize", "gillColor", 
              "stalkShape", "stalkRoot", "stalkSurfaceAboveRing", 
              "stalkSurfaceBelowRing", "stalkColorAboveRing", 
              "stalkColorBelowRing", "veilType", "veilColor", "ringNumber", 
              "ringType", "sporePrintColor", "population", "habitat") 
                    #creates a string of column titles
capShape_variables <- c("bell", "conical", "convex", "flat", 
                        "knobbed", "sunken")
capSurface_variables <- c("fibrous", "grooves", "scaly", "smooth")
capColor_variables <- c("brown", "goldenrod4", "sienna", "gray", "green",
                        "pink", "purple", "red", "white", "yellow")
odor_variables <- c("almond", "anise", "creosote", "fishy", "foul", "musty",
                    "none", "pungent", "spicy")
gillAttachment_variables <- c("attached", "descending", "free", "notched")
gillSpacing_variables <- c("close", "crowded", "distant")
gillSize_variables <- c("broad", "narrow")
gillColor_variables <- c("black", "brown", "goldenrod4", "chocolate",
                         "gray","green", "orange", "pink", "purple","red",
                         "white" ,"yellow")
stalkShape_variables <- c("enlarging", "tapering")
stalkRoot_variables <- c("bulbous", "club", "cup", "equal", "rhizomorphs",
                         "rooted", NA)
stalkSurfaceAboveRing_variables <- c("fibrous", "scaly", "silky", "smooth")
stalkSurfaceBelowRing_variables <- c("fibrous", "scaly", "silky", "smooth")
stalkColorAboveRing_variables <- c("brown", "goldenrod4", "sienna", "gray",
                                   "orange", "pink", "red", "white", "yellow")
stalkColorBelowRing_variables <- c("brown", "goldenrod4", "sienna", "gray",
                                   "orange", "pink", "red", "white", "yellow")
veilType_variables <- c("partial", "universal")
veilColor_variables <- c("brown", "orange","white", "yellow")
ringNumber_variables <- c(0,1,2)
ringType_variables <- c("cobwebby", "evanescent", "flaring", "large", "none",
                        "pendant", "sheathing", "zone")
sporePrintColor_variables <- c("black", "brown", "goldenrod4", "chocolate", 
                               "green", "orange", "purple", "white", "yellow")
population_variables <- c("abundant", "clustered", "numerous", "scattered",
                          "several", "solitary")
habitat_variables <- c("grasses", "leaves", "meadows", "paths", "urban",
                       "waste", "woods")

NumberOf <- function(data, check){
  n = 0
  for (i in data){
    if (i == check){
      n = n+1
    }
  }
  return(n)
}

#####data cleaning#######

#loading the data into R using a working directory 
df_original <- read.csv("agaricus-lepiotaData.data", header = FALSE)
#the title of the columns as well as the contents are not very useful,
#the agaricus-lepiota.names file (among other things) tells use 
#what each of the letters means

df_dirty<- df_original#creates a new data set so the 
                      #original will remain untouched
colnames(df_dirty) <- colNames#makes the column 
                              #titles what is in the array above

summary(df_dirty) #gives us a summary of the data for each column
#all are characters checking the agaricus-lepiota.names file again most of these
#are correct however there are a couple the could be turned into logical or
#numerical variables and for the ones that are not changing the letter what it
#is actually describing will massivly help readibility

#we are told that the data set has some missing values in the StalkRoot column,
#for now they will be converted to NA's to highlight they are missing
#the following checks that all instance of the columns contains what is expected
#and changes it to a better data type or variable
  #edible
  sum(!df_dirty$edible %in% c("e", "p"))
  df_dirty$edible <- df_dirty$edible == "e"

  #capShape
  sum(!df_dirty$capShape %in% c("b", "c", "x", "f", "k", "s"))
  df_dirty$capShape <- capShape_variables[match(df_dirty$capShape, 
                                                c("b", "c", "x","f", "k", "s"))]
  #capSurface
  sum(!df_dirty$capSurface %in% c("f", "g", "y", "s"))
  df_dirty$capSurface <- capSurface_variables[match(df_dirty$capSurface, 
                                                    c("f", "g","y", "s"))]
  #capColor
  sum(!df_dirty$capColor %in% 
        c("n", "b", "c", "g", "r", "p", "u", "e", "w", "y"))
  df_dirty$capColor <- capColor_variables[match(df_dirty$capColor, 
                            c("n", "b", "c", "g","r", "p", "u", "e","w", "y"))]
  #bruises
  sum(!df_dirty$bruises %in% c("t", "f"))
  df_dirty$bruises <- df_dirty$bruises == "t"  
  
  #odor
  sum(!df_dirty$odor %in% c("a", "l", "c", "y", "f", "m", "n", "p", "s"))
  df_dirty$odor <- odor_variables[match(df_dirty$odor, 
                                c("a", "l", "c", "y", "f", "m", "n","p", "s"))]
  #gillAttachment
  sum(!df_dirty$gillAttachment %in% c("a", "d", "f", "n"))
  df_dirty$gillAttachment <- 
    gillAttachment_variables[match(df_dirty$gillAttachment, 
                                                      c("a","d", "f", "n"))]
  #gillSpacing
  sum(!df_dirty$gillSpacing %in% c("c", "w", "d"))
  df_dirty$gillSpacing <- gillSpacing_variables[match(df_dirty$gillSpacing, 
                                                c("c", "w", "d"))]
  #gillSize
  sum(!df_dirty$gillSize %in% c("b", "n"))
  df_dirty$gillSize <- gillSize_variables[match(df_dirty$gillSize, 
                                                c("b", "n"))]
  #gillColor
  sum(!df_dirty$gillColor %in% 
        c("k", "n", "b", "h", "g", "r", "o", "p", "u", "e", "w", "y"))
  df_dirty$gillColor <- gillColor_variables[match(df_dirty$gillColor, 
                                            c("k", "n", "b", "h", "g", "r",
                                              "o", "p", "u", "e", "w", "y"))]
  #stalkShape
  sum(!df_dirty$stalkShape %in% c("e", "t"))
  df_dirty$stalkShape <- stalkShape_variables[match(df_dirty$stalkShape, 
                                                    c("e", "t"))]
  #stalkRoot
  sum(!df_dirty$stalkRoot %in% c("b", "c", "u", "e", "z", "r", "?"))
  df_dirty$stalkRoot <- stalkRoot_variables[match(df_dirty$stalkRoot, 
                                        c("b", "c", "u", "e", "z", "r", "?"))]
  #stalkSurfaceAboveRing
  sum(!df_dirty$stalkSurfaceAboveRing %in% c("f", "y", "k", "s"))
  df_dirty$stalkSurfaceAboveRing <- 
    stalkSurfaceAboveRing_variables[match(df_dirty$stalkSurfaceAboveRing,
                                          c("f", "y", "k", "s"))]
  #stalkSurfaceBelowRing
  sum(!df_dirty$stalkSurfaceBelowRing %in% c("f", "y", "k", "s"))
  df_dirty$stalkSurfaceBelowRing <- 
    stalkSurfaceBelowRing_variables[match(df_dirty$stalkSurfaceBelowRing, 
                                          c("f", "y", "k", "s"))]
  #stalkColorAboveRing
  sum(!df_dirty$stalkColorAboveRing %in% 
        c("n", "b", "c", "g", "o", "p", "e", "w", "y"))
  df_dirty$stalkColorAboveRing <- 
    stalkColorAboveRing_variables[match(df_dirty$stalkColorAboveRing, 
                                        c("n", "b", "c", "g", "o",
                                          "p", "e", "w", "y"))]
  #stalkColorBelowRing
  sum(!df_dirty$stalkColorBelowRing %in% 
        c("n", "b", "c", "g", "o", "p", "e", "w", "y"))
  df_dirty$stalkColorBelowRing <- 
    stalkColorBelowRing_variables[match(df_dirty$stalkColorBelowRing, 
                              c("n", "b", "c", "g", "o", "p", "e", "w", "y"))]
  #veilType
  sum(!df_dirty$veilType %in% c("p", "u"))
  df_dirty$veilType <- veilType_variables[match(df_dirty$veilType, 
                                                c("p", "u"))]
  #veilColor
  sum(!df_dirty$veilColor %in% c("n", "o", "w", "y"))
  df_dirty$veilColor <- veilColor_variables[match(df_dirty$veilColor, 
                                            c("n", "o", "w", "y"))]
  #ringNumber
  sum(!df_dirty$ringNumber %in% c("n", "o", "t"))
  df_dirty$ringNumber <- ringNumber_variables[match(df_dirty$ringNumber, 
                                                    c("n", "o", "t"))]
  #ringType
  sum(!df_dirty$ringType %in% c("c", "e", "f", "l", "n", "p", "s", "z"))
  df_dirty$ringType <- ringType_variables[match(df_dirty$ringType, 
                                    c("c", "e", "f", "l", "n", "p", "s", "z"))]
  #sporePrintColor
  sum(!df_dirty$sporePrintColor %in% 
        c("k", "n", "b", "h", "r", "o", "u", "w", "y"))
  df_dirty$sporePrintColor<-
    sporePrintColor_variables[match(df_dirty$sporePrintColor, 
                              c("k", "n", "b", "h", "r", "o", "u", "w", "y"))]
  #population
  sum(!df_dirty$population %in% c("a", "c", "n", "s", "v", "y"))
  df_dirty$population <- population_variables[match(df_dirty$population, 
                                            c("a", "c", "n", "s", "v", "y"))]
  #habitat
  sum(!df_dirty$habitat %in% c("g", "l", "m", "p", "u","w", "d"))
  df_dirty$habitat <- habitat_variables[match(df_dirty$habitat, 
                                        c("g", "l", "m", "p", "u","w", "d"))]
  
sum(is.na(df_dirty)) # this checks the number of NA's 
# there are 2,480
df <- na.omit(df_dirty) #removes NA's
sum(is.na(df)) # this checks the number of NA's again
#there are now none

sum(duplicated(df))
#this checks if there are any duplicated rows in the data set
#there are none


####pie charts#####

# the following creates pie charts showing the proportions of 
#the variables for each of the cap related features
par(mfrow = c(1, 3))#allows multiple graphs to be displayed at 
                    #once to they can be seen and compared easier
pie( c(NumberOf(df$capShape, "bell"),
       NumberOf(df$capShape, "conical"),
       NumberOf(df$capShape, "convex"), 
       NumberOf(df$capShape, "flat"), 
       NumberOf(df$capShape, "knobbed"), 
       NumberOf(df$capShape, "sunken") ) ,
     capShape_variables, main = "cap-shape",
     col = rainbow(length(capShape_variables)))
pie( c(NumberOf(df$capSurface, "fibrous"),
       NumberOf(df$capSurface, "groves"),
       NumberOf(df$capSurface, "scaly"),
       NumberOf(df$capSurface, "smooth")) , 
     capSurface_variables, main = "cap-surface",
     col = rainbow(length(capSurface_variables)))
pie( c(NumberOf(df$capColor, "brown"),
       NumberOf(df$capColor, "goldenrod4"),
       NumberOf(df$capColor, "sienna"),
       NumberOf(df$capColor, "gray"),
       NumberOf(df$capColor, "green"),
       NumberOf(df$capColor, "pink"),
       NumberOf(df$capColor, "purple"),
       NumberOf(df$capColor, "red"),
       NumberOf(df$capColor, "white"),
       NumberOf(df$capColor, "yellow")) , 
     capColor_variables, main = "capColor",col = capColor_variables)

# the following creates pie charts showing the proportions 
#of the variables for bruises, odor and spore print color
par(mfrow = c(1, 3))#allows multiple graphs to be displayed at 
                    #once to they can be seen and compared easier
pie( c(NumberOf(df$bruises, TRUE),NumberOf(df$bruises, FALSE)) ,
     c("true", "false"), main = "bruises",col = rainbow(2))
pie( c(NumberOf(df$odor, "almond"),
       NumberOf(df$odor, "anise"),
       NumberOf(df$odor, "creosote"),
       NumberOf(df$odor, "fishy"),
       NumberOf(df$odor, "foul"),
       NumberOf(df$odor, "musty"),
       NumberOf(df$odor, "none"),
       NumberOf(df$odor, "pungent"),
       NumberOf(df$odor, "spicy")) , 
     odor_variables, main = "odor",col = rainbow(length(odor_variables)))
pie( c(NumberOf(df$sporePrintColor, "black"),
       NumberOf(df$sporePrintColor, "brown"),
       NumberOf(df$sporePrintColor, "goldenrod4"),
       NumberOf(df$sporePrintColor, "chocolate"),
       NumberOf(df$sporePrintColor, "green"),
       NumberOf(df$sporePrintColor, "orange"),
       NumberOf(df$sporePrintColor, "purple"),
       NumberOf(df$sporePrintColor, "white"),
       NumberOf(df$sporePrintColor, "yellow")) , 
     sporePrintColor_variables, main = "spore Print Color",
     col=sporePrintColor_variables)

# the following creates pie charts showing the 
#proportions of the variables for gill related features
par(mfrow = c(2, 2))#allows multiple graphs to be displayed
                    #at once to they can be seen and compared easier
pie( c(NumberOf(df$gillAttachment, "attached"),
       NumberOf(df$gillAttachment, "descending"),
       NumberOf(df$gillAttachment, "free"),
       NumberOf(df$gillAttachment, "notched")) ,
     gillAttachment_variables, main = "gill-attachment",
     col = rainbow(length(gillAttachment_variables)))
pie( c(NumberOf(df$gillSpacing, "close"),
       NumberOf(df$gillSpacing, "crowded"),
       NumberOf(df$gillSpacing, "distant")),
     gillSpacing_variables, main = "gill-Spacing",
     col = rainbow(length(gillSpacing_variables)))
pie( c(NumberOf(df$gillSize, "broad"),
       NumberOf(df$gillSize, "narrow")), 
     gillSize_variables, main = "gill-Size",
     col = rainbow(length(gillSize_variables)))
pie( c(NumberOf(df$gillColor, "black"),
       NumberOf(df$gillColor, "brown"),
       NumberOf(df$gillColor, "goldenrod4"),
       NumberOf(df$gillColor, "chocolate"),
       NumberOf(df$gillColor, "gray"),
       NumberOf(df$gillColor, "green"),
       NumberOf(df$gillColor, "orange"),
       NumberOf(df$gillColor, "pink"),
       NumberOf(df$gillColor, "purple"),
       NumberOf(df$gillColor, "red"),
       NumberOf(df$gillColor, "white"),
       NumberOf(df$gillColor, "yellow")) , 
     gillColor_variables, main = "gill-Color",col =gillColor_variables)

# the following creates pie charts showing the 
#proportions of the variables for the stalk
par(mfrow = c(2, 3))#allows multiple graphs to be displayed
#at once to they can be seen and compared easier
pie( c(NumberOf(df$stalkShape, "enlarging"),
       NumberOf(df$stalkShape, "tapering")),
     stalkShape_variables, main = "stalk Shape",
     col = rainbow(length(stalkShape_variables)))
pie( c(NumberOf(df$stalkSurfaceAboveRing, "fibrous"),
       NumberOf(df$stalkSurfaceAboveRing, "scaly"),
       NumberOf(df$stalkSurfaceAboveRing, "silky"),
       NumberOf(df$stalkSurfaceAboveRing, "smooth")), 
     stalkSurfaceAboveRing_variables, main = "stalk-Surface-Above-Ring",
     col = rainbow(length(stalkSurfaceAboveRing_variables)))
pie( c(NumberOf(df$stalkColorAboveRing, "brown"),
       NumberOf(df$stalkColorAboveRing, "goldenrod4"),
       NumberOf(df$stalkColorAboveRing, "sienna"),
       NumberOf(df$stalkColorAboveRing, "gray"),
       NumberOf(df$stalkColorAboveRing, "orange"),
       NumberOf(df$stalkColorAboveRing, "pink"),
       NumberOf(df$stalkColorAboveRing, "red"),
       NumberOf(df$stalkColorAboveRing, "white"),
       NumberOf(df$stalkColorAboveRing, "yellow")) , 
     stalkColorAboveRing_variables, main = "stalk-Color-Above-Ring",
     col = rainbow(length(stalkColorAboveRing_variables)))
pie( c(NumberOf(df$stalkRoot, "bulbous"),
       NumberOf(df$stalkRoot, "club"),
       NumberOf(df$stalkRoot, "cup"),
       NumberOf(df$stalkRoot, "equal"),
       NumberOf(df$stalkRoot, "rhizomorphs"),
       NumberOf(df$stalkRoot, "rooted")),
     stalkRoot_variables, main = "stalk-Root",
     col = rainbow(length(stalkRoot_variables)))
pie( c(NumberOf(df$stalkSurfaceBelowRing, "fibrous"),
       NumberOf(df$stalkSurfaceBelowRing, "scaly"),
       NumberOf(df$stalkSurfaceBelowRing, "silky"),
       NumberOf(df$stalkSurfaceBelowRing, "smooth")), 
     stalkSurfaceBelowRing_variables, main = "stalk-Surface-Below-Ring",
     col = rainbow(length(stalkSurfaceBelowRing_variables)))
pie( c(NumberOf(df$stalkColorBelowRing, "brown"),
       NumberOf(df$stalkColorBelowRing, "goldenrod4"),
       NumberOf(df$stalkColorBelowRing, "sienna"),
       NumberOf(df$stalkColorBelowRing, "gray"),
       NumberOf(df$stalkColorBelowRing, "orange"),
       NumberOf(df$stalkColorBelowRing, "pink"),
       NumberOf(df$stalkColorBelowRing, "red"),
       NumberOf(df$stalkColorBelowRing, "white"),
       NumberOf(df$stalkColorBelowRing, "yellow")) , 
     stalkColorBelowRing_variables, main = "stalk-Color-Below-Ring",
     col = rainbow(length(stalkColorBelowRing_variables)))

# the following creates pie charts showing the 
#proportions of the variables for veil and ring related features
par(mfrow = c(2, 2))#allows multiple graphs to be displayed
#at once to they can be seen and compared easier
pie( c(NumberOf(df$veilType, "partial"),
       NumberOf(df$veilType, "universal")) ,
     veilType_variables, main = "veil-Type",
     col = rainbow(length(veilType_variables)))
pie( c(NumberOf(df$veilColor, "brown"),
       NumberOf(df$veilColor, "orange"),
       NumberOf(df$veilColor, "white"),
       NumberOf(df$veilColor, "yellow")),
     veilColor_variables, main = "veil-Color",
     col = rainbow(length(veilColor_variables)))
pie( c(NumberOf(df$ringNumber, 0),
       NumberOf(df$ringNumber, 1),
       NumberOf(df$ringNumber, 2)), 
     ringNumber_variables, main = "ring-Number",
     col = rainbow(length(ringNumber_variables)))
pie( c(NumberOf(df$ringType, "cobwebby"),
       NumberOf(df$ringType, "evanescent"),
       NumberOf(df$ringType, "flaring"),
       NumberOf(df$ringType, "large"),
       NumberOf(df$ringType, "none"),
       NumberOf(df$ringType, "pendant"),
       NumberOf(df$ringType, "sheathing"),
       NumberOf(df$ringType, "zone")) , 
     ringType_variables, main = "ring-Type",
     col =rainbow(length(ringNumber_variables)))

# the following creates pie charts showing the proportions 
#of the variables for bruises, odor and spore print color
par(mfrow = c(1, 3))#allows multiple graphs to be displayed at 
#once to they can be seen and compared easier
pie( c(NumberOf(df$edible, TRUE),NumberOf(df$edible, FALSE)) ,
     c("true", "false"), main = "edibile or not",col = rainbow(2))
pie( c(NumberOf(df$population, "abundant"),
       NumberOf(df$population, "clustered"),
       NumberOf(df$population, "numerous"),
       NumberOf(df$population, "scattered"),
       NumberOf(df$population, "several"),
       NumberOf(df$population, "solitary")) , 
     population_variables, main = "population",
     col = rainbow(length(population_variables)))
pie( c(NumberOf(df$habitat, "grasses"),
       NumberOf(df$habitat, "leaves"),
       NumberOf(df$habitat, "meadows"),
       NumberOf(df$habitat, "paths"),
       NumberOf(df$habitat, "urban"),
       NumberOf(df$habitat, "waste"),
       NumberOf(df$habitat, "woods")) , 
     habitat_variables, main = "habitat",
     col=rainbow(length(habitat_variables)))
par(mfrow = c(1, 1)) 
  #makes it so that only one graph is one each screen for later plots
#one thing to notice from these charts is that there are some values that are
#incredibly one sided and should be investigated more
NumberOf(df$veilType, "universal") #checks how many "universal" types mushrooms 
                                   #there are
#as it turns out there are only partial type so this whole column can
#safely be removed with no consequence
df <- subset(df, select = -veilType )


######probability tables###########

table(df$capShape, df$edible,dnn = 
        c("capShape", "edible"))  
#creates table comparing the cap shape on whether it is edible 
prop.table(table(df$capShape, 
                 df$edible,
                 dnn = c("capShape", "edible")),1) 
#probability table of above
#most of the varaibles are not to far from 50/50 so there is unlikely to be
#much we can get from just these specific values
  
table(df$capSurface, df$edible,dnn = 
        c("cap-Surface", "edible"))  
#creates table comparing cap suface  
#with wiether they are edible or not 
prop.table(table(df$capSurface, 
                 df$edible,
                 dnn = c("cap-Surface", "edible")),1)#probability table of above
#similar to the table above there isn't much to show  a direct corelation 
#for these characteristics  other than  groves of which all are poisonous

table(df$capColor, 
      df$edible,
      dnn = c("capColor", "edible")) 
#creates table comparing if the mushroom is edible and with its cap colour
prop.table(table(df$capColor, 
                 df$edible,
                 dnn = c("capColor", "edible")),1) #probability table of above
#probability table of above
#this shows some more correlation such as pink and goldenrod4 so these might be
#more defining when doing later algorithms



#######Cross-Validation and the Bootstrap###########

set.seed(1)
train <- sample(length(df$edible), length(df$edible)*0.5)
lm.fit <- lm(df$edible ~ df$capColor , data = df , subset = train)
mean((df$edible - predict(lm.fit, df))[-train]^2)
#only 0.1825958 very low error

#the following checks with a diffrent random set to  make sure that 
#it is really that low
set.seed(2)
train <- sample(length(df$edible), length(df$edible)*0.5)
lm.fit <- lm(df$edible ~ df$capColor , data = df , subset = train)
mean((df$edible - predict(lm.fit, df))[-train]^2)
#only 0.1819106 which is very similar and still very low 
#however it would be better to use logistic regression as the data is entirely
#categorical


######glm functions########

ggplot(df, aes(y = edible, x = capColor)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  theme_classic(base_size = 15)  
#creates a jittered plot of cap colour and if the mushroom is poisonous
#the tables above indicated that there are more likely to be a correlation for
#capColors
capColor_edible.glm <- glm(edible ~ capColor, 
                               data = df, 
                               family = binomial)
summary(capColor_edible.glm)    
  #creates a binomial model of the same data as above
plogis(coef(capColor_edible.glm)[1]) #looks at the intercept 
plogis(coef(capColor_edible.glm)[2]) #looks at the slope
predicted <- predict(capColor_edible.glm, type = "response", se.fit = TRUE)
df$fit <- predicted$fit
df$se <- predicted$se.fit #creates predictions for what has been plotted above 
ggplot(df, aes( y = edible, x = capColor)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "blue") +
  theme_classic(base_size = 15) 
  #puts the plot on the same graph as above to compare it visually 
  #we can see that there is not a specific correlation between these two type
  #most likely because there are purely discrete data (although intetersting 
  #to note there are no goldenrod4 edible mushrooms)
df$se <- NULL
df$fit <- NULL  #removes the extra columns that where created since they are
                #no longer needed

edible_model_capColor <- glm(edible ~ capColor, data = df) 
  #fits a logistic regression edible with edible as our response variable and
  #and capColor as the explanatory variable
  #I chose these values since they seemed the most likely from above to give
  #us useful values
summary(edible_model_capColor) # creates a summary of above
  #the summary shows that all reduce the odds of edibility to varying degrees
  #except for red which slightly increases the odds of edibility

edible_model_all <- glm(edible ~ .,data = df,family = binomial) 
                          
                          #fits a logistic regression model with edible as our 
#response variable and all other variables as covariates
#we are unfortunately told that the algorithm didn't converge
summary(edible_model_all) #creates a summary of above
#this shows us the chances of each mushroom being edible for each feature
#compared to the first variable of each feature 

predict(edible_model_all,
        newdata = data.frame(capShape = "bell",
                             capSurface = "fibrous",
                             capColor = "brown",
                             bruises = TRUE,
                             odor = "almond",
                             gillAttachment = "attached",
                             gillSpacing = "close",
                             gillSize = "broad",
                             gillColor = "black",
                             stalkShape = "enlarging",
                             stalkRoot = "club",
                             stalkSurfaceAboveRing = "smooth",
                             stalkSurfaceBelowRing = "smooth",
                             stalkColorAboveRing = "brown",
                             stalkColorBelowRing = "brown",
                             veilColor = "white",
                             ringNumber = 0,
                             ringType = "evanescent",
                             sporePrintColor = "white",
                             population = "clustered",
                             habitat = "woods"),
        type = "response") 
# Compare the estimated probabilities of a mushroom with the above qualities
#it is near zero that this mushroom is edible according to this predictor
#it does give a warning which is most likely because some of the features have
#very little variability however this is just the nature of the data and 
#mushrooms themselves.
#This is very useful for when you want to check for a specific mushroom since 
#you could enter observed characteristics of the mushroom in the above algorthm
#and it would be able to tell you the probability of it being edible or not.


model_stalkSurfaceBelowRing <- glm(edible ~ stalkSurfaceBelowRing, 
                                   data = df, 
                                   family = binomial)
hnp(model_stalkSurfaceBelowRing) 
model_stalkColorBelowRing <- glm(edible ~ stalkColorBelowRing, 
                   data = df, 
                   family = binomial)
hnp(model_stalkColorBelowRing)

model_both <- glm(edible ~ stalkSurfaceBelowRing + stalkColorBelowRing, 
                  data = df, 
                  family = binomial)
hnp(model_both) 
#the vast majority of all three of these plots lie within the envelope 
#so they are likely a good fit




#####Leave-One-Out Cross-Validation##########

glm.fit <- glm(df$edible ~ df$capShape , data = df)
coef (glm.fit)
#the reference here is bell shaped and almost all are less likely to be edible
#than the reference

glm.fit <- glm(df$edible ~ df$capShape , data = df)
cv.err <- cv.glm (df , glm.fit)
cv.err$delta















######fitting trees#########

#through all the previous analysis I have decided that since almost all the
#data is categorical that decision trees will be a very good way of 
#classifying this data

#the classifications only work with factors so all mut be converted to them
df_class <- df
df_class$edible <- as.factor(df$edible)
df_class$capShape <- as.factor(df$capShape)
df_class$capSurface <- as.factor(df$capSurface)
df_class$capColor <- as.factor(df$capColor)
df_class$bruises <- as.factor(df$bruises)
df_class$odor <- as.factor(df$odor)
df_class$gillAttachment <- as.factor(df$gillAttachment)
df_class$gillSpacing <- as.factor(df$gillSpacing)
df_class$gillSize <- as.factor(df$gillSize)
df_class$gillColor <- as.factor(df$gillColor)
df_class$stalkShape <- as.factor(df$stalkShape)
df_class$stalkRoot <- as.factor(df$stalkRoot)
df_class$stalkSurfaceAboveRing <- as.factor(df$stalkSurfaceAboveRing)
df_class$stalkSurfaceBelowRing <- as.factor(df$stalkSurfaceBelowRing)
df_class$stalkColorAboveRing <- as.factor(df$stalkColorAboveRing)
df_class$stalkColorBelowRing <- as.factor(df$stalkColorBelowRing)
df_class$veilColor <- as.factor(df$veilColor)
df_class$ringNumber <- as.factor(df$ringNumber)
df_class$ringType <- as.factor(df$ringType)
df_class$sporePrintColor <- as.factor(df$sporePrintColor)
df_class$population <- as.factor(df$population)
df_class$habitat <- as.factor(df$habitat)

set.seed(45)
train <- sample(length(df$edible), length(df$edible)*0.5)

tree.df_class=tree(edible~. , data = df_class)
summary(tree.df_class)
plot(tree.df_class)
text(tree.df_class, pretty = 0)
tree.df_class
#this has created a decision tree to help identify edible mushrooms which is 
#exactly what I wanted, however I should do more checking to make sure it is 
#accurate

df_class.test = df_class[-train,]
edible.test=df_class$edible[-train]
tree.df_class=tree(edible~. , data = df_class,subset=train)
tree.pred=predict(tree.df_class,df_class.test,type = "class")
table(tree.pred,edible.test)
#this shows that the accuracy is 100%, there are no false positives or false
#negatives, initial this looks to be a good thing however this could very
#possibly be a result of over fitting

set.seed(3)
cv.df_class=cv.tree(tree.df_class,FUN=prune.misclass)
names(cv.df_class)

par(mfrow=c(1,2))
plot(cv.df_class$size,cv.df_class$dev,type="b")
plot(cv.df_class$k,cv.df_class$dev,type="b")
#the above graphs show that it is unlikely that pruning the tree will help at
#all as it is very accurate and compact as it is
par(mfrow=c(1,1))

prune.df_class=prune.misclass(tree.df_class,best=5)
plot(prune.df_class)
text(prune.df_class,pretty=0)
#the above shows that it the original number of branches is in fact correct

##########random forest###############

set.seed(1)
bag.df_class=randomForest(df_class$edible~., data=df_class, subset=train,
                          mtry=21, importance=TRUE)
bag.df_class
# the above creates random forests which shows very similar results to the
#trees above with no false posatives or false negatives

df_class.train = df_class[train,]
for (col in names(df_class.train)) {
  if (is.factor(df_class.train[[col]])) {
    df_class.train[[col]] <- factor(df_class.train[[col]], 
                                    levels = levels(df_class[[col]]))
  }
}

df_class.test = df_class[-train,] #creates the test set from the training set
bag.df_class = randomForest(edible ~ ., data = df_class.train)
yhat.bag = predict(bag.df_class, df_class.test)

varImpPlot(bag.df_class)
#this shows how important the odor is specifically for checking the edibility of
#the mushrooms in the data set with sporePrintColor and a few others coming
#just after it

##########classification forest###########

#splits data into training and testing
set.seed(150)    
split<-sample.split(df$edible, SplitRatio = 0.5)  
training_set<-subset(df_class,split==TRUE)       
test_set<-subset(df_class,split==FALSE)
topredict_set <-  dplyr::select(test_set,-edible)

#fits a decision tree
model_dtree<- rpart(edible ~ ., data=training_set)
preds_dtree<-predict(model_dtree,newdata=topredict_set,type="class")

conf_matrix_dtree <- table(preds_dtree, test_set$edible)
confusionMatrix(conf_matrix_dtree)
#this tree misclassifies 11 mushrooms as poisonous when they where really
#edible this is very accurate (and for someone who where to try to use this 
#algorithm its good that's its being safe)
#the Kappa value and accuracy show how well this model does, and from a quick
#observation doesn't look quite as susceptible to overfitting 

#creates forest
set.seed(1234)
model_rf<-randomForest(edible~.,data=training_set,importance=TRUE, ntree=1000) 
preds_rf <- predict(model_rf, topredict_set)              
(conf_matrix_forest <- table(preds_rf, test_set$edible))
confusionMatrix(conf_matrix_forest) 
#above creates a forest which has a 100% accuracy no there are no 
#misclassifcations unfortunately even though this looks like a good thing it
#it could be a result of over fitting, it could be that the data is very 
#reliant on a few data points as seen in some of the graphs above

train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(edible ~ ., data = df_class, 
                  method = "rf",
                  trControl = train_control)
print(cv_model)
#the above creates and compares different subsets to compare the model
#it shows that that appose to what I'd initially thought the data is unlikely
#to be over fitting instead there are just a few elements that can describe
#edibility of the mushrooms in the data set very well
#However there is a chance that this would be specific to the mushrooms within
#this data set and could not be used on others.


  