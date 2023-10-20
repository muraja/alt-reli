# This script renames Finnish labels of the data in English


# renaming, with identifiers ".A" for Allardt's Life Satisfaction scale and ".G" De Jong Gierveld's Loneliness Scale.
SCALES <- yksdata %>% rename(
  "ID" = "psykid",
  "interest.A" = "kiinnos30",
  "happy.A" = "onnel30", 
  "easy.A" = "helppo30",   
  "lonely.A" = "yksin30",  
  "empty.G" = "olotyhjä30", 
  "rely.G" = "tukeut30",
  "trust.G" = "luotihm30",  
  "close.G" = "lähihm30",   
  "around.G" = "kaipihm30",  
  "reject.G" = "torjut30"
)

# view names
SCALES %>% names


### A ###
# check attributes
SCALES$interest.A  %>% attributes 
# rename label
attr(SCALES$interest.A, "label") <- "Interesting life"
# rename labels
attr(SCALES$interest.A, "labels") <- setNames(
  c(1:5,9,12,13,23,25,45), # list values
  c("very interesting", "quite interesting", "quite boring", "very boring", "cannot say","question mark; cannot say","circled 1 & 2","circled 1 & 3","circled 2 & 3","circled 2 & 5","circled 4 & 5") # list corresponding labels
)

SCALES$happy.A  %>% attributes 
attr(SCALES$happy.A, "label") <- "Happiness"
attr(SCALES$happy.A, "labels") <- setNames(
  c(1:5,9,12,23,25,35), 
  c("very happy", "quite happy", "quite unhappy", "very unhappy", "cannot say","question mark; cannot say","circled 1 & 2", "circled 2 & 3","circled 2 & 5","circled 3 & 5") 
)

SCALES$easy.A %>% attributes 
attr(SCALES$easy.A, "label") <- "Ease of life"
attr(SCALES$easy.A, "labels") <- setNames(
  c(1:5,9,23,25), 
  c("very easy", "quite easy", "quite hard", "very hard", "cannot say","question mark; cannot say", "circled 2 & 3","circled 2 & 5") 
)

SCALES$lonely.A %>% attributes 
attr(SCALES$lonely.A, "label") <- "Feeling lonely"
attr(SCALES$lonely.A, "labels") <- setNames(
  c(1:4,9,13,23,24,34), 
  c("very lonely", "somewhat lonely", "not lonely at all", "cannot say","question mark; cannot say","circled 1 & 3", "circled 2 & 3","circled 2 & 4","circled 3 & 4") 
)

### G ### I used item labels from Kelly et al 2023
SCALES$empty.G %>% attributes 
attr(SCALES$empty.G, "label") <- "General sense of emptiness"
attr(SCALES$empty.G, "labels") <- setNames(
  c(1:5,9,23,34,123), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 2 & 3","circled 3 & 4","circled 1, 2 & 3") 
)

SCALES$rely.G %>% attributes 
attr(SCALES$rely.G, "label") <- "People to rely on"
attr(SCALES$rely.G, "labels") <- setNames(
  c(1:5,9,12, 15, 23, 24, 25, 34), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 1 & 2", "circled 1 & 5", "circled 2 & 3", "circled 2 & 4", "circled 2 & 5", "circled 3 & 4") 
)

SCALES$trust.G %>% attributes 
attr(SCALES$trust.G, "label") <- "Trusts many people"
attr(SCALES$trust.G, "labels") <- setNames(
  c(1:5,9,12,14,15,23,25,34,345), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 1 & 2", "circled 1 & 4", "circled 1 & 5", "circled 2 & 3", "circled 2 & 5", "circled 3 & 4","circled 3, 4 & 5") 
)

SCALES$close.G %>% attributes 
attr(SCALES$close.G, "label") <- "Feels close to people"
attr(SCALES$close.G, "labels") <- setNames(
  c(1:5,9,12,13,14,15,23,24,25,34,45,234), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 1 & 2", "circled 1 & 3", "circled 1 & 4", "circled 1 & 5", "circled 2 & 3", "circled 2 & 4", "circled 2 & 5", "circled 3 & 4","circled 4 & 5", "circled 2, 3 & 4") 
)

SCALES$around.G %>% attributes 
attr(SCALES$around.G, "label") <- "Misses having people around"
attr(SCALES$around.G, "labels") <- setNames(
  c(1:5,9,12,14,15,23,24,25,34,35,45), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 1 & 2", "circled 1 & 4", "circled 1 & 5", "circled 2 & 3", "circled 2 & 4", "circled 2 & 5", "circled 3 & 4", "circled 3 & 5", "circled 4 & 5") 
)

SCALES$reject.G %>% attributes 
attr(SCALES$reject.G, "label") <- "Often feels rejected"
attr(SCALES$reject.G, "labels") <- setNames(
  c(1:5,9,24,34,45), 
  c("absolutely yes", "yes", "sometimes", "no", "absolutely no","question mark; cannot say", "circled 2 & 3","circled 3 & 4","circled 4 & 5") 
)
# view labels
library(finalfit); library(haven)
ff_glimpse(SCALES)$Continuous[] %>% select(label)