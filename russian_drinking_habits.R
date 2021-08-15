library(devtools)
install_github("vqv/ggbiplot")
library(purrr)
library(ggplot2)
library(tidyverse)
library(ggbiplot)
library(dplyr)

df <- read.csv("alcohol-consumption-in-russia_with_regions.csv")
#We added Federal District as the 8th columnn and Economic Region as the 9th column using information off the web.
df[1,10] = 0


normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df <- df %>%
  filter(region != "Chechen Republic")
df <- df %>%
  filter(region != "Republic of Crimea")
df <- df %>%
  filter(region != "Republic of Ingushetia")
df <- df %>%
  filter(region != "Sevastopol")







years = min(df$year) : max(df$year)
y = max(df$year) - min(df$year) + 1
Tot_Dist_c1 = numeric(81)
Tot_Dist_c = numeric(81)
Tot_Dist_Vodka = numeric(81)
Tot_Dist_Not_Vodka = numeric(81)



c1  = .95
c = 0.65

df_n = df #This is the normalized dataset.
df_n$wine       <- normalize(df_n$wine)
df_n$beer       <- normalize(df_n$beer)
df_n$vodka      <- normalize(df_n$vodka)
df_n$champagne  <- normalize(df_n$champagne)
df_n$brandy     <- normalize(df_n$brandy)






df <- arrange(df,desc(year))
df_n <- arrange(df_n,desc(year))


## Begin computing the distances from St. P to each region.

j = -1
for (y in years)
{
  j = j+1
  df_y = filter(df_n, year == y)
  x <- df_y[3:7] #This is the numerical values only for a given year across all regions.
  x_not_vodka = x[-3]
  x_vodka = x[3]
  sp = x[58,]  #Row 60 is St. P (or 58 after you remove the 4 regions with insufficient data).
  sp_not_vodka = x_not_vodka[58,]
  sp_vodka = x_vodka[58,]
  
  dist = 1:nrow(df_y) #This is an intialization (so the length doesn't grow in a for loop)
  dist_vodka = 1:nrow(df_y)
  dist_not_vodka = 1:nrow(df_y)
  for (i in 1:nrow(df_y))
  {
    dist[i] = rowSums((x[i,] - sp)^2)
    dist_vodka[i] = (x_vodka[i,1] -  sp_vodka)^2
    dist_not_vodka[i] = rowSums((x_not_vodka[i,] -  sp_not_vodka)^2)
  }
  
  
  
  
  
  
  
  a = j*(81) + 1 
  b = a + 80
  df[a:b,10] = dist
  df[a:b,11] = dist_vodka
  df[a:b,12] = dist_not_vodka
  
  
  
  Tot_Dist_c1 = Tot_Dist_c1 + (c1^(j)*dist)
  Tot_Dist_c = Tot_Dist_c + (c^(j)*dist)
  Tot_Dist_Vodka = Tot_Dist_Vodka + (c1^(j)*dist_vodka)
  Tot_Dist_Not_Vodka = Tot_Dist_Not_Vodka + (c1^(j)*dist_not_vodka)
  
  
  
}

Closest_To_SP = data.frame(Region            = df[1:81,2],   
                           Federal.District  = df[1:81,8],
                           Economic.District = df[1:81,9],
                           Distance_c1       = Tot_Dist_c1,
                           Distance_c = Tot_Dist_c,
                           Dist_Vodka = Tot_Dist_Vodka,
                           Dist_Not_Vodka = Tot_Dist_Not_Vodka
)
Closest_To_SP <- Closest_To_SP %>%
  arrange(Distance_c1)


distinct_regions <- unique(df$region)


df.pca <- prcomp(df_n[,(3:7)], center = TRUE,scale. = TRUE)
df.fed_dist <- df[,8]
df.eco_regn <- df[,9]

ggbiplot(
  df.pca,
  ellipse = TRUE,
  #labels = rownames(mtcars),
  groups = df.fed_dist
  #scale_colour_manual(name = "Federal District",
  #                  values = c("forest green", "red3", "dark blue", "pink", "yellow", "magenta", "purple", "grey"))
)


ggbiplot(
  df.pca,
  ellipse = TRUE,
  #labels = rownames(mtcars),
  groups = df.eco_regn
)
