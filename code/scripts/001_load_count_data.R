library(readxl)
library(tidyverse)
elephant_groups_cleaned <- read_excel("data/raw/Nkhotakota collar data/elephant_groups_cleaned_062121.xlsx", 
                                      sheet = "elephant_groups_cleaned_062121")
elephant_groups_cleaned$`Group size cumulative` <- as.numeric(elephant_groups_cleaned$`Group size cumulative`)

# Load original DF with sampling zeros
y <- read_excel("data/raw/dh_Elephant.xlsx", 
                sheet = "clean_station_count",
                col_types = c("text", 
                              rep("numeric",99)))

#group by site and week strata and split into a list of dfs
g_elephant_groups_cleaned <-  elephant_groups_cleaned %>% 
  group_by(site,WK) %>%  
  split(group_indices(.))

# i <- 3
# j <- 2

# empty storage df 
CH <- data.frame(site = rep(NA,length(g_elephant_groups_cleaned)), week = rep(NA,length(g_elephant_groups_cleaned)) , count = rep(NA,length(g_elephant_groups_cleaned)))

delta <- 60 #thinning interval
for(i in 1:length(g_elephant_groups_cleaned)){#length(g_elephant_groups_cleaned)
  CH$site[i] <- g_elephant_groups_cleaned[[i]]$site[[1]]
  CH$week[i] <- g_elephant_groups_cleaned[[i]]$WK[[1]]
  CH$count[i] <- ifelse(!is.na(g_elephant_groups_cleaned[[i]]$`Group size cumulative`[[1]]),g_elephant_groups_cleaned[[i]]$`Group size cumulative`[[1]],1)
  
  cluster_num <- 1
  g_elephant_groups_cleaned[[i]]$cluster_num <- cluster_num
  
  if(nrow(g_elephant_groups_cleaned[[i]]) > 1) {
    for(j in 2:nrow(g_elephant_groups_cleaned[[i]])){
      if(as.numeric((g_elephant_groups_cleaned[[i]]$actualDate[j] - g_elephant_groups_cleaned[[i]]$actualDate[j-1]), units = "mins") > delta){
        cluster_num <- cluster_num + 1
        g_elephant_groups_cleaned[[i]]$cluster_num[j:nrow(g_elephant_groups_cleaned[[i]])] <- cluster_num
        if(!is.na(g_elephant_groups_cleaned[[i]]$`Group size cumulative`[[j]])) CH$count[i] <- CH$count[i] + g_elephant_groups_cleaned[[i]]$`Group size cumulative`[[j]]
      }
    }
    
  }
}

# Pull the list into a common df
rbound_elephant_groups_cleaned <- do.call("rbind",g_elephant_groups_cleaned)

# build df to get mean group sizes after detecting temporal clusters at 1 hr
mean_cluster_size <- rbound_elephant_groups_cleaned %>% 
  group_by(site,WK,cluster_num) %>% 
  summarise(mean=mean(`Group size cumulative`,na.rm=TRUE))

# Corrected count values clustered at 1 hr intervals used to replace duplicated counts in the original data
y_corrected <- CH %>% pivot_wider( names_from = week,values_from = count)
names(y_corrected) <- c("site",paste0("w_",names(y_corrected[-1]))) #correct col names for string searches

# Replace duplicate values in y
for(i in 1:nrow(y_corrected)){
  row <- which(y$site == y_corrected$site[i])
  for(j in 2:ncol(y_corrected)){
    if(!is.na(y_corrected[i,j]) && y_corrected[i,j]>0)y[row,which(names(y) == names(y_corrected)[j])] <- y_corrected[i,j]
  }
}
