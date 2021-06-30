library(readr)
library(move)

Combined_collar_data <- read_delim("data/raw/Nkhotakota collar data/Combined_collar_data_csv.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Combined_collar_data$id <- paste0(Combined_collar_data$Elephant,"_",format(Combined_collar_data$Time, format="%Y"),"_",format(Combined_collar_data$Time, format="%m"))
data <- move(x=Combined_collar_data$dde, 
             y=Combined_collar_data$dds,
             time=as.POSIXct(Combined_collar_data$Time, format="%Y-%m-%d %H:%M:%S", tz="CAT"),
             proj=  CRS(SRS_string="OGC:CRS84"),
             # proj= CRS(SRS_string="EPSG:WGS84"),
             # proj=CRS("+proj=longlat +ellps=WGS84"),
             animal=Combined_collar_data$id
)
#head(X2018_7)
# coordinates(X2018_7) <- c("dde", "dds")
# proj4string(X2018_7) <- CRS("+proj=longlat +datum=WGS84 +units=km")
# data <- move(x=X2018_7$dde, 
#              y=X2018_7$dds,
#              time=as.POSIXct(X2018_7$datetime, format="%Y-%m-%d %H:%M:%S", tz="CAT"),
#              proj=  CRS(SRS_string="OGC:CRS84"),
#              # proj= CRS(SRS_string="EPSG:WGS84"),
#              # proj=CRS("+proj=longlat +ellps=WGS84"),
#              animal=X2018_7$elephant
#              )
#0.3233028
V1 = mean(unlist(speed(data))) #m/s
V2 = V1*60 #m/min
V3 = V2*60 #m/hr
V4 = V3*24 #m/day
V5 = V4*7 #m/min
(V = V1 *60*60*24*7) #m/wk

#(V = V1 *60*60*24*7*0.001)
# 
# 124 distances
# 
# library(magrittr)
# X2018_7 %>% 
#   subset(elephant=="01226908SKYF949")
# unique(X2018_7$elephant)
# distance(data)[1]
# speed(data)[1]
# timeLag(data, units= "mins")[1]
# 60*60*24*7
# 
# plot(data, type="b", pch=20)
# ?speed
# proj4string(data)
# str(speed(data))
