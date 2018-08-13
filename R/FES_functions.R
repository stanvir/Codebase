 
#this file will have the functions needed to work with FES
#Calcualtion
# now we are going to define some of our workhorse functions
SumCalc <- function(Rawdat,Variable){
        metric <-lapply(Rawdat,function(.indx){
                sum(as.numeric(as.character(Variable[.indx])),na.rm = T)
        })
        metric <- data.frame(matrix(unlist(metric)),stringsAsFactors= F)
}

AvgCalc <- function(Rawdat,Variable){
        metric <-lapply(Rawdat,function(.indx){
                mean(as.numeric(as.character(Variable[.indx])),na.rm = T)
        })
        metric <- data.frame(matrix(unlist(metric)),stringsAsFactors= F)
}

FirstCalc <- function(Rawdat,Variable){
        metric <-lapply(Rawdat,function(.indx){
                head(Variable[.indx], n=1)
        })
        metric <- data.frame(matrix(unlist(metric)),stringsAsFactors= F)
}

LastCalc <- function(Rawdat,Variable){
        metric <-lapply(Rawdat,function(.indx){
                tail(Variable[.indx], n=1)
        })
        metric <- data.frame(matrix(unlist(metric)),stringsAsFactors= F)
}

LengthCalc <- function(Rawdat,Variable){
        metric <-lapply(Rawdat,function(.indx){
                length(Variable[.indx])
        })
        metric <- data.frame(matrix(unlist(metric)),stringsAsFactors= F)
}

numconvert <- function (x){
        as.numeric(as.character(x))
}

CalculateVSP2 <- function(speed, acc, slope){
        ##################################################
        ########## VSP Calculation #####################
        
        #VSP (kW/ton) = v 1.1a + 9.81 a tan sin grade + 0.132 + 0.000302v3
        #Output unit is KW per tonne/ same as W per Kg
        VSP_myway <- speed*0.44704*(1.1* acc*0.44704+
                                            9.81* (sin(atan(slope)))+0.132)+
                0.000302*(speed*0.44704)^3
}

CalculateVSP <- function(speed, acceleration){
        ##################################################
        ########## VSP Calculation #####################
        #coefficients- A, B, C, m resprectively
        coef <- c(0.156461, 0.00200193, 0.000492646, 1.4788)
        
        #VSP Equation VSP= (A?Vt+B?Vt2+C?Vt3+m?Vt?at)/m
        #Output unit is KW per tonne/ same as W per Kg
        VSP_myway <- (coef[1]*speed*0.44704 +
                              coef[2]*(speed*0.44704)^2 +
                              coef[3]*(speed*0.44704)^3 +        
                              coef[4]*speed*(0.44704)^2 * acceleration)/coef[4]
}

FES_calc2 <- function (MPG, ui, li){
        if(MPG > ui) {FES <- 100}
        else if (MPG < li) {FES <- 20}
        else {FES <- 20+ (MPG - li)/(ui- li)*80}
        return(FES)
}

i2d_read <- function(semicolon = F){
  if (semicolon == T){
    rawdat <- read.csv2(file.choose(), header = T, na.strings=c(""," ","NA"))
  } else{
    rawdat <- read.csv(file.choose(), header = T, na.strings=c(""," ","NA"))
  }
  
  #only keeps the complete rows
  rawdat <-  rawdat[complete.cases(rawdat[,3]),]
  
  #add a case so we can integrate column names from
  #load("data/columnnames.Rda")
  
  #initiates acceleration
  rawdat$Acceleration <- rep(NA, length(rawdat[,1]))
  
  rawdat$Speed.mph <- numconvert(rawdat$Speed..mph.)
  rawdat$Slope..m.m. <- numconvert(rawdat$Slope..m.m.)
  rawdat$TRIP_ID <- as.character(rawdat$TRIP_ID)
  
  
  tripidList <- unique(rawdat$TRIP_ID)
  
  
  coefficient <- NULL
}

science_theme <-  theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
                        axis.line = element_line(size = 0.7, color = "black"),
                        legend.position = c(0.9, 0.70), text = element_text(size = 14))