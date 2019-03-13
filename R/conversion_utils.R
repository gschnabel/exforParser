#  exforParser - extract information from EXFOR entries 
#  Copyright (C) 2019  Georg Schnabel
#  
#  exforParser is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#  
#  exforParser is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>

parseReactionStr <- function(entry) {
  
  reacStr <- entry$BIB$REACTION
  if (!isTRUE(is.character(reacStr))) return(NULL)
  
  partRegex <- "([[:digit:]]{1,2}-[A-Z]{1,2}-[[:digit:]]{1,3}|N|P|D|T|HE3|A)"
  regex <- paste0("\\(",partRegex,"\\(",partRegex,",")
  
  res <- regexpr(regex,reacStr,perl=TRUE)
  if (isTRUE(res==-1)) return(NULL)
  part <- with(attributes(res), {
    substring(reacStr,capture.start,capture.start+capture.length-1)
  })
  # some conversions
  part[part=="N"] <- "n"
  part[part=="1-H-1" | part=="P"] <- "p"
  part[part=="1-H-2" | part=="D"] <- "d"
  part[part=="1-H-3" | part=="T"] <- "t"
  part[part=="2-HE-4" | part=="A"] <- "a"
  part[part=="HE3"] <- "2-HE-3"
  # extract mass, element and charge
  partProp <- strsplit(part,"-",fixed=TRUE)
  partProp <- lapply(partProp,function(x) {
    if (length(x)==1) {
      switch(x, n=c(0,x,1), p=c(1,x,1), d=c(1,x,2), 
             t=c(1,x,3), a=c(2,x,4), default=NULL)
    } else x
  })
  
  projSym <- substring(partProp[[2]][2],1:2,c(1,100))
  projSym <- paste0(projSym[1],tolower(projSym[2]))
  projA <- as.integer(partProp[[2]][3])
  if (projSym %in% c("n","p","d","t","a")) {
    projInclSym <- projSym
  } else {
    projInclSym <- paste0(projSym,if(projA==0) "" else projA)
  }
  
  tarSym <- substring(partProp[[1]][2],1:2,c(1,100))
  tarSym <- paste0(tarSym[1],tolower(tarSym[2]))
  tarA <- as.integer(partProp[[1]][3])
  if (tarSym %in% c("n","p","d","t","a")) {
    tarInclSym <- tarSym
  } else {
    tarInclSym <- paste0(tarSym,if(tarA==0) "" else tarA)
  }
  
  list(projectile=list(str=part[2],
                       Sym=projSym,
                       Z=as.integer(partProp[[2]][1]),
                       A=as.integer(partProp[[2]][3]),
                       inclSym=projInclSym),
       target=list(str=part[1],
                   Sym=tarSym,
                   Z=as.integer(partProp[[1]][1]),
                   A=tarA,
                   inclSym=tarInclSym)
  )
}

unitConverter <- function(data) {
  
  if (is.null(data$UNIT) || is.null(data$TABLE)) return(data)
  
  # B
  regex <- "(^|\\*)B"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 1000
  # /B
  regex <- "/B"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 1000
  # microB (MUB)
  regex <- "(^|\\*)MUB"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 1000
  # /microB (MUB)
  regex <- "/MUB"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 1000
  # microB
  regex <- "(^|\\*)MICRO-B"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 1000
  # /microB
  regex <- "/MICRO-B"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MB",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 1000
  # KEV
  regex <- "(^|\\*)KEV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 1000
  # /KEV
  regex <- "/KEV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 1000
  # EV
  regex <- "(^|\\*)EV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 10^6   
  # /EV
  regex <- "/EV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 10^6   
  # GEV
  regex <- "(^|\\*)GEV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"\\1MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] * 10^3   
  # /GEV
  regex <- "/GEV"
  hasUnit <- grepl(regex,data$UNIT)
  data$UNIT <- sub(regex,"/MEV",data$UNIT)
  data$TABLE[,hasUnit] <- data$TABLE[,hasUnit] / 10^3   
  
  data
}


