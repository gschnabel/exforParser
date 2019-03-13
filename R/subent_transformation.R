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

transformSubent <- function(firstSub,curSub) {

  # merge BIB from first entry with BIB from data entry
  nameCollisionIndex <- match(names(curSub$BIB), names(firstSub$BIB))
  nameCollisionIndex <- nameCollisionIndex[!is.na(nameCollisionIndex)]
  if (length(nameCollisionIndex)>0) {
    workStr <- names(firstSub$BIB)[nameCollisionIndex]
    names(firstSub$BIB)[nameCollisionIndex] <- paste0(workStr, "_firstSub")
  }
  
  curSub$BIB <- c(firstSub$BIB, curSub$BIB)
  stopifnot(anyDuplicated(names(curSub$BIB))==0)
  
  # add info from COMMON blocks to data tables
  firstSubCommon <- firstSub$COMMON

  combinedCommon <- list(UNIT = character(0),
                         DESCR = character(0),
                         TABLE = NULL)

  # convert units and save columns of tables under keys
  if (!is.null(firstSubCommon)) {
    combinedCommon$UNIT <- c(combinedCommon$UNIT, firstSubCommon$UNIT)
    combinedCommon$DESCR <- c(combinedCommon$DESCR, firstSubCommon$DESCR)
    combinedCommon$TABLE <- cbind(combinedCommon$TABLE, firstSubCommon$TABLE)
  }
  if (!is.null(curSub$COMMON)) {
    combinedCommon$UNIT <- c(combinedCommon$UNIT, curSub$COMMON$UNIT)
    combinedCommon$DESCR <- c(combinedCommon$DESCR, curSub$COMMON$DESCR)
    combinedCommon$TABLE <- cbind(combinedCommon$TABLE, curSub$COMMON$TABLE)
    curSub$COMMON <- unitConverter(curSub$COMMON)
  }
  if (!is.null(curSub$DATA)) {
    curSub$DATA <- unitConverter(curSub$DATA)
    curSub$DATA$TABLE <- as.data.frame(curSub$DATA$TABLE,stringsAsFactors=FALSE)
    names(curSub$DATA$TABLE) <- curSub$DATA$DESCR
  }

  # bomb out if lengths not equal
  with(combinedCommon,stopifnot(length(UNIT)==length(DESCR),length(DESCR)==ncol(TABLE)))

  # add common block if any present
  if (!is.null(combinedCommon$TABLE)) {
    combinedCommon <- unitConverter(combinedCommon)
  }

  # bind the common values to the data
  if (!is.null(curSub$DATA) && !is.null(combinedCommon$TABLE)) {
    curSub$DATA$UNIT <- c(combinedCommon$UNIT,curSub$DATA$UNIT)
    curSub$DATA$DESCR <- c(combinedCommon$DESCR,curSub$DATA$DESCR)
    curSub$DATA$TABLE <- cbind(combinedCommon$TABLE,curSub$DATA$TABLE)
    names(curSub$DATA$TABLE) <- curSub$DATA$DESCR
  }

  # convert the units to a list
  if (!is.null(curSub$DATA)) {
    names(curSub$DATA$UNIT) <- curSub$DATA$DESCR
    curSub$DATA$UNIT <- as.list(curSub$DATA$UNIT)
  }

  # add identifiers
  curSub$ACCNUM <- substring(curSub$ID,1,5)
  curSub$SUBACCNUM <- substring(curSub$ID,6,8)

  # add meta information
  curSub$META <- list()

  tryRes <- try({
    reacInfo <- parseReactionStr(curSub)
    if (!is.null(reacInfo)) {
      curSub$META$PROJECTILE <- with(reacInfo$projectile,
                                     list(SYM = inclSym,
                                          A = A, Z = Z))
      curSub$META$TARGET <- with(reacInfo$target,
                                 list(SYM = inclSym,
                                      A = A, Z = Z))
      if (reacInfo$target$A == 0) {
        curSub$META$TARGET$A <- inclSymbolToAZ(reacInfo$target$inclSym)$A
        curSub$META$TARGET$nat <- 1
      }
    }
  })
  if (class(tryRes)=="try-error") {
    print(curSub$BIB$REACTION)
  }

  # remove meta if no information
  if (length(curSub$META)==0) curSub$META <- NULL

  curSub
}


