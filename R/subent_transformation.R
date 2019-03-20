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


inclSymbolToAZ <- function(x) {
  Zmap <- c(H=1,He=2,Li=3,Be=4,B=5,C=6,N=7,O=8,F=9,Ne=10,
            Na=11,Mg=12,Al=13,Si=14,P=15,S=16,Cl=17,Ar=18,
            K=19,Ca=20,Sc=21,Ti=22,V=23,Cr=24,Mn=25,Fe=26,Co=27,Ni=28,Cu=29,
            Zn=30,Ga=31,Ge=32,As=33,Se=34,Br=35,Kr=36,
            Rb=37,Sr=38,Y=39,Zr=40,Nb=41,Mo=42,Tc=43,Ru=44,Rh=45,Pd=46,Ag=47,
            Cd=48,In=49,Sn=50,Sb=51,Te=52,I=53,Xe=54,
            Cs=55,Ba=56,La=57,Ce=58,Pr=59,Nd=60,Pm=61,Sm=62,Eu=63,Gd=64,
            Tb=65,Dy=66,Ho=67,Er=68,Tm=69,Yb=70,Lu=71,
            Hf=72,Ta=73,W=74,Re=75,Os=76,Ir=77,Pt=78,Au=79,Hg=80,
            Tl=81,Pb=82,Bi=83,Po=84,At=85,Rn=86,
            Fr=87,Ra=88,Ac=89,Th=90,Pa=91,U=92,Np=93,Pu=94,Am=95,Cm=96,
            Bk=97,Cf=98,Es=99,Fm=100,Md=101,No=102,Lr=103,
            Rf=104,Db=105,Sg=106,Bh=107,Hs=108,Mt=109,Ds=110,Rg=111,
            Cn=112,Nh=113,Fl=114,Mc=115,Lv=116,Ts=117,Og=118)
  Amap <- c(H=1,He=4,Li=7,Be=9,B=11,C=12,N=14,O=16,F=19,Ne=20,
            Na=23,Mg=24,Al=27,Si=28,P=31,S=32,Cl=35,Ar=40,
            K=39,Ca=40,Sc=45,Ti=48,V=51,Cr=52,Mn=55,Fe=56,Co=59,
            Ni=59,Cu=64,Zn=65,Ga=70,Ge=73,As=75,
            Se=79,Br=80,Kr=84,Rb=85,Sr=88,Y=89,Zr=91,Nb=93,Mo=96,
            Tc=98,Ru=101,Rh=101,Pd=106,Ag=108,
            Cd=112,In=115,Sn=119,Sb=122,Te=128,I=127,Xe=131,Cs=133,
            Ba=137,La=139,Ce=140,Pr=141,Nd=144,Pm=145,Sm=150,Eu=152,
            Gd=157,Tb=159,Dy=163,Ho=165,Er=167,Tm=169,Yb=173,Lu=174,Hf=178,
            Ta=181,W=184,Re=186,Os=190,Ir=192,Pt=195,Au=197,Hg=201,
            Tl=204,Pb=207,Bi=208,Po=209,At=210,Rn=222,Fr=223,Ra=226,
            Ac=227,Th=232,Pa=231,U=238,Np=237,Pu=244,Am=243,Cm=247,
            Bk=247,Cf=251,Es=252,Fm=257,Md=258,No=259,Lr=262,Rf=261,
            Db=262,Sg=263,Bh=262,Hs=265,Mt=266,Ds=269,Rg=272)
  regres <- regmatches(x,regexec("([A-Z][a-z]*)([[:digit:]]*)",x))
  Zstr <- sapply(regres,function(x) { Zmap[x[2]] })
  Astr <- sapply(regres,function(x) { if (x[3]=="") Amap[x[2]] else x[3] })
  data.table(A=as.integer(Astr),Z=as.integer(Zstr),Sym=x)
}
