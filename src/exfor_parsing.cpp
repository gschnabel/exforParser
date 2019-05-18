//  exforParser - extract information from EXFOR entries 
//  Copyright (C) 2019  Georg Schnabel
//  
//  exforParser is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  exforParser is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>

#include <Rcpp.h>
#include <sstream>
#include <iostream>
#include <cmath>
#include <iomanip>

using namespace Rcpp;

std::string ttostr(double d)
{
  char buffer[50];
  int n;
  n = sprintf(buffer,"%.10e",d);
  return std::string(buffer);
}

std::string ttostr(int i) {
  char buffer[50];
  int n;
  n = sprintf(buffer,"%d",i);
  return std::string(buffer);
}


void debugOut(std::string msg) {
  // std::cout << msg << std::endl;
}


std::string trim(std::string line) {
  int p1, p2;
  std::string whitespaces(" \\t\\f\\v\\n\\r");
  p1 = line.find_first_not_of(whitespaces);
  p2 = line.find_first_of(whitespaces,p1);
  if (p1 == std::string::npos) return "";
  if (p2 == std::string::npos) p2=line.size();
  return line.substr(p1,p2-p1);
}

std::string getField(std::string &line,int pos) {
  if (line.size()==0) return "";
  return line.substr((pos-1)*11,11);
}

bool existField(std::string &line, int pos) {
  if (line.size()-1 < (pos-1)*11)
    return false;
  std::string fieldStr = trim(getField(line,pos));
  if (fieldStr.size()==0)
    return false;
  return true;
}

std::string getFirstField(std::string &line) {
  
  std::string fieldName = getField(line,1);
  return trim(fieldName);
}

std::string getAfterFirst(std::string &line) {
  if (line.size()<=11) return "";
  return line.substr(11,std::string::npos);
}

int getIntField(std::string &line, int pos) {
  std::string field = getField(line, pos);
  return atoi(field.c_str());
}

double getDoubleField(std::string &line, int pos) {
  
  std::string field = getField(line, pos);
  std::string subField;
  int i = 0, len = field.length();
  int si = -1, ei = -1;
  int numCnt = 0;
  double finalNumber, curNum;
  
  for (i=0; i<len; i++) {
    if (field.at(i) != ' ' && si == -1) si = i;
    if ((field.at(i) == ' ' || i==(len-1)) && si != -1)
    {
      if (i==(len-1)) ei = i; else ei = i-1;
      subField = field.substr(si, ei+1-si);
      curNum = atof(subField.c_str());
      if (numCnt == 0)
        finalNumber = curNum;
      else
        finalNumber *= pow(10, curNum);
      si = -1; ei = -1; numCnt++;
    }
  } 
  
  return finalNumber;
}

void skipElement(std::string &line, std::istringstream &f) {
  
  getline(f,line);
  while(!f.eof() && getFirstField(line).size()==0) 
    getline(f,line);
}

Rcpp::CharacterVector parseGeneric(std::string &line, std::istringstream &f) {
  
  bool firstLine = true;
  CharacterVector resVec;
  while (!f.eof()) {
    //std::cout << "--- " << getFirstField(line) << std::endl;
    if (!firstLine && getFirstField(line).size()>0) {
      break;
    }
    firstLine = false;
    resVec.push_back(getAfterFirst(line));
    getline(f,line);
  }
  return resVec;
}

Rcpp::CharacterVector parseGenericNLB(std::string &line, std::istringstream &f) {
  
  CharacterVector origVec = parseGeneric(line,f);
  std::string combRes("");
  for (int i=0; i<origVec.size(); i++)
    combRes += " " + origVec[i];
  combRes.erase(0,1);
  return(combRes);
}

Rcpp::CharacterVector parseBIBfield(std::string &line, std::istringstream &f) {

  std::string firstField = getFirstField(line);
  std::string baseFirstField = firstField;
  if (firstField.size() == 0)
    throw std::invalid_argument("Expected non-empty first field in line: " + line);
  

  CharacterVector origVec = parseGenericNLB(line,f);
  CharacterVector combRes;
  combRes.push_back(Rcpp::as<std::string>(origVec));

  
  do {
    firstField = getField(line,1);
    // NOCOMMON block after REAC has only 8 characters
    if (firstField.length()<11) break; 
    // otherwise we merge new fields also having an index into the previous one 
    if (trim(firstField.substr(0,10)).size() > 0) break;

    if (firstField.at(10) >= '0' && firstField.at(10) <= '9') {
      combRes.push_back(Rcpp::as<std::string>(parseGenericNLB(line,f)));
    } else break;
  } while (!f.eof());

  return(combRes);
}


Rcpp::CharacterVector parseBracketList(std::string &line, std::istringstream &f) {
  
  Rcpp::CharacterVector resVec;
  std::string rawString = Rcpp::as<std::string>(parseGenericNLB(line,f));
  int p1 = rawString.find_first_of("(");
  int p2 = rawString.find_first_of(")");
  if (p1 == std::string::npos || p2 == std::string::npos)
    throw std::invalid_argument("author list must be in brackets");
  rawString = rawString.substr(p1+1,p2-p1-1);
  
  std::istringstream ss(rawString);
  std::string item;
  while (std::getline(ss,item,',')) {
    resVec.push_back(trim(item));
  }
  return resVec;
}

Rcpp::List parseBib(std::string &line, std::istringstream &f) {
  
  Rcpp::List resList;
  
  std::string firstField = getFirstField(line);
  if (firstField.compare("BIB")!=0)
    throw std::invalid_argument("BIB handler cannot parse this block starting with: " + line);
  
  int numElements = getIntField(line,2);
  int numRows = getIntField(line,3);
  getline(f,line);
  //std::cout << "first field " << firstField << std::endl;
  while(!f.eof()) {
    firstField = getFirstField(line);
    if (firstField.compare("ENDBIB")!=0)
      resList[firstField] = parseBIBfield(line,f);
    else {
      getline(f,line);
      break;
    } 
  }
  
  return resList;
}

Rcpp::List parseDataAndCommon(std::string &line, std::istringstream &f) {
  
  debugOut("enter parseAndCommon");
  
  Rcpp::List resList;
  Rcpp::CharacterVector fieldDescr;
  Rcpp::CharacterVector fieldUnits;
  
  int i,j,elCount;
  int numCols=0, numRows=0;
  std::string headerLine = line;
  debugOut("first line: " + headerLine);
  
  std::string firstField = getFirstField(line);
  if (firstField.compare("DATA")!=0 && firstField.compare("COMMON")!=0)
    throw std::invalid_argument("DataAndCommon function cannot parse this block starting with: " + line);
  
  numCols = getIntField(line,2);
  if (firstField.compare("DATA")==0) {
    numRows = getIntField(line,3);
  } else {
    numRows = 1;
  }
  
  debugOut("read description");
  std::string curField;
  std::string trimmedField;
  getline(f,line);
  for (i=1,elCount=1; i<=numCols; i++, elCount++) {
    debugOut("i: " + ttostr(i) + " - elCount: " + ttostr(elCount));
    curField = getField(line,elCount);
    trimmedField = trim(curField);
    if (curField.size()==11 && curField.at(10) != ' ') {
      trimmedField += '-';
      trimmedField += curField.at(10);
    }
    fieldDescr.push_back(trimmedField);
    if (elCount == 6 && i<numCols) {
      getline(f,line);
      elCount = 0;
    }
  }
  
  debugOut("read units");
  getline(f,line);
  for (i=1,elCount=1; i<=numCols; i++, elCount++) {
    debugOut("i: " + ttostr(i) + " - elCount: " + ttostr(elCount));
    fieldUnits.push_back(trim(getField(line,elCount)));
    if (elCount == 6 && i<numCols) {
      getline(f,line);
      elCount = 0;
    }
  }
  
  debugOut("read table");
  Rcpp::NumericMatrix fieldValues(numRows,numCols);
  for (i=1; i<=numRows; i++) {
    getline(f,line);
    debugOut("curline: " + line);
    for (j=1,elCount=1; j<=numCols; j++, elCount++) {
      debugOut("i: " + ttostr(i) + " - j: " + ttostr(j));
      if (existField(line,elCount)) {
        fieldValues(i-1,j-1) = getDoubleField(line,elCount);
      } else {
        fieldValues(i-1,j-1) = NA_REAL;
      }
      if (elCount == 6 && j<numCols) {
        getline(f,line);
        elCount = 0;
      }
    }
  }
  
  getline(f,line); // skip ENDBLOCK statement
  firstField = getFirstField(line);
  if (firstField.compare("ENDCOMMON")!=0 &&
      firstField.compare("ENDDATA")!= 0)
    throw std::invalid_argument
    ("something went wrong with parsing block starting with " + headerLine);
  
  getline(f,line); // provide next line to to be parsed  
  
  resList["DESCR"] = fieldDescr;
  resList["UNIT"] = fieldUnits;
  resList["TABLE"] = fieldValues;
  
  debugOut("leave parseAndCommon");
  return resList;
}

Rcpp::List parseData(std::string &line, std::istringstream &f) {
  return parseDataAndCommon(line,f);
}

Rcpp::List parseCommon(std::string &line, std::istringstream &f) {
  return parseDataAndCommon(line,f);
}

Rcpp::List parseSubent(std::string &line, std::istringstream &f) {
  
  Rcpp::List resList;
  std::string subentId;
  std::string dateStr;
  
  std::string firstField;
  while (!f.eof()) {
    firstField = getFirstField(line);
  
    if (firstField.compare("SUBENT")==0) {
      subentId = trim(getField(line,2));
      dateStr = trim(getField(line,3));
      resList["ID"] = subentId;
      resList["DATEMOD"] = dateStr;
      std::getline(f,line);
    } else if (firstField.compare("BIB")==0)
      resList[firstField] = parseBib(line,f);
    else if (firstField.compare("DATA")==0)
      resList[firstField] = parseData(line,f);
    else if (firstField.compare("COMMON")==0)
      resList[firstField] = parseCommon(line,f);
    else if (firstField.compare("NOCOMMON")==0)
      skipElement(line,f);
    else if (firstField.compare("NODATA")==0)
      skipElement(line,f);
    else if (firstField.compare("ENDSUBENT") != 0)
      resList[firstField] = parseGeneric(line,f);
    else if (firstField.compare("ENDSUBENT")==0) {
      std::getline(f,line);
      break;
    }
    else
      getline(f,line);
  }
  return resList;
}



std::string escape_json(const std::string &s) {
  std::ostringstream o;
  for (std::string::const_iterator c = s.begin(); c != s.end(); c++) {
    switch (*c) {
    case '"': o << "\\\""; break;
    case '\\': o << "\\\\"; break;
    case '\b': o << "\\b"; break;
    case '\f': o << "\\f"; break;
    case '\n': o << "\\n"; break;
    case '\r': o << "\\r"; break;
    case '\t': o << "\\t"; break;
    default:
      if ('\x00' <= *c && *c <= '\x1f') {
        o << "\\u"
          << std::hex << std::setw(4) << std::setfill('0') << (int)*c;
      } else {
        o << *c;
      }
    }
  }
  return o.str();
}

template <typename GenValType>
std::string numToJSON(GenValType genVal) {
  if (R_IsNA(genVal))
    return std::string("null");
  else
    return ttostr((genVal));
}

std::string basicToStr(SEXP Rval) {
  
  std::string res("");
  switch( TYPEOF(Rval) ) {
  case REALSXP: {
    if (RObject(Rval).hasAttribute("dim")) {
      NumericMatrix tmp(Rval);
      res += "[ ";
      for (int j=0; j<tmp.ncol(); j++) {
        res += "[ ";
        for (int i=0; i<tmp.nrow(); i++) {
          res += numToJSON(tmp(i,j)) + std::string(", ");
        }
        res.erase(res.size()-2,1);
        res += "], ";
      }
      res.erase(res.size()-2,1);
      res += std::string("]");
      return res;
    } else {
      NumericVector tmp(Rval);
      if (tmp.size()>1) res += "[ ";
      for (NumericVector::iterator it = tmp.begin(); 
           it != tmp.end(); ++it) {
        res += numToJSON(*it) + std::string(", ");
      }
      res.erase(res.size()-2,1);
      if (tmp.size()>1) res += std::string("]");
      return res;
    }
  }
  case INTSXP: {
    if (RObject(Rval).hasAttribute("dim")) {
      IntegerMatrix tmp(Rval);
      res += "[ ";
      for (int j=0; j<tmp.ncol(); j++) {
        res += "[ ";
        for (int i=0; i<tmp.nrow(); i++) {
          res += numToJSON(tmp(i,j)) + std::string(", ");
        }
        res.erase(res.size()-2,1);
        res += "], ";
      }
      res.erase(res.size()-2,1);
      res += std::string("]");
      return res;
    } else {
      IntegerVector tmp(Rval);
      if (tmp.size()>1) res += "[ ";
      for (IntegerVector::iterator it = tmp.begin(); 
           it != tmp.end(); ++it) {
        res += numToJSON(*it) + std::string(", ");
      }
      res.erase(res.size()-2,1);
      if (tmp.size()>1) res += std::string("]");
      return res;
    }
  }
  case STRSXP: {
    CharacterVector tmp(Rval);
    if (tmp.size()>1) res += "[ ";
    for (CharacterVector::iterator it = tmp.begin(); 
         it != tmp.end(); ++it) {
      std::string tmp2 = Rcpp::as<std::string>(*it);
      res += std::string("\"") + escape_json(tmp2) + std::string("\"") + std::string(", ");
    }
    res.erase(res.size()-2,1);
    if (tmp.size()>1) res += std::string("]");
    return res;
  }
  }
  return std::string("null");
}


// ################################################################################
// #                   PUBLIC FUNCTIONS EXPOSED TO THE R USER
// ################################################################################

// [[Rcpp::export]]
Rcpp::List parseEntry(Rcpp::CharacterVector entryText) {
  std::istringstream f(Rcpp::as<std::string>(entryText));

  Rcpp::List resList;
  Rcpp::List subentList;
  std::string entryId;
  std::string dateStr;
  
  std::string line;
  std::string firstField;

  while (!f.eof()) {
    firstField = getFirstField(line);
    
    if (firstField.compare("ENTRY")==0) {
      entryId = trim(getField(line,2));
      dateStr = trim(getField(line,3));
      getline(f,line);
    }
    else if (firstField.compare("SUBENT") == 0)
      subentList.push_back(parseSubent(line,f));
    else if (firstField.compare("ENDENTRY")==0) {
      std::getline(f,line);
      break;
    } else {
      std::getline(f,line);
    }
  }
  resList["ID"] = entryId;
  resList["DATEMOD"] = dateStr;
  resList["SUBENT"] = subentList;
  return resList; 
}


// [[Rcpp::export]]
Rcpp::CharacterVector convToJSON(Rcpp::List myList) {
  
  std::string res("");
  SEXP dm = myList.attr("names");
  bool isNamed = false;
  Rcpp:CharacterVector fieldNames;
  int i = 0;
  if (!Rf_isNull(dm)) isNamed = true;
  
  if (isNamed) {
    fieldNames = Rcpp::CharacterVector(dm);
    res += std::string("{ ");
  } else {
    res += std::string("[ ");
  }
  
  i = 0;
  for (List::iterator it = myList.begin(); it!=myList.end(); ++it) {
    
    if (isNamed)
      res += std::string("\"") + Rcpp::as<std::string>(fieldNames[i]) + std::string("\": ");
    
    switch (TYPEOF(*it)) {
    
    case VECSXP: {
      res += Rcpp::as<std::string>(convToJSON(Rcpp::List(*it)));
      break;
    }
    default: {
      res += basicToStr(*it);
      break;
    }
    }
    // fill the value
    res += std::string(", ");
    i++;
  }
  // erase the superfluous ", "
  res.erase(res.size()-2,1);
  if (isNamed) {
    res += std::string("}");
  } else {
    res += std::string("]");
  }
  return(res);
}
