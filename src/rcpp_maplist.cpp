/*
 * rcpp_maplist.cpp
 * This file is part of hashtable
 *
 * Copyright (C) 2013 - Giacomo Bergami
 *
 * hashtable is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * hashtable is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with hashtable. If not, see <http://www.gnu.org/licenses/>.
 */


#include <Rcpp.h>
#include <Rinternals.h>
#include <unordered_set>
#include <map>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
using namespace Rcpp;
using namespace std;

unsigned long local_getPtr(SEXP g) {
    return ((unsigned long)g);
}

SEXP local_ptr_toRObject(unsigned long g) {
    return ((SEXP)g);
}

bool same(SEXP a, SEXP b) {
    return R_compute_identical(a,b,0);
}

bool sameid(int id,SEXP b) {
    return same(local_ptr_toRObject(id),b);
}

// [[Rcpp::export]]
SEXP getPtr(SEXP g) { // Given an R object, it returns an Integer Identifier
    return Rf_ScalarInteger(local_getPtr(g)); 
}

// [[Rcpp::export]]
SEXP ptr_ToObj(SEXP g) { // Given an Integer Identifier, it returns an R Object
    if (Rf_isInteger(g)) {
        return (local_ptr_toRObject(Rf_asInteger(g)));
    } else
        return R_NilValue;
}

struct hashmap {
    unsigned int max;
    map<int,int> *valmap;
    
    // fst: is the object id
    // snd: is the R object ptr
    map<std::string,map<int,int>*> *g;
};



static void _rmHashtable(SEXP xp)
{
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) return;
    delete list->g;
    delete list->valmap;
    R_ClearExternalPtr(xp);
    free(list);
}

// [[Rcpp::export]]
SEXP rmHashtable(SEXP xp) {
    _rmHashtable(xp);
    return R_NilValue;
}

// [[Rcpp::export]]
SEXP newHashT() {
    struct hashmap* hm = (struct hashmap* )malloc(sizeof(struct hashmap));
    if (!hm) return R_NilValue;
    hm->max = 0;
    hm->g = new map<string,map<int,int>*>();
    hm->valmap = new map<int,int>();
    
    SEXP ret;
    ret = R_MakeExternalPtr(hm, Rf_mkString("hashtable_obj"), R_NilValue);
    PROTECT(ret);
    R_RegisterCFinalizerEx(ret, _rmHashtable, TRUE);
    UNPROTECT(1);
    return ret;
    
}

// [[Rcpp::export]]
SEXP getHMaxId(SEXP xp) {
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) return R_NilValue;
    Rf_ScalarInteger(list->max);
}

// [[Rcpp::export]]
SEXP getHSize(SEXP xp) {
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) return R_NilValue;
    Rf_ScalarInteger(list->valmap->size());
}

// [[Rcpp::export]]
SEXP idToObj(SEXP xp,SEXP id) {
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) return R_NilValue;
    map<int,int>::iterator it;
    it = list->valmap->find(Rf_asInteger(id));
    if (it == list->valmap->end()) return R_NilValue;
    return local_ptr_toRObject(it->second);
}



// [[Rcpp::export]]
SEXP addKeyValueElem(SEXP xp,SEXP key,SEXP val) {
    if (!Rf_isString(key)) 
        return Rf_ScalarInteger(0);
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) 
        return Rf_ScalarInteger(0);
    
    // Hash Key
    std::string keystr = as<std::string>(key);
    //Rf_error(keystr.c_str());
    
    // Search with the hashing function
    map<string,map<int,int>*>::iterator it;
    it = list->g->find(keystr);
    
    //If the hashkey was found
    if (!(it == list->g->end())) {
        map<int,int>* elem = it->second;
        char* ele;
        if (!elem) 
            error_return("Error while obtaining déjà inserted data\n");
        for (auto i: *elem) {
            if (sameid(i.second,val)) { return Rf_ScalarInteger(i.first); }    
        }
        list->max++;
        int curpos = list->max;
        elem->insert(pair<int,int>(curpos,local_getPtr(val)));
        list->valmap->insert(pair<int,int>(curpos,local_getPtr(val)));
        return Rf_ScalarInteger(curpos);
    }
    else {
        // Value Index
        list->max++;
        int curpos = list->max;
        map<int,int>* e = new map<int,int>();
        e->insert(pair<int,int>(curpos,local_getPtr(val)));
        list->valmap->insert(pair<int,int>(curpos,local_getPtr(val)));
        list->g->insert(pair<string,map<int,int>*>(keystr,e));
        return Rf_ScalarInteger(curpos);
    }
    
}

// [[Rcpp::export]]
SEXP hasKeyValueElem(SEXP xp,SEXP key,SEXP val) {
    if (!Rf_isString(key)) 
        return Rf_ScalarInteger(0);
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) 
        return Rf_ScalarInteger(0);
    
    // Hash Key
    std::string keystr = as<std::string>(key);
    //Rf_error(keystr.c_str());
    
    // Search with the hashing function
    map<string,map<int,int>*>::iterator it;
    it = list->g->find(keystr);
    
    //If the hashkey was found
    if (!(it == list->g->end())) {
        map<int,int>* elem = it->second;
        char* ele;
        if (!elem) 
            error_return("Error while obtaining déjà inserted data\n");
        for (auto i: *elem) {
            if (sameid(i.second,val)) { return Rf_ScalarInteger(1); }    
        }
        return Rf_ScalarInteger(0);
    }
    else {
        // Value Index
        return Rf_ScalarInteger(0);
    }
    
}

// [[Rcpp::export]]
SEXP remKeyValueElem(SEXP xp,SEXP key,SEXP val) {
    if (!Rf_isString(key)) 
        return Rf_ScalarInteger(0);
    struct hashmap* list = (struct hashmap*)R_ExternalPtrAddr(xp);
    if (!list) 
        return Rf_ScalarInteger(0);
    
    // Hash Key
    std::string keystr = as<std::string>(key);
    //Rf_error(keystr.c_str());
    
    // Search with the hashing function
    map<string,map<int,int>*>::iterator it;
    it = list->g->find(keystr);
    
    //If the hashkey was found
    if (!(it == list->g->end())) {
        map<int,int>* elem = it->second;
        SEXP siz;
        int pos = 0;
        char* ele;
        if (!elem) 
            error_return("Error while obtaining déjà inserted data\n");
        for (auto i: *elem) {
            if (sameid(i.second,val)) { 
                siz = Rf_ScalarInteger(i.first); 
                pos = i.first;
                list->valmap->erase(pos);   // Remove the N->Id element
                break; 
            }
        }
        // If the removed element was the last, remove the hashing code
        if (elem->size()==1)
            list->g->erase(keystr);
        else // Remove only the item
            elem->erase(pos);
        return siz;
    }
    else {
        return Rf_ScalarInteger(0);
    }
    
}

/////////////////////////////////////////////////////////////////////////

#define MAP  map<string,unsigned long>

static void _rmMap(SEXP xp)
{
    MAP* list = (MAP*)R_ExternalPtrAddr(xp);
    if (!list) return;
    R_ClearExternalPtr(xp);
    delete list;
}

// [[Rcpp::export]]
SEXP rmMap(SEXP xp) {
    _rmMap(xp);
    return R_NilValue;
}

// [[Rcpp::export]]
SEXP newMap() {
    MAP* m = new MAP();
    if (!m) return R_NilValue;
    
    SEXP ret;
    ret = R_MakeExternalPtr(m, Rf_mkString("map_obj"), R_NilValue);
    PROTECT(ret);
    R_RegisterCFinalizerEx(ret, _rmMap, TRUE);
    UNPROTECT(1);
    return ret;
    
}

int __map_hasKey(SEXP m, SEXP k) {
    MAP* list = (MAP*)R_ExternalPtrAddr(m);
    if (!list) return 0;
    
    if (!Rf_isString(k)) 
        return 0;
    
    std::string keystr = as<std::string>(k);
    
    MAP::iterator it = list->find(keystr);
    return (it != list->end());
}

// [[Rcpp::export]]
SEXP map_AddKeyVal(SEXP m, SEXP k, SEXP val) {
    MAP* list = (MAP*)R_ExternalPtrAddr(m);
    if (!list) return R_NilValue;
    
    if (!Rf_isString(k)) 
        return R_NilValue;
    
    std::string keystr = as<std::string>(k);
    
    if (!__map_hasKey(m,k))
        list->insert(pair<string,unsigned long>(keystr,local_getPtr(val)));
    else 
        return R_NilValue; 
    
    return m;
    
}


// [[Rcpp::export]]
SEXP map_hasKey(SEXP m, SEXP k) {
    return Rf_ScalarInteger(__map_hasKey(m,k));
}

// [[Rcpp::export]]
SEXP map_getVal(SEXP m, SEXP k) {
    MAP* list = (MAP*)R_ExternalPtrAddr(m);
    if (!list) return R_NilValue;
    
    if (!Rf_isString(k)) 
        return R_NilValue;
    
    std::string keystr = as<std::string>(k);
    
    MAP::iterator it = list->find(keystr);
    if (it == list->end()) 
        return R_NilValue;
    else
        return local_ptr_toRObject(it->second);
}
