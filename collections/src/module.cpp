
#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::plugins("cpp17")]]

#include <string>
#include <iostream>
#include <complex>
#include <list>
#include <map>
#include <set>
#include <any>

#include <deque>






std::string marshall_string(const std::string& X)
{
std::any a(10);
    Rcout << X << std::endl;
    std::string Y {" blady blah ..."};
    return X + Y;
}

template <typename T>
T marshall_T_T(const T& X)
{
    return X;
}

template <typename T>
void marshall_T_void(const T& X)
{
}


template <typename T>
T marshall_double_T(const double& X)
{
  T Y;
  return Y;
}


RCPP_MODULE(marshalling) 
{
// function("rcpp_marshall_string", &marshall_string);


function("rcpp_marshall_list_list_double", &marshall_T_T<std::list<std::list<double> > >);
function("rcpp_marshall_list_vector_double", &marshall_T_T<std::list<std::vector<double> > >);
function("rcpp_marshall_vector_list_double", &marshall_T_T<std::vector<std::list<double> > >);
function("rcpp_marshall_vector_vector_double", &marshall_T_T<std::vector<std::vector<double> > >);

function("rcpp_marshall_vector_double", &marshall_T_T<std::vector<double> > );
function("rcpp_marshall_list_double", &marshall_T_T<std::list<double> > );


function("rcpp_marshall_vector_double_void", &marshall_T_void<std::vector<double> > );

function("rcpp_marshall_double_map_string_double", &marshall_double_T<std::map<std::string,double> > );
function("rcpp_marshall_double_set_double", &marshall_double_T<std::set<double> > );
function("rcpp_marshall_double_unordered_set_double", &marshall_double_T<std::unordered_set<double> > );

}


void deque_push_back(std::deque<SEXP>* obj, SEXP x )
{
  obj->push_back(x);
}

void deque_push_front(std::deque<SEXP>* obj, SEXP x )
{
  obj->push_front(x);
}


void deque_pop_back(std::deque<SEXP>* obj)
{
  obj->pop_back();
}

void deque_pop_front(std::deque<SEXP>* obj)
{
  obj->pop_front();
}

SEXP deque_front(std::deque<SEXP>* obj)
{
  return obj->front();
}

SEXP deque_back(std::deque<SEXP>* obj)
{
  return obj->back();
}


SEXP deque_at(std::deque<SEXP>* obj, const int& i)
{
  return obj->at(i-1);
}


RCPP_MODULE(collections) 
{
function("rcpp_marshall_string", &marshall_string);


class_<std::deque<SEXP> >("deque")
.constructor()
.method("push_back", &deque_push_back)
.method("push_front", &deque_push_front)
.method("pop_back", &deque_pop_back)
.method("pop_front", &deque_pop_front)
.method("front", &deque_front)
.method("back", &deque_back)
.method("at", &deque_at)
;
}







