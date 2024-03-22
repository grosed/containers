
#include "Rcpp.h"
using namespace Rcpp;


#include <deque>


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

void deque_dispatch(std::deque<SEXP>* obj)
{
  
}


RCPP_MODULE(collections) 
{
class_<std::deque<SEXP> >("deque")
.constructor()
.method("push_back", &deque_push_back)
.method("push_front", &deque_push_front)
.method("pop_back", &deque_pop_back)
.method("pop_front", &deque_pop_front)
.method("front", &deque_front)
.method("back", &deque_back)
.method("at", &deque_at)
.method("dispatch", &deque_dispatch)
;
}







