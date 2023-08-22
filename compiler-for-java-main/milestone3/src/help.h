#include <iostream>
#include <list>
#include <string>
#include <stdexcept>
#include <algorithm>
#include <cassert>

using namespace std;

extern int cnt;

class node
{
public:
    int num;
    string name;
    string _id; /* temporarily put everything here */
    node(int _n, string _s) : num(_n), name(_s), _id("") {}
    node(int _n, string _s, string _i) : num(_n), name(_s), _id(_i) {}
};

typedef list<node *> nodes;

void semantic(list<node *> *l1, list<node *> *l2);

void nodjoin(node *n1, node *n2);

void nodtolis(node *n1, list<node *> *l1);

void listpushback(list<node *> *l1, list<node *> *l2);

void nodtostr(node *n1, string s1,string tok);
