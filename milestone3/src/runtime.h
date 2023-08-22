#include <bits/stdc++.h>

using namespace std;

extern int cnt;
struct act_rec{
    stack<string> param;
    int storage=0;
    act_rec * controlLink;
    string returnVal;
    stack<string> local;
};
struct vtable
{
    map<string,act_rec> methods;
};

struct Klass
{
    stack<string> local;
    vtable _vtable;
};

// typedef list<node *> nodes;

// void semantic(list<node *> *l1, list<node *> *l2);

// void nodjoin(node *n1, node *n2);

// void nodtolis(node *n1, list<node *> *l1);

// void listpushback(list<node *> *l1, list<node *> *l2);

// void nodtostr(node *n1, string s1,string tok);
