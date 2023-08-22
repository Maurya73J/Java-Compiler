#include <stdio.h>
#include <iostream>
#include <string>
#include <list>
#include "help.h"

using namespace std;





void nodjoin(node *n1, node *n2)
{
    cout << "\"";
    printf("[%d] ", n1->num);
    cout << n1->name << "\""
         << "--"
         << "\"";
    printf("[%d] ", n2->num);
    cout << n2->name << "\"" << endl;
}

void nodtolis(node *n1, list<node *> *l)
{
    if (l->size() == 0)
        return;
    for (auto it : *l)
    {
        cout << "\"";
        printf("[%d] ", n1->num);
        cout << n1->name << "\""
             << "--"
             << "\"";
        printf("[%d] ", it->num);
        cout << it->name << "\"" << endl;
    }
}

void listpushback(list<node *> *l1, list<node *> *l2){
    // list<node *> *tp;
    for (auto it : *l2){
        l1->push_back(it);
    }
}

void nodtostr(node *n1, string s1,string tok)
{
    cout << "\"";
    printf("[%d] ", n1->num);
    cout << n1->name << "\""
         << "--"
         << "\"[" << to_string(++cnt)<<"] "<<tok+" ( "+s1+" )" << "\"" << endl;
}
