#include <stdio.h>
#include <iostream>
#include <string>
#include <list>
#include "help.h"

using namespace std;

extern int yylineno;

void semantic(list<node *> *l1, list<node *> *l2)
{
    for (list<node *>::iterator it = l2->begin(); it != l2->end(); ++it)
    {
        if ((*it)->_id == "")
            continue;
        bool found = false;
        for (list<node *>::iterator jt = l1->begin(); jt != l1->end(); ++jt)
        {

            if ((*it)->_id == (*jt)->_id)
            {
                found = true;
                break;
            }
        }
        if (!found)
        {
            char holder[20];
            sprintf(holder, "line %d ", yylineno);
            throw logic_error(string(holder) + "Semantic error on the related code block: undefined " + (*it)->_id);
        }
    }
}

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
