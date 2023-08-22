#include <bits/stdc++.h>

using namespace std;

extern int cnt;
struct symtabentry{
    string lexeme;
    string synCat;
    string dataType;
    string class_id;
    string func_id;
    vector<string> arguements;
    int lineno;
    int scope;
    string dimentions;
    struct desc { 
		string reg;
		bool stack;
		bool heap;
	} addr_descriptor;
	int heap_mem = 0;
	int next_use = -1;
	int is_derefer = 0;
	int isArray = 0;
	int size_of_pointed = 0;
	int is_global = 0;

	vector<int> array_dims;
	string storage_class;
};
typedef pair <string, symtabentry*> qid;
struct act_rec{
    stack<string> param;
    int storage=0;
    act_rec * controlLink;
    string returnVal;
    stack<string> local;
};
struct quad{
    string op="";
    string arg1="";
    string arg2="";
    string res="";
    string idx="";
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
