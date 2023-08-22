#pragma once
#include <bits/stdc++.h>
#include "runtime.h"
using namespace std;
string get_label();
void starting_code();
int is_integer(string sym);
void add_op(quad* instr);
void sub_op(quad* instr);
void mul_op(quad* instr);
void assign_op(quad* instr);
void genCode();
void initializeRegs();
string get_mem_location(string * sym, string* sym2, int idx, int flag);
// string getTemporaryReg(qid* exclude_symbol, int idx);
string getReg(string* sym, string* result, string* sym2, int idx);
void findBasicBlocks();
void dfs(int curr, vector<int>&visited, vector<vector<int> >&adj_list);
vector<int> findDeadCode();
