%option noyywrap
%{
#include <bits/stdc++.h>
using namespace std;
map<string,pair<string,pair<int,int> > > lexda;
int i=0;
int lin=1;
%}

%%
([/][*][^*]*[*]+([^*/][^*]*[*]+)*[/])|([/][/].*) {
       // if(lexda[yytext].second==0){
       // lexda[yytext].first="comment";
// }̉
// lexda[yytext].second++;
}
[(){}]|[[]|[]]|[.]{3}|[;,.@]|([:][:]) {if(lexda[yytext].second.first==0){
       lexda[yytext].first="seperator";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}

[=]|[>]|[<]|[!]|[~]|[?]|[:]|([-][>])|([=][=])|([>][=])|([<][=])|([!][=])|([&][&])|([|][|])|([+][+])|([-][-])|[+]|[-]|[*]|[/]|[&]|[|]|^|[%]|([<][<])|([>][>])|([>][>][>])|([+][=])|([-][=])|([*][=])|([/][=])|([&][=])|([|][=])|(^=)|([%][=])|([<][<][=])|([>][>][=])|([>][>][>][=]) {if(lexda[yytext].second.first==0){
       lexda[yytext].first="operator";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}
(["]([^"\n]*)["])|["]["]["][ ]*[\n]+[^"]*(["]["]?([^"])+)*["]["]["]|([']([^'\n]*)[']) {if(lexda[yytext].second.first==0){
       lexda[yytext].first="literal";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}
(abstract|continue|for|new|switch|assert|default|if|package|synchronized|boolean|do|goto|private|this|break|double|implements|protected|throw|byte|else|import|public|throws|case|enum|instanceof|return|transient|catch|extends|int|short|try|char|final|interface|static|void|class|finally|long|strictfp|volatile|const|float|native|super|while|[_]|exports|opens|requires|uses|module|permits|sealed|var|non-sealed|provides|to|with|open|record|transitive|yield)  {if(lexda[yytext].second.first==0){
       lexda[yytext].first="keyword";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}

([ ][+-])?(([0-9_]*((([eE][+-][0-9_]+)?[lL]?)|(([.][0-9_]*([eE][+-][0-9_]+)?[fFdD]?)))?)|([0][b][01_]*((([pP][+-][01_]+)?[lL]?)|(([.][01_]*([pP][+-][01_]+)?[fFdD]?)))?)|([0][x][01a-fA-F_]*((([pP][+-][01a-fA-F_]+)?[lL]?)|(([.][01a-fA-F_]*([pP][+-][01a-fA-F_]+)?[fFdD]?)))?))|(true|false|null) {if(lexda[yytext].second.first==0){
       lexda[yytext].first="literal";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}




([a-zA-Z$_][a-zA-Z0-9$_]*) {if(lexda[yytext].second.first==0){
       lexda[yytext].first="identifier";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;}
\n {lin++;}
\t|[ ] {}
. {cout<<"error in line "<<lin<<endl;
       if(lexda[yytext].second.first==0){
       lexda[yytext].first="error";
       lexda[yytext].second.second=i;
       i++;
}
lexda[yytext].second.first++;
}
%%
// bool comp(pair<string,pair<string,pair<int,int> > > a,pair<string,pair<string,pair<int,int> > > b){
//        return a.second.second.second>b.second.second.secon;
// }

int main(){
    FILE *fi;
    char fn[20],opfile[20];
    cout<<"Enter filename:";
    cin>>fn;
    cout<<"Enter output filename (including \".csv\"):";
       cin>>opfile;
     yyin=fopen(fn,"r");
     yylex();
     ofstream csvfile(opfile);
       vector< pair<int, pair< string, pair<string,int> > > > listed;
    for (auto x : lexda){
       listed.push_back(make_pair(x.second.second.second,make_pair(x.first,make_pair(x.second.first,x.second.second.first))));
    }sort(listed.begin(),listed.end());
    csvfile<<"Lexeme,Token,Count"<<endl;
    for(auto x : listed){
       csvfile<<"\""<<x.second.first<<"\","<<x.second.second.first<<","<<x.second.second.second<<endl;
    }
    return 0;
}
