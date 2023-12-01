run:
	/bin/rm -f lex.yy.* y.tab.* out *.o *.dot *.png *.output output.txt
	flex -8 java.l
	bison -d -t -y javaR.y
	g++ -O2 y.tab.c -c -Wno-write-strings
	g++ -O2 lex.yy.c -c -Wno-write-strings
	g++ -c help.cpp
	g++ -o out y.tab.o lex.yy.o help.o
	./out $<
sym:
	/bin/rm -f lex.yy.* y.tab.* out *.o *.dot *.png *.output symboltable.csv output.txt
	flex -8 javasym.l
	bison -d -t -y javasym.y
	g++ -O2 y.tab.c -c -Wno-write-strings
	g++ -O2 lex.yy.c -c -Wno-write-strings
	g++ -c help.cpp
	g++ -o out y.tab.o lex.yy.o help.o
	./out $< >> output.txt
clean:
	/bin/rm -f lex.yy.* y.tab.* out *.o *.dot *.png *.output symboltable.csv *.txt
