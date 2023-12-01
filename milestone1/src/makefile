java:
	/bin/rm -f lex.yy.* y.tab.* out *.o *.dot *.png *.output
	flex -8 java.l
	bison -d -t -y java.y
	g++ -O2 y.tab.c -c -Wno-write-strings
	g++ -O2 lex.yy.c -c -Wno-write-strings
	g++ -c help.cpp
	g++ -o out y.tab.o lex.yy.o help.o
	echo "strict graph{ " > te.dot
	./out $< >> te.dot
	echo "}" >> te.dot
	dot -Tpng te.dot > te.png
clean:
	/bin/rm -f lex.yy.* y.tab.* out *.o *.dot *.output
