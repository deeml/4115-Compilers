OBJS = ast.cmo parser.cmo scanner.cmo semantics.cmo generator.cmo probl.cmo

#TESTS = \
arith1 \
arith2 \
fib \
for1 \
func1 \
func2 \
func3 \
gcd \
global1 \
hello \
if1 \
if2 \
if3 \
if4 \
ops1 \
var1 \
while1

#TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml bytecode.ml interpret.ml compile.ml execute.ml microc.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

probl : $(OBJS)
	ocamlc -o probl $(OBJS)

.PHONY : test
test : microc testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

#microc.tar.gz : $(TARFILES)
#	cd .. && tar czf microc/microc.tar.gz $(TARFILES:%=microc/%)

.PHONY : clean
clean :
	rm -f probl parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

.PHONY : all
all : 
	make clean && make

ast.cmo: 
ast.cmx: 
probl.cmo: scanner.cmo parser.cmi generator.cmo ast.cmo semantics.cmo
probl.cmx: scanner.cmx parser.cmx generator.cmx ast.cmx semantics.cmx
semantics.cmo: ast.cmo generator.cmo
semantics.cmx: ast.cmx generator.cmx
generator.cmo: scanner.cmo parser.cmi ast.cmo
generator.cmx: scanner.cmx parser.cmx ast.cmx 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
