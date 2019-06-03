# Unix makefile for tigermain exampleD


HOME=/usr/bin
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm /usr/share/mosml/tools
MOSMLLEX=${MOSMLHOME}/mosmllex
MOSMLYACC=${MOSMLHOME}/mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS = tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo \
	tigertemp.uo tigertopsort.uo tigertree.uo tigerframe.uo \
	tigertrans.uo tigerit.uo tigerpila.uo tigermuestratipos.uo \
	tigercanon.uo tigerinterp.uo tigercodegen.uo tigergraph.uo \
	tigerflow.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml tigertopsort.sml tigermuestratipos.sml \
	tigergraph.sml tigerflow.sml

	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigergraph.ui: tigertab.ui 
tigerescap.ui: tigerabs.uo 
tigermuestratipos.ui: tigertips.uo 
tigertree.uo: tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerpila.uo: tigerpila.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo 
tigerpp.uo: tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo \
    tigermuestratipos.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigertab.uo: tigertab.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigertemp.uo: tigertemp.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerseman.ui: tigerabs.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigermuestratipos.ui \
    tigertrans.ui 
tigergraph.uo: tigergraph.ui tigertab.ui tigertemp.ui 
tigerassem.uo: tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerflow.uo: tigerflow.ui tigertab.ui tigergraph.ui tigerassem.uo \
    tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui 
tigerliveness.ui: tigergraph.ui tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigercanon.ui tigerinterp.uo tigerlex.uo tigertrans.ui \
    tigerpp.uo 
