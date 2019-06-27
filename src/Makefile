# Unix makefile for tigermain example

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
	tigerflow.uo tigerliveness.uo tigercolor.uo tigerregalloc.uo \
	tigersimpleregalloc.uo

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
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigermuestratipos.ui \
    tigertrans.ui 
tigerescap.ui: tigerabs.uo 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo \
    tigermuestratipos.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertemp.uo: tigertemp.ui 
tigerflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertab.uo: tigertab.ui 
tigerseman.ui: tigerabs.uo 
tigergraph.uo: tigergraph.ui tigertab.ui tigertemp.ui 
tigerpp.uo: tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.uo \
    tigertemp.ui tigerpp.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerassem.uo: tigertemp.ui 
tigerflow.uo: tigerflow.ui tigertab.ui tigergraph.ui tigerassem.uo \
    tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui tigerassem.uo tigerpp.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigersimpleregalloc.ui \
    tigerescap.ui tigergrm.ui tigerframe.ui tigerregalloc.ui tigercanon.ui \
    tigerassem.uo tigerinterp.uo tigerlex.uo tigerliveness.ui tigertrans.ui \
    tigerflow.ui tigerpp.uo 
tigergrm.ui: tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigerpila.uo: tigerpila.ui 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigersimpleregalloc.ui: tigerframe.ui tigerassem.uo 
tigercolor.uo: tigercolor.ui tigertab.ui tigerframe.ui tigertemp.ui 
tigerregalloc.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigertree.uo: tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigerregalloc.uo: tigerregalloc.ui tigertab.ui tigergraph.ui tigerframe.ui \
    tigerassem.uo tigertemp.ui tigerliveness.ui tigerflow.ui tigerpp.uo 
tigerliveness.uo: tigerliveness.ui tigertab.ui tigergraph.ui tigertemp.ui \
    tigerflow.ui 
tigergraph.ui: tigertab.ui 
tigermuestratipos.ui: tigertips.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigercolor.ui: tigertab.ui tigergraph.ui tigerframe.ui tigertemp.ui \
    tigerliveness.ui 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
