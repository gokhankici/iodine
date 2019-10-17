DEPDIR := .d
$(shell mkdir -p $(DEPDIR) >/dev/null)
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.Td

MAKEFLAGS += -j3

srcdir = .

SUBDIRS = ivlpp 

dllib=-ldl -lunwind

CXX  = g++
LEX  = flex
YACC = bison

INCLUDE_PATH      = -I.
CPPFLAGS          = -DHAVE_CONFIG_H $(INCLUDE_PATH) -g
ANNOYING_WARNINGS = -Wno-deprecated-declarations -Wno-implicit-fallthrough
# CXXFLAGS          = -Wall -Wextra -Wshadow -g -O2 $(ANNOYING_WARNINGS)
CXXFLAGS          = -Wall -Wextra -Wshadow $(ANNOYING_WARNINGS) -std=c++17
PICFLAGS          = -fPIC
LDFLAGS           = -rdynamic

M = LineInfo.o StringHeap.o

TT = t-dll.o t-dll-api.o t-dll-expr.o t-dll-proc.o t-dll-analog.o
FF = cprop.o nodangle.o synth.o synth2.o syn-rules.o

MY_OBJS = IRExpr.o IRStmt.o IRExporter.o

O = main.o async.o design_dump.o discipline.o dup_expr.o elaborate.o \
    elab_expr.o elaborate_analog.o elab_lval.o elab_net.o \
    elab_scope.o elab_sig.o elab_sig_analog.o elab_type.o \
    emit.o eval.o eval_attrib.o \
    eval_tree.o expr_synth.o functor.o lexor.o lexor_keyword.o link_const.o \
    load_module.o netlist.o netmisc.o nettypes.o net_analog.o net_assign.o \
    net_design.o netclass.o netdarray.o \
    netenum.o netparray.o netqueue.o netscalar.o netstruct.o netvector.o \
    net_event.o net_expr.o net_func.o \
    net_func_eval.o net_link.o net_modulo.o \
    net_nex_input.o net_nex_output.o net_proc.o net_scope.o net_tran.o \
    net_udp.o pad_to_width.o parse.o parse_misc.o pform.o pform_analog.o \
    pform_disciplines.o pform_dump.o pform_package.o pform_pclass.o \
    pform_class_type.o pform_string_type.o pform_struct_type.o pform_types.o \
    symbol_search.o sync.o sys_funcs.o verinum.o verireal.o target.o \
    Attrib.o HName.o Module.o PClass.o PDelays.o PEvent.o PExpr.o PGate.o \
    PGenerate.o PModport.o PPackage.o PScope.o PSpec.o PTask.o PUdp.o \
    PFunction.o PWire.o Statement.o AStatement.o $M $(FF) $(TT) $(MY_OBJS)

.PHONY: install clean

all: config.h _pli_types.h version_tag.h ivl
	$(foreach dir,$(SUBDIRS),$(MAKE) -C $(dir) $@ && ) true

clean:
	$(foreach dir,$(SUBDIRS),$(MAKE) -C $(dir) $@ && ) true
	rm -f *.o

ivl: $O
	$(CXX) $(LDFLAGS) -o ivl $O $(dllib)

install: ivl ivlpp/ivlpp
	install -D ivl ivlpp/ivlpp $(HOME)/.local/bin

%.o: %.cc config.h
	$(CXX) $(DEPFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $*.o
	mv -f $(DEPDIR)/$*.Td $(DEPDIR)/$*.d && touch $@

$(DEPDIR)/%.d: ;
.PRECIOUS: $(DEPDIR)/%.d

# Here are some explicit dependencies needed to get things going.
main.o: main.cc version_tag.h

lexor.o: lexor.cc parse.h

parse.o: parse.cc

# Build this in two steps to avoid parallel build issues (see pr3462585)
parse.cc: $(srcdir)/parse.y
	$(YACC) --verbose -t -p VL -d -o $@ $<

parse.h: parse.cc
	mv parse.cc.h $@ 2>/dev/null || mv parse.hh $@
	touch parse.h

syn-rules.cc: $(srcdir)/syn-rules.y
	$(YACC) --verbose -t -p syn_ -o $@ $<

lexor.cc: $(srcdir)/lexor.lex
	$(LEX) -s -t $< > $@

lexor_keyword.o: lexor_keyword.cc parse.h

lexor_keyword.cc: $(srcdir)/lexor_keyword.gperf
	gperf -o -i 7 -C -k 1-4,6,9,$$ -H keyword_hash -N check_identifier -t $(srcdir)/lexor_keyword.gperf > lexor_keyword.cc || (rm -f lexor_keyword.cc ; false)

-include $(patsubst %.o,$(DEPDIR)/%.d,$O)

print-%  : ; @echo $* = $($*)
