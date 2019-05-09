#include "config.h"
#include "pform.h"
#include "PClass.h"
#include "PEvent.h"
#include "PGenerate.h"
#include "PPackage.h"
#include "PSpec.h"
#include "PTask.h"
#include "discipline.h"
#include "ivl_target_priv.h"
#include <iostream>
#include <iomanip>
#include <typeinfo>
#include <stdlib.h>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <cstring>

#include "Visitor.h"
#include "PrologExporter.h"
#include "ExprVisitor.h"

#define UNW_LOCAL_ONLY
#include <cxxabi.h>
#include <libunwind.h>
#include <cstdio>
#include <cstdlib>

void backtrace() {
  unw_cursor_t cursor;
  unw_context_t context;

  // Initialize cursor to current frame for local unwinding.
  unw_getcontext(&context);
  unw_init_local(&cursor, &context);

  std::fprintf(stderr, "PRINTING THE STACK TRACE :\n");

  // Unwind frames one by one, going up the frame stack.
  while (unw_step(&cursor) > 0) {
    unw_word_t offset, pc;
    unw_get_reg(&cursor, UNW_REG_IP, &pc);
    if (pc == 0) {
      break;
    }
    std::printf("0x%lx:", pc);

    char sym[256];
    if (unw_get_proc_name(&cursor, sym, sizeof(sym), &offset) == 0) {
      char* nameptr = sym;
      int status;
      char* demangled = abi::__cxa_demangle(sym, nullptr, nullptr, &status);
      if (status == 0) {
        nameptr = demangled;
      }
      std::fprintf(stderr, " (%s+0x%lx)\n", nameptr, offset);
      std::free(demangled);
    } else {
      std::fprintf(stderr, " -- error: unable to obtain symbol name for this frame\n");
    }
  }
  std::fprintf(stderr, "\n");
}

using namespace std;

extern std::map<perm_string,Module*> pform_modules;

const string prolog_comment = "% ";
const string sep  = "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%";
const string sep2 = "%-------------------------------------------------------------------------------";

const string missing_id = "id_MISSING_ID";

const string id_prefix = ""; // "v_";

const string nopStmt = "skip";

unsigned long UninterpretedFunction::name_counter = 0;
unsigned long PrologExporter::idCounter = 1;

void PrologExporter::visit(Module* m) {
    // ###########################################################################
    // print module information
    // ###########################################################################
    out << sep << endl;
    printModuleHeader(out);

    // ###########################################################################
    // registers & wires
    // ###########################################################################
    bool isFirstPort = true;
    out << "[";
    for (map<perm_string,PWire*>::const_iterator wire = m->wires.begin(); wire != m->wires.end(); ++ wire) {
        PWire* w = (*wire).second;
        
        if (w->get_port_type() == PortType::PINOUT) {
          cerr << "inout port types are not supported yet: " << endl;
          w->dump(cerr, 0);
          exit(1);
        }

        if (! isFirstPort) {
            out << ", ";
        }

        switch(w->get_wire_type()) {
        case NetNet::WIRE:
        case NetNet::IMPLICIT:
            out << "wire(";
            break;
        case NetNet::REG:
        case NetNet::IMPLICIT_REG:
            out << "register(";
            break;
        default:
            cerr << "NOT SUPPORTED: wires of type" << w->get_wire_type() << endl;
            exit(1);
        }

        unsigned oldind = ind; ind = 0;
        w->accept(this);
        ind = oldind;

        out << ")";

        isFirstPort = false;
    }
    out << "]," << endl;

    // ###########################################################################
    // PGAssign, PGBuiltin, and PGModule
    // ###########################################################################
    bool isFirstGate = true;
    out << "[";
    if (! isToplevel()) {
        printModInstPortAssignments(); out << endl;
        isFirstGate = false;
    }
    for (auto gateItr = m->get_gates().begin(); gateItr != m->get_gates().end(); ++ gateItr ) {
        unsigned oldind = ind; ind = 0;
        if (isFirstGate) {
            out << endl;
        } else {
            out << "," << endl;
        }
        (*gateItr)->accept(this);
        ind = oldind;

        isFirstGate = false;
    }
    out << endl << "]," << endl;

    // ###########################################################################
    // always, initial and final blocks
    // ###########################################################################
    bool isFirstProcess = true;
    out << "[";
    for (list<PProcess*>::const_iterator idx = m->behaviors.begin() ; idx != m->behaviors.end() ; ++ idx ) {

        if ((*idx)->type() == IVL_PR_INITIAL) {
            // skipping initial block
            continue;
        }

        unsigned oldind = ind; ind = 0;
        if (isFirstProcess) {
            out << endl;
        } else {
            out << "," << endl;
        }
        (*idx)->accept(this);
        ind = oldind;
        isFirstProcess = false;
    }
    out << endl << "]," << endl;

    // ###########################################################################
    // uninterpreted functions
    // ###########################################################################
    out << "[";
    if (ufs.size() > 0) {
        ufs[0].export_to_prolog(out);
    }
    for (unsigned i=1; i < ufs.size(); i++) {
        out << ", ";
        ufs[i].export_to_prolog(out);
    }
    out << "])";

    if (isToplevel()) {
        out << "." << endl << endl;
    }

    out << endl << sep2 << endl;

    // ###########################################################################
    // missing functionality
    // ###########################################################################
    // skipping variable initializations
    if (m->var_inits.size() != 0) {
        out << endl << prolog_comment << "skipping variable initializations" << endl;
        for(unsigned i=0; i < m->var_inits.size(); i++) {
            Statement* s = m->var_inits[i];
            PAssign* ba  = dynamic_cast<PAssign*>(s);
            assert(ba != NULL);

            PExpr* rexpr = ba->rval();
            if (rexpr == NULL) {
                // don't know what to do, so continue
                continue;
            } 

            FoldNames fn(this);
            rexpr->accept(&fn);

            if(! fn.names.empty()) {
                cerr << endl;
                s->dump(cerr, 0);
                cerr << endl << "initial assignment is not a constant blocking assignment" << endl;
                exit(1);
            }
        }
    }

    // attributes
    if (m->attributes.begin() != m->attributes.end()) {
        cerr << "NOT SUPPORTED: attributes" << endl;
        exit(1);
    }

    // typedefs
    if (m->typedefs.begin() != m->typedefs.end()) {
        cerr << "NOT SUPPORTED: typedefs" << endl;
        exit(1);
    }

    // specparams
    if (m->specparams.begin() != m->specparams.end()) {
        cerr << "NOT SUPPORTED: specparams" << endl;
        exit(1);
    }

    // enumerations
    if (m->enum_sets.begin() != m->enum_sets.end()) {
        cerr << "NOT SUPPORTED: enumerations" << endl;
        exit(1);
    }

    // classes
    if (m->classes.begin() != m->classes.end()) {
        cerr << "NOT SUPPORTED: classes" << endl;
        exit(1);
    }

    // nested modules
    if (m->nested_modules.begin() != m->nested_modules.end()) {
        cerr << "NOT SUPPORTED: nested modules" << endl;
        exit(1);
    }

    // generate variables
    if (m->genvars.begin() != m->genvars.end()) {
        cerr << "NOT SUPPORTED: genvars" << endl;
        exit(1);
    }

    // generate schemes
    if (m->generate_schemes.begin() != m->generate_schemes.end()) {
        cerr << "NOT SUPPORTED: generate schemes" << endl;
        exit(1);
    }

    // defparms
    if (m->defparms.begin() != m->defparms.end()) {
        cerr << "NOT SUPPORTED: defparms" << endl;
        exit(1);
    }

    // events
    if (m->events.begin() != m->events.end()) {
        cerr << "NOT SUPPORTED: events" << endl;
        exit(1);
    }

    // tasks
    if (m->tasks.begin() != m->tasks.end()) {
        cerr << "NOT SUPPORTED: tasks" << endl;
        exit(1);
    }

    // analog behaviors
    if (m->analog_behaviors.begin() != m->analog_behaviors.end()) {
        cerr << "NOT SUPPORTED: analog behaviors" << endl;
        exit(1);
    }

    // specify paths
    if (m->specify_paths.begin() != m->specify_paths.end()) {
        cerr << "NOT SUPPORTED: specify paths" << endl;
        exit(1);
    }
}

void PrologExporter::visit(PWire* w) {
    switch(w->get_wire_type()) {
    case NetNet::REG:
    case NetNet::WIRE:
    case NetNet::IMPLICIT:
    case NetNet::IMPLICIT_REG: {
        out << export_name_component_to_prolog(w->basename(), std::list<index_component_t>());
        break;
    }
    default:
        cerr << "NOT SUPPORTED: wires of type" << w->get_wire_type() << endl;
        exit(1);
    }
}

void PrologExporter::visit(PGate* g) {
    cerr << "NOT SUPPORTED: PrologExporter " << typeid(*g).name() << endl;
    exit(1);
}


void PrologExporter::export_pins_to_prolog(PGate* g) {
    if (g->pin_count()) {
        if (g->pin(0))  g->pin(0)->accept(this);

        for (unsigned idx = 1 ;  idx < g->pin_count() ;  idx += 1) {
            out << ", ";
            if (g->pin(idx)) g->pin(idx)->accept(this);
        }
    }
}

// continuous assignment
void PrologExporter::visit(PGAssign* ga) { // asn
    if (ga->delay_count() != 0) {
        cerr << "continuous assignment has a delay:" << endl;
        ga->dump(cerr,0);
        cerr << endl;
    }

    if (ga->pin_count() != 2 || ga->pin(0) == NULL || ga->pin(1) == NULL) {
        cerr << "NOT SUPPORTED: PrologExporter@PGAssign: sth wrong with pins" << endl;
        exit(1);
    }

    doAsgn(ContAsgn, ga->pin(0), ga->pin(1));
}

void PrologExporter::visit(PGBuiltin* gb) {
    unsigned inputCnt = 0;
    out << "/* ";
    switch (gb->type()) {
    case PGBuiltin::AND:  out << "and";  inputCnt = 2; break;
    case PGBuiltin::NAND: out << "nand"; inputCnt = 2; break;
    case PGBuiltin::OR:   out << "or";   inputCnt = 2; break;
    case PGBuiltin::NOR:  out << "nor";  inputCnt = 2; break;
    case PGBuiltin::XOR:  out << "xor";  inputCnt = 2; break;
    case PGBuiltin::XNOR: out << "xnor"; inputCnt = 2; break;
    case PGBuiltin::NOT:  out << "not";  inputCnt = 1; break;
    default:
        cerr << "NOT SUPPORTED: builtin gate type: " << gb->type() << endl;
        exit(1);
    }
    out << " gate" << " */ ";

    if (inputCnt + 1 != gb->pin_count()) {
        cerr << "builtin gate ";
        gb->dump(cerr, 0);
        cerr << " has wrong number of inputs" << endl;
        exit(1);
    }

    out << "asn(";
    PExpr* outExp = gb->pin(0);
    if (outExp == NULL) {
        cerr << "builtin gate's output pin is NULL" << endl;
        exit(1);
    }
    out << exportExpr(outExp) << ",";

    if (inputCnt == 1) {
        PExpr* inExp = gb->pin(1);

        if (inExp == NULL) {
            cerr << "builtin gate's input pin is NULL" << endl;
            exit(1);
        }
        out << exportExpr(inExp) << ")";
    } else {
        PExpr* inExp = NULL;
        FoldNames fn(this);

        for (unsigned i=1; i<inputCnt + 1; i++) {
            inExp = gb->pin(i);
            if (inExp == NULL) {
                cerr << "builtin gate's input pin is NULL" << endl;
                exit(1);
            }
            inExp->accept(&fn);
        }

        UninterpretedFunction uf(fn.names.begin(), fn.names.end());
        ufs.push_back(uf);

        out << uf.getOutput() << ")";
    }
}

void PrologExporter::visit(PGModule* gm) {
    const perm_string module_type = gm->get_type();
    string module_name(gm->get_name().str());

    Module* mod = NULL;

    auto mod_itr = pform_modules.find(module_type);
    if (mod_itr != pform_modules.end()) {
        mod = (*mod_itr).second;
    } else {
        cerr << endl << "module " << module_type.str() << " not found !";
        exit(1);
    }

    PGate* g = (PGate*) gm;

    PrologExporter submoduleExporter(this, mod, gm);

    if (gm->pins_) {
        for (unsigned i=0; i < gm->npins_; i++) {
            PExpr* expr = gm->pins_[i].parm;
            if(expr == NULL) {
                cerr << endl << "pin is null of the following module:" << endl;
                printModuleHeader(cerr); cerr << endl;
                gm->dump(cerr, 0);
                exit(1);
            }
            string name(gm->pins_[i].name.str());
            // string exprId = exportExpr(expr);

            submoduleExporter.portMapping.emplace(name, expr);
        }
    } else {
        for (unsigned i = 0; i < g->pin_count(); i += 1) {
            PExpr* expr = g->pin(i);
            if(expr == NULL) {
                cerr << endl << "pin is null of the following module:" << endl;
                printModuleHeader(cerr); cerr << endl;
                gm->dump(cerr, 0);
                exit(1);
            }
            string name(mod->ports[i]->name.str());
            // string exprId = exportExpr(expr);

            submoduleExporter.portMapping.emplace(name, expr);
        }
    }

    mod->accept(&submoduleExporter);
}

void PrologExporter::visit(Statement* s) {
    cerr << "NOT SUPPORTED: PrologExporter " << typeid(*s).name() << endl;
    exit(1);
}

void PrologExporter::visit(PProcess* p) {
    if (p->type() == IVL_PR_ALWAYS) {
        out << "always(";

        // export_attributes_map_to_prolog(out, attributes, ind+2);

        if (p->statement()) {
            unsigned oldind = ind; ind = ind+7;
            p->statement()->accept(this);
            ind = oldind;
        } else {
            cerr << "statement_ == NULL in PProcess" << endl;
            exit(1);
        }

        out << ")";
    } else {
        cerr << "NOT SUPPORTED: non always process block" << endl;
        exit(1);
    }
}

void PrologExporter::visit(PEventStatement* es) {
    if (es->expr_.count() == 0) {
        out << "event1(star)"; // @*
    } else if ((es->expr_.count() == 1) && (es->expr_[0] == 0)) {
        cerr << "PEventStatement: event expression is wait_fork" << endl;
        exit(1);
    } else if (es->expr_.count() == 1) {
        es->expr_[0]->accept(this);
    } else {
        cerr << "PEventStatement: multiple event expression" << endl;
        es->dump(cerr, ind);
        exit(1);
    }

    out << "," << endl;
    out << setw(ind) << "";

    if (es->statement_) {
        es->statement_->accept(this);
    } else {
        cerr << "statement_ == NULL in PEventStatement" << endl;
        exit(1);
    }
}

void PrologExporter::visit(PCondit* c) {
    out << "ite(";

    out << exportExpr(c->expr_);

    out << "," << endl;

    out << setw(ind+4) << "";
    if (c->if_) {
        unsigned oldind = ind; ind = ind+4;
        c->if_->accept(this);
        ind = oldind;
    }  else {
        out << nopStmt;
    }
    out << "," << endl;

    out << setw(ind+4) << "";
    if (c->else_) {
        unsigned oldind = ind; ind = ind+4;
        c->else_->accept(this);
        ind = oldind;
    }  else {
        out << nopStmt;
    }

    out << ")";
}

// blocking assignment
void PrologExporter::visit(PAssign* ba) { // b_asn
    if (ba->delay_ || ba->count_ || ba->event_) {
        cerr << "Blocking assignment has a delay, repeat or event:" << endl;
        ba->dump(cerr, 0);
        exit(1);
    }

    doAsgn(BlockingAsgn, ba->lval_, ba->rval_);
}

// non blocking assignment
void PrologExporter::visit(PAssignNB* nba) { // nb_asn
    if (nba->count_ || nba->event_) {
        cerr << "Non-blocking assignment has a delay, repeat or event:" << endl;
        nba->dump(cerr, 0);
        exit(1);
    }

    PExpr* delayExpr = nba->delay_;
    if (delayExpr) {
        cerr << endl;
        nba->dump(cerr, 0);
        cerr << endl;
    }

    doAsgn(NonBlockingAsgn, nba->lval_, nba->rval_);
}

void PrologExporter::visit(PBlock* b) {
    out << "block([";

    if (b->pscope_name() != 0) {
        cerr << "NOT SUPPORTED: PBLock w/ pscope_name non-NULL" << endl;
        exit(1);
    }

    if (b->list_.size() == 0) {
        out << "])" << endl;
        return;
    } else {
        out << " ";
    }


    unsigned oldind = ind; ind = ind+8;
    b->list_[0]->accept(this);
    ind = oldind;

    out << endl;

    for (unsigned idx = 1;  idx < b->list_.size() ;  idx += 1) {
        out << setw(ind+6) << "" << ", ";
        if (b->list_[idx]) {
            Statement* s = b->list_[idx];

            oldind = ind; ind = ind+8;
            s->accept(this);
            ind = oldind;
        } else {
            out << nopStmt;
        }
        out << endl;
    }

    out << setw(ind+6) << "" << "])";
}

void PrologExporter::visit(PCallTask* ct) {
    ostringstream os;
    os << ct->path();
    string taskname = os.str();

    if (taskname == "$readmemh" || 
        taskname == "$display"  || 
        taskname == "$finish" ) {
        // do nothing ...
        out << nopStmt;
    } else {
        cerr << endl << "unknown task name " << taskname << endl;
        exit(1);
    }
}

typedef struct CaseStruct{
    string      itemVar;
    Statement*  stmt;
} CaseStruct;

void PrologExporter::visit(PCase* c) {
    vector<CaseStruct> items;
    bool hasDefault = false;
    Statement* defaultStmt = NULL;

    string caseExpr = exportExpr(c->expr_);

    for (unsigned idx = 0 ;  idx < c->items_->count() ;  idx += 1) {
        PCase::Item* cur = (* c->items_)[idx];
        if (cur == NULL || cur->stat == NULL) {
            continue;
        }

        CaseStruct cs;

        if (cur->expr.size() == 0 && cur->stat) {
            hasDefault  = true;
            defaultStmt = cur->stat;
        } else {
            FoldNames fn(this);

            for(auto idx_exp = cur->expr.begin();
                idx_exp != cur->expr.end();
                ++idx_exp) {
                PExpr* e = *idx_exp;
                e->accept(&fn);
            }

            fn.names.insert(caseExpr);

            UninterpretedFunction uf(fn.names.begin(), fn.names.end());
            ufs.push_back(uf);

            cs.itemVar = uf.getOutput();
            cs.stmt    = cur->stat;

            items.push_back(cs);
        }
    }

    if (items.size() == 0) {
        if (hasDefault) {
            defaultStmt->accept(this);
        }
        return;
    }

    for (unsigned i = 0; i < items.size(); i++) {
        out << "ite(";
        out << items[i].itemVar << ", "; // condition
        items[i].stmt->accept(this);     // then branch
        out << ",";
    }

    if (hasDefault) {
        defaultStmt->accept(this);
    } else {
        out << nopStmt;
    }

    for (unsigned i = 0; i < items.size(); i++) {
        out << ")";
    }
}

void PrologExporter::visit(PExpr* e) {
    cerr << "PrologExporter for " << typeid(*e).name() << " is not implemented" << endl;
    exit(1);
}

void PrologExporter::visit(PEIdent* id) {
    if (id->package_) {
        cerr << "PrologExporter @ PEIdent:" << endl;
        cerr << "identifier has a package name" << endl;
        exit(1);
    }

    out << export_pform_name_to_prolog(id->path_);
}

void PrologExporter::visit(PETernary* te)       {
    out << "ite(";
    out << exportExpr(te->expr_);
    out << ",";
    te->tru_->accept(this);
    out << ", ";
    te->fal_->accept(this);
    out << ")";
}

void PrologExporter::visit(PEConcat* cat)        {
    out << exportExpr(cat);
}

void PrologExporter::visit(PECallFunction* cf) {
    cerr << endl;
    cerr << "CALLING FUNCTION: ";

    if (cf->package_) cerr << cf->package_->pscope_name() << "::";

    cerr << cf->path_ << "(";

    if (! cf->parms_.empty()) {
        if (cf->parms_[0]) cf->parms_[0]->accept(this);
        for (unsigned idx = 1; idx < cf->parms_.size(); ++idx) {
            cerr << ", ";
            if (cf->parms_[idx]) cerr << cf->parms_[idx];
        }
    }
    cerr << ")" << endl;
    exit(1);
}

static void print_number(ostream&o, const verinum&v) {
    if (v.is_string()) {
        cerr << endl << "NOT SUPPORTED: Number as a string: " << v.as_string() << endl;
        exit(1);
    }

    /* If the number is fully defined (no x or z) then print it
       out as a decimal number. */
    unsigned dec_len = 8*sizeof(int);  /* avoid 32/64 bit differences. */
    if (! v.has_sign()) dec_len -= 1;  /* an unsigned number. */
    if (v.is_defined() && v.len() <= dec_len) {
        if (v.has_sign())
            // o << "'sd" << v.as_long();
            o << v.as_long();
        else
            // o << "'d" << v.as_ulong();
            o << v.as_ulong();
        return;
    }

    o << "0b";

    if (v.len() == 0) {
        o << "0";
        return;
    }

    verinum::V trim_left = v.get(v.len()-1);
    unsigned idx;

    if (v.has_sign()) {
        for (idx = v.len()-1;  idx > 0;  idx -= 1)
            if (trim_left != v.get(idx-1))
                break;

        o << trim_left;
    } else {
        idx = v.len();
    }

    while (idx > 0) {
        o << v.get(idx-1);
        idx -= 1;
    }
}

void PrologExporter::visit(PENumber* n) {
    print_number(out, n->value());
}

void PrologExporter::visit(PEEvent* ev) {
    out << "event2(";
    switch (ev->type()) {
    case PEEvent::POSEDGE:
        out << "posedge";
        break;
    case PEEvent::NEGEDGE:
        out << "negedge";
        break;
    default:    
        cerr << endl << "PEvent: NOT SUPPORTED: ";
        ev->dump(cerr);
        cerr << endl;
        exit(1);
    }
    out << ",";
    ev->expr()->accept(this);
    out << ")";
}

void PrologExporter::visit(PEBinary* be) {
    out << "binexp(\"";
    switch (be->op_) {
    case 'a': out << "and";          break; // &&
    case 'o': out << "or";           break; // ||
    case 'e': out << "eq";           break; // ==
    case 'n': out << "neq";          break; // !=
    case 'l': out << "shl";          break; // <<
    case 'r': out << "shr";          break; // >>
    case '&': out << "bitwise-and";  break; // &
    case '|': out << "bitwise-or";   break; // |
    case '<': out << "lt";           break; // <
    case '>': out << "gt";           break; // >
    case '^': out << "xor";          break; // ^
    case '+': out << "add";          break; // +
    case '-': out << "sub";          break; // -
    case '*': out << "mul";          break; // *
    case '/': out << "div";          break; // /
    default:
        cerr << endl << "NOT SUPPORTED: Binary expr operand: " << be->op_ << endl;
        exit(1);
//    case 'E': out << "==="; break;
//    case 'L': out << "<=";  break;
//    case 'N': out << "!=="; break;
//    case 'p': out << "**";  break;
//    case 'R': out << ">>>"; break;
//    default:  out << op_;   break;
    }

    out << "\", ";
    be->left_->accept(this);
    out << ", ";
    be->right_->accept(this);
    out << ")";
}

void PrologExporter::visit(PEUnary* ue) {
    out << "unexp(\"";
    switch (ue->op_) {
    case 'm': out << "abs"; break;
    case '!': out << "not"; break;
    case '~': out << "neg"; break;
    default:
        cerr << endl << "NOT SUPPORTED: Unary expr operand: " << ue->op_ << endl;
        exit(1);
    }
    out << "\", ";
    ue->expr_->accept(this);
    out << ")";
}

void PrologExporter::visit(PEString* s) {
    out << "\"" << s->text_ << "\"";
}

void PrologExporter::visit(PDelays*) {
    cerr << endl << "visiting a  delay ..." << endl;
    exit(1);
}

void PrologExporter::visit(data_type_t*)     {}

string PrologExporter::export_name_component_to_prolog(
    const perm_string &name,
    const std::list<index_component_t>& indices)
{
    string nameStr(name.str());
    bool varExists = false;
    PExpr* paramExp = NULL;

    // check if variable is a parameter
    if (auto paramItr = module->parameters.find(name); paramItr != module->parameters.end()) {
        varExists = true;
        paramExp = paramItr->second.expr;
    } else if (auto localParamItr = module->localparams.find(name); localParamItr != module->localparams.end()) {
        varExists = true;
        paramExp = localParamItr->second.expr;
    } 

    if (paramExp) {
        if (moduleInstantiation == NULL) {
            return exportExpr(paramExp);
        }

        if (moduleInstantiation->overrides_ && (! moduleInstantiation->overrides_->empty())) {
            auto orItr        = moduleInstantiation->overrides_->begin();
            auto allParamsItr = module->param_names.begin();

            while (allParamsItr != module->param_names.end()) {
                perm_string paramName = *allParamsItr;
                PExpr* overriddenExp = *orItr;

                if (strcmp(paramName.str(), name.str()) == 0) {
                    if (overriddenExp == NULL) {
                        break;
                    }
                    return exportExpr(overriddenExp);
                }

                ++orItr;
                ++allParamsItr;
            }
        }

        if (moduleInstantiation->parms_) {
            assert(moduleInstantiation->overrides_ == NULL);

            for (unsigned i = 0; i < moduleInstantiation->nparms_; i++) {
                named<PExpr*>& n = moduleInstantiation->parms_[i];

                if (strcmp(n.name.str(), name.str()) == 0) {
                    return exportExpr(n.parm);
                }
            }
        }

        return exportExpr(paramExp);
    }

    if (!varExists) {
        varExists = 
            (module->wires.find(name) != module->wires.end()) ||
            (module->funcs.find(name) != module->funcs.end());
    }

    if (! varExists) {
        cerr << endl << "cannot find variable '" << nameStr << "' in module ";
        printModuleHeader(cerr);
        cerr << endl;
        backtrace();
        exit(1);
    }

    ostringstream os;

    for (unsigned i = 1; i < parentModules.size(); ++i) {
        os << "m_" << parentModules[i].moduleInstantiation->get_name() << "_";
    }

    if (moduleInstantiation != NULL) {
        os << "m_" << moduleInstantiation->get_name() << "_";
    }

    os << id_prefix << nameStr;

    string i = os.str();

    if (! indices.empty()) {
        FoldNames fn(this);
        fn.names.insert(i);

        for (auto idx = indices.begin(); idx != indices.end(); ++idx) {
            const index_component_t& ic = (*idx);
            switch (ic.sel)
            {
            case index_component_t::SEL_BIT:
                ic.msb->accept(&fn);
                break;
            case index_component_t::SEL_PART:
            case index_component_t::SEL_IDX_UP:
            case index_component_t::SEL_IDX_DO:
                ic.msb->accept(&fn);
                ic.lsb->accept(&fn);
                break;
            case index_component_t::SEL_BIT_LAST:
            default:
                break;
            }
        }

        UninterpretedFunction uf(fn.names.begin(), fn.names.end());
        ufs.push_back(uf);
        return uf.getOutput();
    } else {
        return i;
    }
}

string PrologExporter::export_pform_name_to_prolog(const pform_name_t&that) {
    pform_name_t::const_iterator cur;

    cur = that.begin();
    const name_component_t& n = *cur;
    string res = export_name_component_to_prolog(n.name, n.index);

    ++ cur;

    if (cur != that.end()) {
        cerr << endl << "NOT SUPPORTED: multiple name components: " << that << endl;
        exit(1);
    }

    return res;
}

string PrologExporter::exportExpr(PExpr* expr) {
    if (expr == NULL) {
        backtrace();
        assert(expr != NULL);
    }

    PEIdent* id = dynamic_cast<PEIdent*>(expr);
    if (id != NULL) {
        return export_pform_name_to_prolog(id->path_);
    } else {
        UninterpretedFunction uf(this, expr);
        ufs.push_back(uf);
        return uf.getOutput();
    }
}

void PrologExporter::printModuleName(ostream& o) {
    for (unsigned i = 1; i < parentModules.size(); ++i)
    {
        o << "m_" << parentModules[i].moduleInstantiation->get_name() << "_";
    }
    o << "m_" << moduleInstantiation->get_name();
}

void PrologExporter::printModuleHeader(ostream& o) {
    // print name
    if (isToplevel()) {
        o << "topmodule(";
    } else {
        o << "module(";
        assert(this->module != NULL);
        o << this->module->mod_name() << ", ";
        printModuleName(o);
        o << ", ";
    }
    o << endl;

    // print formal parameters
    if (module->port_count() == 0) {
        o << "[]";
    } else {
        bool isFirst = true;
        o << "[";

        for (unsigned i = 0; i < module->port_count(); i++) {
            Module::port_t* p = module->ports[i];
            if (p == NULL) {
                cerr << "unconnected port in " << module->mod_name() << endl;
                exit(1);
            }

            string portname = export_name_component_to_prolog(p->name, std::list<index_component_t>());

            if (p->expr.size() != 1 ||
                portname.compare(export_pform_name_to_prolog(p->expr[0]->path()).c_str()) != 0
                ) {
                cerr << "port in " << module->mod_name() << " has weird internal connections" << endl;
                cerr << "    ." << p->name << "(" << *p->expr[0];
                for (unsigned wdx = 1 ;  wdx < p->expr.size() ;  wdx += 1) {
                  cerr << ", " << *p->expr[wdx];
                }
                cerr << ")";
                exit(1);
            }

            auto wireItr = module->wires.find(p->name);
            PWire* wire = NULL;

            if (wireItr == module->wires.end()) {
                cerr << portname << " not found in module->wires" << endl;
                exit(1);
            }
            wire = wireItr->second;
            assert(wire != NULL);

            if (isFirst) {
                isFirst = false;
            } else {
                o << ", ";
            }

            switch (wire->get_port_type()) {
                case PortType::PINPUT:  out << "input(" << portname << ")";  break;
                case PortType::POUTPUT: out << "output(" << portname << ")"; break;
                default: {
                    cerr << portname << " is not 'input' or 'output'" << endl;
                    wire->dump(cerr, 0);
                    exit(1);
                }
            }
        }
        o << "]";
    }
    o << ", " << endl;
}

void PrologExporter::visit(PForStatement* fr) {
    cerr << endl << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*fr).name() << endl;
    exit(1);
}

static void printAsgn(PrologExporter::AssignType a, PExpr* lhs, PExpr* rhs);

void PrologExporter::doAsgn(AssignType a, AsgnArg lhsArg, AsgnArg rhsArg) {
    PExpr* lhs = lhsArg.e;
    PExpr* rhs = rhsArg.e;

    PrologExporter* lPE = lhsArg.pe;
    PrologExporter* rPE = rhsArg.pe;

    if (lhs == NULL || rhs == NULL) {
        cerr << endl << "lhs or rhs is NULL @ PrologExporter::doAsgn" << endl;
        exit(1);
    }

    switch(a) {
        case ContAsgn:         out << "asn(";    break;
        case BlockingAsgn:     out << "b_asn(";  break;
        case NonBlockingAsgn:  out << "nb_asn("; break;
    }


    PEIdent* lhs_id = dynamic_cast<PEIdent*>(lhs);
    if (lhs_id == NULL) {
        cerr << endl << "lhs of the following assignment is not a PEIdent !!!" << endl;
        printAsgn(a,lhs,rhs);
        exit(1);
    }

    if(lhs_id->path_.size() != 1) {
        cerr << endl << "lhs of the following assignment has multiple path_ !!!" << endl;
        printAsgn(a,lhs,rhs);
        exit(1);
    }

    const name_component_t& lhs_nc = lhs_id->path_.front();
    string lhsExprId = 
        lPE->export_name_component_to_prolog(lhs_nc.name, 
                                             std::list<index_component_t>());
    out << lhsExprId << ", ";

    // ------------------------------------------------------------
    // check whether the lhs of the assignment is a wire
    // ------------------------------------------------------------
    bool isWire = false;
    PWire* lhsWire = NULL;
    PrologExporter* pe = this;
    while (pe != NULL) {
        lhsWire = pe->module->wires_find(lhs_nc.name);
        if(lhsWire) break;
        pe = pe->parentExporter;
    } 
    if(lhsWire == NULL) {
        cerr << endl << "lhsWire is NULL" << endl;
        printAsgn(a, lhsArg.e, rhsArg.e);
        exit(1);
    }
    switch (lhsWire->get_wire_type()) {
    case NetNet::WIRE:
    case NetNet::IMPLICIT:
        isWire = true;
        break;
    case NetNet::REG:
    case NetNet::IMPLICIT_REG:
        isWire = false;
        break;
    default:
        cerr << "NOT SUPPORTED: (in doAsgn) wires of type" << lhsWire->get_wire_type() << endl;
        exit(1);
    }
    // ------------------------------------------------------------

    if (lhs_nc.index.empty() || isWire) {
        // it's simple if lhs does not have an index
        // or if it's a wire, then the new value does not depend on the old one
        out << rPE->exportExpr(rhs);
    } else if(lhs_nc.index.size() == 1) {
        // if assignment is of the form 
        // x[i] <= y
        // convert it to
        // x <= f(x,i,y)

        const index_component_t& lhs_ic = lhs_nc.index.front();
        string rhsExprId = rPE->exportExpr(rhs);
        FoldNames lhs_fn(lPE);

        switch (lhs_ic.sel)
        {
        case index_component_t::SEL_PART:
        case index_component_t::SEL_IDX_UP:
        case index_component_t::SEL_IDX_DO:
            lhs_ic.lsb->accept(&lhs_fn);
            // fall through ...
        case index_component_t::SEL_BIT: {
            lhs_ic.msb->accept(&lhs_fn);

            lhs_fn.names.insert(rhsExprId);
            lhs_fn.names.insert(lhsExprId);

            UninterpretedFunction uf(lhs_fn.names.begin(), lhs_fn.names.end());
            ufs.push_back(uf);
            out << uf.getOutput();

            break;
        }
        case index_component_t::SEL_BIT_LAST:
        default:
            out << rhsExprId;
            break;
        }
    } else if(lhs_nc.index.size() > 1) {
        // if assignment is of the form 
        // x[i][j][k]... <= y
        // convert it to
        // x <= f(x,i,j,k,y)

        string rhsExprId = rPE->exportExpr(rhs);
        FoldNames lhs_fn(lPE);

        bool hasAnIndex = false;

        for(auto& lhs_ic : lhs_nc.index) {
            switch (lhs_ic.sel) {
            case index_component_t::SEL_PART:
            case index_component_t::SEL_IDX_UP:
            case index_component_t::SEL_IDX_DO:
                lhs_ic.lsb->accept(&lhs_fn);
                // fall through ...
            case index_component_t::SEL_BIT: {
                lhs_ic.msb->accept(&lhs_fn);
                hasAnIndex = true;
                break;
            }
            case index_component_t::SEL_BIT_LAST:
            default:
                break;
            }
        }

        if(!hasAnIndex) {
            out << rhsExprId;
        } else {
            lhs_fn.names.insert(rhsExprId);
            lhs_fn.names.insert(lhsExprId);

            UninterpretedFunction uf(lhs_fn.names.begin(), lhs_fn.names.end());
            ufs.push_back(uf);
            out << uf.getOutput();
        }
    }

    out << ")";
}

static void printAsgn(PrologExporter::AssignType a, PExpr* lhs, PExpr* rhs) {
    switch (a)
    {
    case PrologExporter::ContAsgn:
        cerr << "assign "; lhs->dump(cerr); cerr << " = "; rhs->dump(cerr); cerr << endl;
        break;
    case PrologExporter::BlockingAsgn:
        lhs->dump(cerr); cerr << " = "; rhs->dump(cerr); cerr << endl;
        break;
    case PrologExporter::NonBlockingAsgn:
        lhs->dump(cerr); cerr << " <= "; rhs->dump(cerr); cerr << endl;
        break;
    }
}

void PrologExporter::printModInstPortAssignments()
{
    assert(! isToplevel());

    if (module->port_count() == 0) {
        out << "skip)";
        return;
    }
    bool isFirstArg = true;

    for (unsigned i = 0; i < module->port_count(); i++) {
        if (isFirstArg) {
            isFirstArg = false;
        } else {
            out << ", ";
        }

        Module::port_t* p = module->ports[i];
        if (p->expr.size() != 1) {
            cerr << endl;
            cerr << "port "  << p->name << " of ";
            printModuleName(cerr);
            cerr << " has multiple internal names !" << endl;
            exit(1);
        }

        auto pmItr = portMapping.find(p->name.str());
        if (pmItr == portMapping.end())
        {
            cerr << "could not find " << p->name << " in ports of " << moduleInstantiation->name_ << endl;
            exit(1);
        }


        PExpr* formalParam = p->expr[0];
        PExpr* actualParam = pmItr->second;

        auto formalWireItr = module->wires.find(p->name);
        assert(formalWireItr != module->wires.end());
        PWire* formalWire = formalWireItr->second;

        switch (formalWire->get_port_type()) {
        case PortType::PINPUT:
            doAsgn(PrologExporter::ContAsgn,
                   {this,           formalParam}, 
                   {parentExporter, actualParam});
            break;
        case PortType::POUTPUT:
            doAsgn(PrologExporter::ContAsgn,
                   {parentExporter, actualParam}, 
                   {this, formalParam});
            break;
        default: {
            cerr << p->name << " is not 'input' or 'output'" << endl;
            exit(1);
        }
        }
    }
}