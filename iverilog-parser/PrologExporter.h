#ifndef PROLOG_EXPORTER_H
#define PROLOG_EXPORTER_H

#include <iostream>
#include <vector>
#include <sstream>
#include <cassert>
#include <unordered_set>
#include <unordered_map>

#include "Visitor.h"
#include "ExprVisitor.h"

class PrologExporter;

using namespace std;

class FoldNames;
class FoldNamesExtended;
class UninterpretedFunction;

class PrologExporter : public Visitor
{
public:
    PrologExporter(ostream &o, Module* m) :
        out(o), module(m), moduleInstantiation(NULL),
        parentExporter(NULL)
    { }

    PrologExporter(PrologExporter* pe, Module* m, PGModule* mi) :
        out(pe->out), parentModules(pe->parentModules),
        module(m), moduleInstantiation(mi),
        parentExporter(pe)
    {
        ModulePair mp;
        mp.module              = pe->module;
        mp.moduleInstantiation = pe->moduleInstantiation;
        this->parentModules.push_back(mp);
    }

    void visit(Module*)          ;

    void visit(PWire*)           ;
    void visit(PGate*)           ;
    void visit(PGAssign*)        ;
    void visit(PGBuiltin*)       ;
    void visit(PGModule*)        ;

    void visit(Statement*)       ;
    void visit(PProcess*)        ;
    void visit(PEventStatement*) ;
    void visit(PCondit*)         ;
    void visit(PAssign*)         ;
    void visit(PAssignNB*)       ;
    void visit(PBlock*)          ;
    void visit(PCase*)           ;
    void visit(PCallTask*)       ;
    void visit(PForStatement*)   ;

    void visit(PExpr*)           ;
    void visit(PEIdent*)         ;
    void visit(PETernary*)       ;
    void visit(PEConcat*)        ;
    void visit(PECallFunction*)  ;
    void visit(PENumber*)        ;
    void visit(PEEvent*)         ;
    void visit(PEBinary*)        ;
    void visit(PEUnary*)         ;
    void visit(PEString*)        ;

    void visit(data_type_t*)     ;
    void visit(PDelays*)         ;

    typedef struct ModulePair {
        Module*    module;
        PGModule*  moduleInstantiation;
    } ModulePair;

// private:
    unsigned ind = 0;
    vector<UninterpretedFunction> ufs;

    ostream& out;

    // first pair's inst is always NULL
    vector<ModulePair> parentModules;

    bool isToplevel() const {
        return parentModules.empty();
    }

    Module*   module;
    PGModule* moduleInstantiation;

    unordered_map<string,PExpr*> portMapping;

    string export_pform_name_to_prolog(const pform_name_t&that);
    string export_name_component_to_prolog(const perm_string&, const std::list<index_component_t>&);
    void export_pins_to_prolog(PGate* g);

    string exportExpr(PExpr*) ;

    void printModuleName(ostream&);
    void printModuleHeader(ostream&);

    static unsigned long idCounter;

    enum AssignType {ContAsgn, BlockingAsgn, NonBlockingAsgn};

private:

    void printModInstPortAssignments();
    PrologExporter* parentExporter;
    typedef struct {PrologExporter* pe; PExpr* e;} AsgnArg;
    void doAsgn(AssignType, AsgnArg, AsgnArg);

    void doAsgn(AssignType a, PExpr* lhs, PExpr* rhs) {
        return doAsgn(a, {this, lhs}, {this, rhs});
    }
};

extern const string id_prefix;

class FoldNames : public ExprVisitor
{
public:
    FoldNames(PrologExporter* _pe) : pe(_pe) {}

    void visit(PEIdent* id) {
        auto itr = id->path().begin();

        string s = pe->export_name_component_to_prolog((*itr).name, (*itr).index);

        ++itr;

        if (itr != id->path_.end()) {
            cerr << endl << "NOT SUPPORTED: " << typeid(*this).name() << " @ ";
            cerr << "multiple name components: " << id->path_ << endl;
            exit(1);
        }

        names.insert(s);
    }

    void visit(PETernary* ter) {
        ter->expr_->accept(this);
        ter->tru_->accept(this);
        ter->fal_->accept(this);
    }

    void visit(PEConcat* cat) {
        for(auto itr = cat->parms_.begin(); itr != cat->parms_.end(); ++itr) {
            if (*itr) {
                (*itr)->accept(this);
            }
        }
    }

    void visit(PECallFunction* cf) {
        for(auto itr = cf->parms_.begin(); itr != cf->parms_.end(); ++itr) {
            if (*itr) {
                (*itr)->accept(this);
            }
        }
    }

    void visit(PENumber*) {}

    void visit(PEEvent*) {}

    void visit(PEString*) {}

    void visit(PEBinary* be) {
        be->left_->accept(this);
        be->right_->accept(this);
    }

    void visit(PEUnary* ue) {
        ue->expr_->accept(this);
    }

    void print_names(ostream& out) {
        if (! names.empty()) {
            auto itr = names.begin();
            out << (*itr);
            ++itr;
            for (; itr != names.end(); ++itr) {
                out << ", " << (*itr);
            }
        }
    }

    PrologExporter* pe;
    unordered_set<string> names;
};

class FoldNamesExtended : public FoldNames {
public:
    FoldNamesExtended(PrologExporter* _pe) : FoldNames(_pe) { }

    void visit(PForStatement* o) {
        if (o->statement_) {
            o->statement_->accept(this);
        }
    }

    void visit(PCondit*o) {
        o->expr_->accept(this);
        if (o->if_) {
            o->if_->accept(this);
        }
        if (o->else_) {
            o->else_->accept(this);
        }
    }

    void visit(PAssign* ba) {
        if (ba->lval_) {
            ba->lval_->accept(this);
        }
        if (ba->rval_) {
            ba->rval_->accept(this);
        }
    }

    void visit(PAssignNB* nba){
        if (nba->lval_) {
            nba->lval_->accept(this);
        }
        if (nba->rval_) {
            nba->rval_->accept(this);
        }
    }

    void visit(PBlock* b) {
        for (unsigned idx = 0; idx < b->list_.size(); idx += 1) {
            if (b->list_[idx]) {
                b->list_[idx]->accept(this);
            }
        }
    }
};

class UninterpretedFunction {
public:
    UninterpretedFunction(string& i) {
        this->output = get_unique_name(i);
        this->inputs.push_back(i);
    }

    UninterpretedFunction(string& i1, string& i2) {
        this->output = get_unique_name(i1);
        this->inputs.push_back(i1);
        this->inputs.push_back(i2);
    }

    UninterpretedFunction(const string& functionName, unordered_set<string>::const_iterator begin, unordered_set<string>::const_iterator end) {
        if (begin == end) {
            this->output = get_unique_name("const_expr");
            return;
        }

        this->output = get_unique_name(functionName);

        while (begin != end) {
            this->inputs.push_back(*begin);
            ++begin;
        }

    }

    UninterpretedFunction(unordered_set<string>::const_iterator begin, unordered_set<string>::const_iterator end) :
        UninterpretedFunction(*begin, begin, end) { }

    UninterpretedFunction(PrologExporter* pe, PExpr* expr) {
        assert(expr != NULL);

        if (PECallFunction *cf = dynamic_cast<PECallFunction *>(expr); cf != NULL) {
            this->isVerilogFunction = true;
            this->verilogFunction   = pe->export_pform_name_to_prolog(cf->path_);
            this->output            = get_unique_name(this->verilogFunction);

            for(auto parmE : cf->parms_) {
                parmE->dump(cerr); cerr<<endl;
                this->inputs.push_back(pe->exportExpr(parmE));
            }
        } else {
            FoldNames fn(pe);
            expr->accept(&fn);
            this->output = get_unique_name("");

            auto begin = fn.names.begin();
            auto end = fn.names.end();

            if (begin != end) {
                while (begin != end) {
                    this->inputs.push_back(*begin);
                    ++begin;
                }
            } else {
                this->output = get_unique_name("const_expr");
            }
        }

    }

    void export_to_prolog(ostream& out) {
        if (isVerilogFunction)
            out << "linkF(" << output << ", " << verilogFunction << ", [";
        else
            out << "link(" << output << ", [";

        if (inputs.size() > 0) {
            out << inputs[0];

            for (unsigned i = 1; i < inputs.size(); ++i) {
                out << ", " << inputs[i];
            }
        }

        out << "])";
    }

    string getOutput() {
        return this->output;
    }

private:
    string output;
    vector<string> inputs;

    bool isVerilogFunction = false;
    string verilogFunction = "";

    static unsigned long name_counter;

    string get_unique_name(const string& s) {
        ostringstream oss;
        oss << "uf_" << name_counter++;
        if (s.size() > 0)
            oss << "_" << s;
        return oss.str();
    }
};


#endif // PROLOG_EXPORTER_H
