#include <sstream>

#include "IRExporter.h"
#include "IRStmtVisitor.h"
#include "IRExprVisitor.h"

using namespace std;

void IRStmtVisitor::visit(PGAssign *ga)
{
    if (ga->delay_count() != 0) {
        cerr << "continuous assignment has a delay:" << endl;
        ga->dump(cerr,0);
        cerr << endl;
    }

    if (ga->pin_count() != 2 || ga->pin(0) == NULL || ga->pin(1) == NULL) {
        cerr << "NOT SUPPORTED: PrologExporter@PGAssign: sth wrong with pins" << endl;
        exit(1);
    }

    irStmt = doAssignment(IR_CONTINUOUS_ASSIGNMENT, ga->pin(0), ga->pin(1));
}

void IRStmtVisitor::visit(PGBuiltin *gb)
{
    unsigned inputCnt = 0;
    const char* fun;
    switch (gb->type()) {
    case PGBuiltin::AND:  fun = "and";  inputCnt = 2; break;
    case PGBuiltin::NAND: fun = "nand"; inputCnt = 2; break;
    case PGBuiltin::OR:   fun = "or";   inputCnt = 2; break;
    case PGBuiltin::NOR:  fun = "nor";  inputCnt = 2; break;
    case PGBuiltin::XOR:  fun = "xor";  inputCnt = 2; break;
    case PGBuiltin::XNOR: fun = "xnor"; inputCnt = 2; break;
    case PGBuiltin::NOT:  fun = "not";  inputCnt = 1; break;
    default:
        cerr << "NOT SUPPORTED: builtin gate type: " << gb->type() << endl;
        exit(1);
    }

    if (inputCnt + 1 != gb->pin_count()) {
        cerr << "builtin gate ";
        gb->dump(cerr, 0);
        cerr << " has wrong number of inputs" << endl;
        exit(1);
    }

    PExpr* lhs = gb->pin(0);
    IRExpr_UF* uf = new IRExpr_UF(fun);

    for (unsigned i = 1; i < inputCnt + 1; i++)
    {
        uf->addOperand(toIRExpr(gb->pin(i)));
    }

    irStmt = doAssignment(IR_CONTINUOUS_ASSIGNMENT, lhs, uf);
}

extern std::map<perm_string,Module*> pform_modules;

void IRStmtVisitor::visit(PGModule *gm)
{
    Module* mod;

    auto mod_itr = pform_modules.find(gm->get_type());
    if (mod_itr != pform_modules.end())
    {
        mod = (*mod_itr).second;
    }
    else
    {
        cerr << "module " << gm->get_type() << " not found !";
        exit(1);
    }

    const string module_name(gm->get_type().str());
    const string instance_name(gm->get_name().str());

    IRStmt_ModuleInstance* mi = new IRStmt_ModuleInstance(module_name, instance_name);

    if (gm->pins_) {
        for (unsigned i=0; i < gm->npins_; i++) {
            const string name(gm->pins_[i].name.str());
            PExpr* expr = gm->pins_[i].parm;

            if(expr == NULL) {
                cerr << "module instance " << instance_name
                     << " of type " << module_name
                     << " has a null pin for " << name
                     << endl;
                gm->dump(cerr, 0);
                exit(1);
            }

            mi->setPort(IRExpr_Variable(name, mod), toIRExpr(expr));
        }
    } else {
        PGate *g = (PGate *)gm;
        for (unsigned i = 0; i < g->pin_count(); i += 1) {
            const string name(mod->ports[i]->name.str());
            PExpr* expr = g->pin(i);
            if(expr == NULL) {
                cerr << "module instance " << module_name
                     << " of type " << module_name
                     << " has a null pin for " << name
                     << endl;
                gm->dump(cerr, 0);
                exit(1);
            }

            mi->setPort(IRExpr_Variable(name, mod), toIRExpr(expr));
        }
    }

    if (!IRExporter::moduleExists(module_name))
    {
        IRExporter submoduleExporter(mod, gm);
        submoduleExporter.extractModule();
    }

    irStmt = mi;
}

void IRStmtVisitor::visit(PCondit *c)
{
    irStmt = new IRStmt_If(toIRExpr(c->expr_),
                           toIRStmt(c->if_),
                           toIRStmt(c->else_));
}

void IRStmtVisitor::visit(PAssign *ba)
{
    if (ba->delay_ || ba->count_ || ba->event_) {
        cerr << "Blocking assignment has a delay, repeat or event:" << endl;
        ba->dump(cerr, 0);
        exit(1);
    }

    irStmt = doAssignment(IR_BLOCKING_ASSIGNMENT, ba->lval_, ba->rval_);
}

void IRStmtVisitor::visit(PAssignNB *nba)
{
    if (nba->count_ || nba->event_) {
        cerr << "Non-blocking assignment has a delay, repeat or event:" << endl;
        nba->dump(cerr, 0);
        exit(1);
    }

    if (nba->delay_) {
        cerr << endl;
        nba->dump(cerr, 0);
        cerr << endl;
    }

    irStmt = doAssignment(IR_NON_BLOCKING_ASSIGNMENT, nba->lval_, nba->rval_);
}

void IRStmtVisitor::visit(PBlock *b)
{
    if (b->pscope_name() != 0)
    {
        cerr << "NOT SUPPORTED: PBLock w/ pscope_name non-NULL" << endl;
        exit(1);
    }

    if (b->list_.size() == 0)
    {
        irStmt = new IRStmt_Skip();
    }

    IRStmt_Sequence *irSeq = new IRStmt_Sequence();

    for (unsigned idx = 0; idx < b->list_.size(); idx += 1)
    {
        Statement *s = b->list_[idx];
        if (s)
        {
            irSeq->addStmt(toIRStmt(s));
        }
        else
        {
            irSeq->addStmt(new IRStmt_Skip());
        }
    }

    irStmt = irSeq;
}

void IRStmtVisitor::visit(PCase *c)
{
    struct CaseStruct
    {
        const IRExpr *const caseExpr;
        const IRStmt *const caseStmt;
    };

    vector<CaseStruct> items;
    bool hasDefault = false;
    Statement *defaultStmt = NULL;
    auto switchExpr = toIRExpr(c->expr_);

    for (unsigned idx = 0; idx < c->items_->count(); idx += 1)
    {
        PCase::Item *cur = (*c->items_)[idx];
        if (cur == NULL || cur->stat == NULL)
        {
            continue;
        }

        if (cur->expr.size() == 0 && cur->stat)
        {
            hasDefault = true;
            defaultStmt = cur->stat;
        }
        else
        {
            IRExpr_UF *uf = new IRExpr_UF("switch-eq", switchExpr);
            for (auto idx_expr : cur->expr)
            {
                uf->addOperand(toIRExpr(idx_expr));
            }

            CaseStruct cs = {uf, toIRStmt(cur->stat)};
            items.push_back(cs);
        }
    }

    irStmt = (hasDefault) ? toIRStmt(defaultStmt) : new IRStmt_Skip();
    for (int i = items.size() - 1; i >= 0; i--)
    {
        CaseStruct cs = items.at(i);
        irStmt = new IRStmt_If(cs.caseExpr, cs.caseStmt, irStmt);
    }
}

void IRStmtVisitor::visit(PCallTask *ct)
{
    ostringstream os;
    os << ct->path();
    string taskname = os.str();

    if (taskname == "$readmemh" || taskname == "$display" || taskname == "$finish")
    {
        irStmt = new IRStmt_Skip();
    }
    else
    {
        cerr << endl
             << "unknown task name " << taskname << endl;
        exit(1);
    }
}

const IRStmt *IRStmtVisitor::doAssignment(IRStmt_AssignmentType assignmentType,
                                          const IRExpr *lhs,
                                          const IRExpr *rhs) const
{
    if (auto lhsVar = dynamic_cast<const IRExpr_Variable *>(lhs))
    {
        return new IRStmt_Assignment(assignmentType,
                                     lhsVar,
                                     rhs);
    }
    else if (auto lhsSelect = dynamic_cast<const IRExpr_Select *>(lhs))
    {
        IRExpr_UF *newRhs = new IRExpr_UF("write to index");
        newRhs->addOperand(lhsSelect->getVariable());
        for (auto i : lhsSelect->getIndices())
        {
            newRhs->addOperand(i);
        }
        newRhs->addOperand(rhs);
        return new IRStmt_Assignment(assignmentType,
                                     lhsSelect->getVariable(),
                                     newRhs);
    }
    else
    {
        cerr << "Lhs is not a variable or a select expression" << endl;
        cerr << lhs->toIRString() << " = " << rhs->toIRString() << endl;
        exit(1);
    }
}