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
#include "IRExporter.h"
#include "IRExprVisitor.h"
#include "IRStmtVisitor.h"

#define UNW_LOCAL_ONLY
#include <cxxabi.h>
#include <libunwind.h>
#include <cstdio>

using namespace std;

extern std::map<perm_string, Module *> pform_modules;

std::unordered_map<std::string, const IRModule*> IRExporter::irModules;

static IRVariableType getVariableType(PWire *w)
{
    switch (w->get_wire_type())
    {
    case NetNet::WIRE:
    case NetNet::IMPLICIT:
        return IRVariableType::WIRE;
    case NetNet::REG:
    case NetNet::IMPLICIT_REG:
        return IRVariableType::REGISTER;
    default:
        cerr << "Not supported net:" << endl;
        w->dump(cerr, 0);
        exit(1);
    }
}

const IRModule *IRExporter::extractModule() const
{
    string module_name(module->mod_name().str());
    if (moduleExists(module_name))
    {
        backtrace();
        cerr << module_name << " is already extracted, why bother?" << endl;
        exit(1);
    }
    setModule(module_name, NULL);

    IRModule *irModule = new IRModule;
    setModulePorts(irModule);

    // add registers and wires to the module
    for (map<perm_string, PWire *>::const_iterator wire = module->wires.begin(); wire != module->wires.end(); ++wire)
    {
        PWire *w = (*wire).second;
        IRVariable v(getVariableType(w), getPermString(w->basename()));
        irModule->addVariable(v);
    }

    // PGAssign, PGBuiltin, and PGModule
    for (auto gateItr = module->get_gates().begin(); gateItr != module->get_gates().end(); ++gateItr)
    {
        PGate *pg = *gateItr;
        const IRStmt *stmt;
        if (PGAssign *assignStmt = dynamic_cast<PGAssign *>(pg))
        {
            stmt = toIRStmt(assignStmt);
        }
        else if (PGBuiltin *builtinStmt = dynamic_cast<PGBuiltin *>(pg))
        {
            stmt = toIRStmt(builtinStmt);
        }
        else if (PGModule *moduleStmt = dynamic_cast<PGModule *>(pg))
        {
            stmt = toIRStmt(moduleStmt);
        }
        else
        {
            cerr << "Unknown PGate:" << endl;
            pg->dump(cerr, 0);
            exit(1);
        }

        irModule->addGateStatement(stmt);
    }

    // always blocks
    vector<const PEventStatement *> alwaysBlocks;
    for (list<PProcess *>::const_iterator idx = module->behaviors.begin(); idx != module->behaviors.end(); ++idx)
    {
        const PProcess *process = *idx;
        switch (process->type())
        {
        case IVL_PR_INITIAL:
            // skipping initial block
            break;
        case IVL_PR_FINAL:
            cerr << "final blocks are not supported:" << endl;
            process->dump(cerr, 0);
            exit(1);
        case IVL_PR_ALWAYS:
            const PEventStatement *eventStmt = dynamic_cast<PEventStatement *>(process->statement_);
            const IREvent *irEvent;

            if (eventStmt->expr_.count() == 0)
            {
                irEvent = new IREvent(IREventType::STAR, NULL);
            }
            else if (eventStmt->expr_.count() == 1)
            {
                if (eventStmt->expr_[0] == NULL)
                {
                    cerr << "PEventStatement: event expression is wait_fork" << endl;
                    eventStmt->dump(cerr, 0);
                    exit(1);
                }
                irEvent = toIREvent(eventStmt->expr_[0]);
            }
            else
            {
                cerr << "PEventStatement: multiple event expression" << endl;
                eventStmt->dump(cerr, 0);
                exit(1);
            }

            Statement *statement = eventStmt->statement_;
            IRAlwaysBlock *ab = new IRAlwaysBlock(irEvent, toIRStmt(statement));
            irModule->addAlwaysBlock(ab);
            break;
        }
    }

    // ###########################################################################
    // missing functionality
    // ###########################################################################
    // skipping variable initializations
    if (module->var_inits.size() != 0)
    {
        cerr << "skipping variable initializations" << endl;
        for (unsigned i = 0; i < module->var_inits.size(); i++)
        {
            Statement *s = module->var_inits[i];
            PAssign *ba = dynamic_cast<PAssign *>(s);
            assert(ba != NULL);

            PExpr *rexpr = ba->rval();
            if (rexpr == NULL)
            {
                // don't know what to do, so continue
                continue;
            }

            if (!isConstantExpr(rexpr))
            {
                cerr << endl;
                s->dump(cerr, 0);
                cerr << endl
                     << "initial assignment is not a constant blocking assignment" << endl;
                exit(1);
            }
        }
    }

    // attributes
    if (module->attributes.begin() != module->attributes.end())
    {
        cerr << "NOT SUPPORTED: attributes" << endl;
        exit(1);
    }

    // typedefs
    if (module->typedefs.begin() != module->typedefs.end())
    {
        cerr << "NOT SUPPORTED: typedefs" << endl;
        exit(1);
    }

    // specparams
    if (module->specparams.begin() != module->specparams.end())
    {
        cerr << "NOT SUPPORTED: specparams" << endl;
        exit(1);
    }

    // enumerations
    if (module->enum_sets.begin() != module->enum_sets.end())
    {
        cerr << "NOT SUPPORTED: enumerations" << endl;
        exit(1);
    }

    // classes
    if (module->classes.begin() != module->classes.end())
    {
        cerr << "NOT SUPPORTED: classes" << endl;
        exit(1);
    }

    // nested modules
    if (module->nested_modules.begin() != module->nested_modules.end())
    {
        cerr << "NOT SUPPORTED: nested modules" << endl;
        exit(1);
    }

    // generate variables
    if (module->genvars.begin() != module->genvars.end())
    {
        cerr << "NOT SUPPORTED: genvars" << endl;
        exit(1);
    }

    // generate schemes
    if (module->generate_schemes.begin() != module->generate_schemes.end())
    {
        cerr << "NOT SUPPORTED: generate schemes" << endl;
        exit(1);
    }

    // defparms
    if (module->defparms.begin() != module->defparms.end())
    {
        cerr << "NOT SUPPORTED: defparms" << endl;
        exit(1);
    }

    // events
    if (module->events.begin() != module->events.end())
    {
        cerr << "NOT SUPPORTED: events" << endl;
        exit(1);
    }

    // tasks
    if (module->tasks.begin() != module->tasks.end())
    {
        cerr << "NOT SUPPORTED: tasks" << endl;
        exit(1);
    }

    // analog behaviors
    if (module->analog_behaviors.begin() != module->analog_behaviors.end())
    {
        cerr << "NOT SUPPORTED: analog behaviors" << endl;
        exit(1);
    }

    // specify paths
    if (module->specify_paths.begin() != module->specify_paths.end())
    {
        cerr << "NOT SUPPORTED: specify paths" << endl;
        exit(1);
    }

    setModule(module_name, irModule);
    return irModule;
}

void IRExporter::setModulePorts(IRModule *irModule) const
{
    irModule->setModuleName(this->module->mod_name().str());

    // print formal parameters
    if (module->port_count() > 0)
    {
        for (unsigned i = 0; i < module->port_count(); i++)
        {
            Module::port_t *p = module->ports[i];
            if (p == NULL)
            {
                cerr << "unconnected port in " << module->mod_name() << endl;
                exit(1);
            }

            string portname = getPermString(p->name);
            auto firstExpr = dynamic_cast<const IRExpr_Variable*>(pform_nameToIRExpr(p->expr[0]->path()));

            if (p->expr.size() != 1 || firstExpr == NULL || portname.compare(firstExpr->getOnlyVariableName()) != 0)
            {
                cerr << "port in " << module->mod_name() << " has weird internal connections" << endl;
                cerr << "    ." << p->name << "(" << *p->expr[0];
                for (unsigned wdx = 1; wdx < p->expr.size(); wdx += 1)
                {
                    cerr << ", " << *p->expr[wdx];
                }
                cerr << ")";
                exit(1);
            }

            auto wireItr = module->wires.find(p->name);
            PWire *wire = NULL;

            if (wireItr == module->wires.end())
            {
                cerr << portname << " not found in module->wires" << endl;
                exit(1);
            }
            wire = wireItr->second;
            assert(wire != NULL);

            IRPortType portType;

            switch (wire->get_port_type())
            {
            case PortType::PINPUT:
                portType = IRPortType::INPUT;
                break;
            case PortType::POUTPUT:
                portType = IRPortType::OUTPUT;
                break;
            default:
            {
                cerr << portname << " is not 'input' or 'output'" << endl;
                wire->dump(cerr, 0);
                exit(1);
            }
            }

            IRPort irPort(portType, getVariableType(wire), portname);
            irModule->addPort(irPort);
        }
    }
}

const IRExpr *IRExporter::toIRExpr(PExpr *expr) const
{
    IRExprVisitor v(this);
    expr->accept(&v);
    return v.getIRExpr();
}

const IRStmt *IRExporter::toIRStmt(PGate *pgate) const
{
    IRStmtVisitor v(this);
    pgate->accept(&v);
    return v.getIRStmt();
}

const IRStmt *IRExporter::toIRStmt(Statement *stmt) const
{
    IRStmtVisitor v(this);
    stmt->accept(&v);
    return v.getIRStmt();
}

bool IRExporter::isConstantExpr(PExpr *expr) const
{
    const IRExpr *irExpr = toIRExpr(expr);
    bool result = irExpr->isConstant();
    delete(irExpr);
    return result;
}

std::ostream &operator<<(std::ostream &out, const IRExporter &)
{
    assert(!IRExporter::irModules.empty());

    for (auto itr = IRExporter::irModules.begin(); itr != IRExporter::irModules.end(); itr++)
    {
        out << *(itr->second) << endl;
    }

    return out;
}

// -----------------------------------------------------------------------------
// HELPER METHODS
// -----------------------------------------------------------------------------

extern std::map<perm_string, Module *> pform_modules;

bool IRExporter::isToplevel() const
{
    return moduleInstantiation == NULL;
}

const IRExpr *IRExporter::nameComponentToIRExpr(const perm_string &name,
                                                const std::list<index_component_t> &indices) const
{
    string nameStr(name.str());
    bool varExists = false;
    PExpr *paramExp = NULL;

    // check if variable is a parameter
    if (auto paramItr = module->parameters.find(name); paramItr != module->parameters.end())
    {
        varExists = true;
        paramExp = paramItr->second.expr;
    }
    else if (auto localParamItr = module->localparams.find(name); localParamItr != module->localparams.end())
    {
        varExists = true;
        paramExp = localParamItr->second.expr;
    }

    if (paramExp)
    {
        if (moduleInstantiation == NULL)
        {
            return toIRExpr(paramExp);
        }

        if (moduleInstantiation->overrides_ && (!moduleInstantiation->overrides_->empty()))
        {
            auto orItr = moduleInstantiation->overrides_->begin();
            auto allParamsItr = module->param_names.begin();

            while (allParamsItr != module->param_names.end())
            {
                perm_string paramName = *allParamsItr;
                PExpr *overriddenExp = *orItr;

                if (strcmp(paramName.str(), name.str()) == 0)
                {
                    if (overriddenExp == NULL)
                    {
                        break;
                    }
                    return toIRExpr(overriddenExp);
                }

                ++orItr;
                ++allParamsItr;
            }
        }

        if (moduleInstantiation->parms_)
        {
            assert(moduleInstantiation->overrides_ == NULL);

            for (unsigned i = 0; i < moduleInstantiation->nparms_; i++)
            {
                named<PExpr *> &n = moduleInstantiation->parms_[i];

                if (strcmp(n.name.str(), name.str()) == 0)
                {
                    return toIRExpr(n.parm);
                }
            }
        }

        return toIRExpr(paramExp);
    }

    if (!varExists)
    {
        varExists =
            (module->wires.find(name) != module->wires.end()) ||
            (module->funcs.find(name) != module->funcs.end());
    }

    if (!varExists)
    {
        cerr << endl
             << "cannot find variable '" << nameStr << "' in module " << module->mod_name() << endl;
        exit(1);
    }

    IRExpr_Variable* v = new IRExpr_Variable(nameStr, module);
    if (!indices.empty())
    {
        IRExpr_Select *selectExpr = new IRExpr_Select(v);

        for (auto idx = indices.begin(); idx != indices.end(); ++idx)
        {
            const index_component_t &ic = (*idx);
            switch (ic.sel)
            {
            case index_component_t::SEL_BIT:
                selectExpr->addIndex(toIRExpr(ic.msb));
                break;
            case index_component_t::SEL_PART:
            case index_component_t::SEL_IDX_UP:
            case index_component_t::SEL_IDX_DO:
                selectExpr->addIndex(toIRExpr(ic.msb));
                selectExpr->addIndex(toIRExpr(ic.lsb));
                break;
            case index_component_t::SEL_BIT_LAST:
                selectExpr->addIndex(new IRExpr_Constant("$"));
                break;
            default:
                cerr << "unknown bit: " << ic.sel << endl;
                exit(1);
                break;
            }
        }

        return selectExpr;
    }
    else
    {
        return v;
    }
}

const IRExpr *IRExporter::pform_nameToIRExpr(const pform_name_t &that) const
{
    pform_name_t::const_iterator cur;

    cur = that.begin();
    const name_component_t &n = *cur;
    const IRExpr *result = nameComponentToIRExpr(n.name, n.index);

    ++cur;

    if (cur != that.end())
    {
        cerr << endl
             << "NOT SUPPORTED: multiple name components: " << that << endl;
        exit(1);
    }

    return result;
}

const string IRExporter::getPermString(const perm_string& ps) const
{
    const IRExpr *e = nameComponentToIRExpr(ps, std::list<index_component_t>());
    if (auto v = dynamic_cast<const IRExpr_Variable *>(e))
    {
        string result(v->getOnlyVariableName());
        delete(e);
        return result;
    }
    else
    {
        backtrace();
        cerr << "Given perm string did not return a variable:" << *e << endl;
        exit(1);
    }
}

const IREvent *IRExporter::toIREvent(PEEvent *ev) const
{
    IREventType eventType;
    switch (ev->type())
    {
    case PEEvent::POSEDGE:
        eventType = IREventType::POSEDGE;
        break;
    case PEEvent::NEGEDGE:
        eventType = IREventType::NEGEDGE;
        break;
    default:
        cerr << endl
             << "PEvent: NOT SUPPORTED: ";
        ev->dump(cerr);
        cerr << endl;
        exit(1);
    }

    IRExprVisitor v(this);
    ev->expr()->accept(&v);

    return new IREvent(eventType, v.getIRExpr());
}

bool IRExporter::moduleExists(const std::string &moduleName)
{
    return IRExporter::irModules.find(moduleName) != irModules.end();
}

void IRExporter::setModule(const std::string &moduleName, const IRModule *irModule)
{
    IRExporter::irModules.insert_or_assign(moduleName, irModule);
}
